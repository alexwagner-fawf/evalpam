#' @import shiny
#' @importFrom dplyr filter arrange desc distinct mutate select left_join pull
#' @importFrom DBI dbGetQuery dbExecute
#' @importFrom pool poolWithTransaction
app_server <- function(input, output, session, pool) {

  #pool <- set_db_pool() # Muss in global.R oder utils definiert sein

  # ---- 1. Authentication ----
  res_auth <- shinymanager::secure_server(
    check_credentials = check_credentials_db(pool)
  )

  # ---- 2. Load Metadata (Species List) ----
  # Laden wir einmalig beim Start, da sich Arten selten ändern
  species_list <- reactive({
    # Holt alle Arten für das MultiInput
    DBI::dbGetQuery(pool, "SELECT species_short, species_long_de FROM public.lut_species_code ORDER BY species_short")
  })

  observe({
    req(species_list())
    # Update des MultiInputs mit allen verfügbaren Arten
    choices <- setNames(species_list()$species_short,
                        paste0(species_list()$species_short, " - ", species_list()$species_long_de))

    shinyWidgets::updateMultiInput(session, "inSelect", choices = choices)
  })


  # ---- 3. Project Selection Logic ----

  # Welche Projekte darf der User sehen?
  user_projects <- reactive({
    req(res_auth$user_id)
    query <- "
      SELECT p.project_id, p.project_name_short
      FROM import.projects p
      JOIN public.project_users pu ON p.project_id = pu.project_id
      WHERE pu.user_id = $1
      ORDER BY p.project_name_short
    "
    DBI::dbGetQuery(pool, query, params = list(res_auth$user_id))
  })

  output$project_selector_ui <- renderUI({
    req(user_projects())
    if(nrow(user_projects()) == 0) return(div("Keine Projekte zugewiesen.", style="color:red;"))

    selectInput(
      "selected_project",
      "Projekt wählen:",
      choices = setNames(user_projects()$project_id, user_projects()$project_name_short)
    )
  })

  # ---- 4. Load Data from DB ----

  # Lädt ALLE relevanten Daten für das gewählte Projekt
  # Verknüpft: Results -> Audio -> Deployment -> Project
  project_data <- reactive({
    req(input$selected_project)

    # Query holt Predictions und Datei-Infos
    query <- "
      SELECT
        r.result_id,
        r.confidence as score,
        r.begin_time_s as start,
        r.end_time_s as end_sec,
        af.relative_path as path,
        af.audio_file_id,
        sp.species_short as prediction,
        sp.species_id as prediction_species_id,
        d.deployment_name
      FROM import.results r
      JOIN import.audio_files af ON r.audio_file_id = af.audio_file_id
      JOIN import.deployments d ON af.deployment_id = d.deployment_id
      JOIN public.lut_species_code sp ON r.species_id = sp.species_id
      WHERE d.project_id = $1
      ORDER BY af.relative_path, r.begin_time_s
    "
    # Achtung: Bei sehr großen Projekten hier LIMIT oder Pagination einbauen!
    DBI::dbGetQuery(pool, query, params = list(input$selected_project))
  })

  # Update Sequence Dropdown wenn sich Daten ändern
  observeEvent(project_data(), {
    req(project_data())
    files <- unique(project_data()$path)
    updateSelectizeInput(session, "seq", choices = files, server = TRUE)
  })


  # ---- 5. Current File Logic ----

  # Filtert die geladenen Daten auf die aktuelle Datei
  current_file_df <- reactive({
    req(input$seq, project_data())
    project_data() |>
      dplyr::filter(path == input$seq) |>
      dplyr::arrange(desc(score))
  })

  # Video Player Update
  observeEvent(input$seq, {
    req(input$seq)

    # Pfad Konstruktion:
    # Annahme: Deine Audiofiles liegen in einem Ordner, der via add_resource_path('audio', ...) freigegeben ist.
    # Wenn 'relative_path' z.B. '2024/01/file.wav' ist:
    full_url <- paste0("spectograms/", input$seq) # Oder "audio/", je nach setup

    video::changeVideo("video", full_url)

    # Springe zur ersten Prediction
    first_pred <- current_file_df()$start[1]
    if(!is.na(first_pred)) video::seekVideo("video", seek = first_pred)

    video::playVideo("video")
  })


  # Warning Text: Prüfen ob für dieses File schon eine Annotation in der DB existiert
  output$text1 <- renderText({
    req(input$seq, project_data())

    # Audio ID ermitteln
    aid <- project_data()$audio_file_id[project_data()$path == input$seq][1]

    # Check in ground_truth Tabelle
    check <- DBI::dbGetQuery(pool,
                             "SELECT count(*) as cnt FROM import.ground_truth_annotations WHERE audio_file_id = $1",
                             params = list(aid))$cnt

    if(check > 0) return("Achtung: Diese Datei wurde bereits bearbeitet!")
    return("")
  })

  # Auto-Select Species in MultiInput based on Predictions
  observeEvent(current_file_df(), {
    df <- current_file_df()
    if(nrow(df) > 0) {
      # Wir schlagen alle Arten vor, die BirdNET gefunden hat
      preds <- unique(df$prediction)
      shinyWidgets::updateMultiInput(session, "inSelect", selected = preds)
    }
  })

  # ---- 6. Output Tables & Maps ----

  output$table_bnet <- DT::renderDataTable({
    req(current_file_df())
    df <- current_file_df() |>
      dplyr::select(prediction, score, start, end_sec)

    DT::datatable(df, options = list(dom = 't', pageLength = 10)) |>
      DT::formatRound("score", 3)
  })

  output$plot_info <- renderUI({
    req(current_file_df())
    dep_name <- current_file_df()$deployment_name[1]
    tags$h5(paste("Deployment:", dep_name))
  })

  # ---- 7. SAVE Logic (Database Insert) ----

  observeEvent(input$add_btn, {
    req(input$seq, res_auth$user_id)

    # Aktuelle Metadaten holen
    df <- current_file_df()
    if(nrow(df) == 0) return()

    aid <- df$audio_file_id[1]
    start_time <- min(df$start) # Vereinfachung: Nimmt min/max der Datei oder Predictions
    end_time <- max(df$end_sec)

    selected_species_codes <- input$inSelect # z.B. c("TURMER", "PARMAJ")

    tryCatch({
      pool::poolWithTransaction(pool, function(conn) {

        # A) Falls wir alte Annotationen überschreiben wollen:
        # DBI::dbExecute(conn, "DELETE FROM import.ground_truth_annotations WHERE audio_file_id=$1 AND user_id=$2", list(aid, res_auth$user_id))

        # B) Einfügen der bestätigten Arten
        if (!is.null(selected_species_codes) && length(selected_species_codes) > 0) {
          for(code in selected_species_codes) {
            # ID holen (könnte man optimieren via lookup liste)
            sid_query <- "SELECT species_id FROM public.lut_species_code WHERE species_short = $1"
            sid <- DBI::dbGetQuery(conn, sid_query, params=list(code))$species_id

            if(length(sid) > 0) {
              insert_q <- "
                 INSERT INTO import.ground_truth_annotations
                 (audio_file_id, user_id, species_id, begin_time_s, end_time_s, is_present)
                 VALUES ($1, $2, $3, $4, $5, TRUE)
               "
              DBI::dbExecute(conn, insert_q, params = list(
                aid, res_auth$user_id, sid, start_time, end_time
              ))
            }
          }
        } else {
          # C) Keine Art ausgewählt -> Negative Record?
          # Das hängt von deiner Logik ab. Ggf. einen Eintrag mit 'negative_check' in annotation_status machen.
        }

        # Optional: Status auf 'fertig' setzen in import.annotation_status
        # ...

      })

      showNotification("Gespeichert!", type = "message")

      # Logic: Next File
      current_choices <- project_data() |> dplyr::pull(path) |> unique()
      curr_idx <- which(current_choices == input$seq)
      if(curr_idx < length(current_choices)) {
        updateSelectizeInput(session, "seq", selected = current_choices[curr_idx + 1])
      } else {
        showNotification("Letzte Datei im Projekt erreicht.", type = "warning")
      }

    }, error = function(e) {
      showNotification(paste("Fehler:", e$message), type = "error")
    })
  })

  # User Info Header
  output$user_info <- renderUI({
    tags$div(
      tags$h3(paste0("Hallo ", res_auth$first_name, "!")),
      tags$p(paste("Rolle:", paste(res_auth$pg_role, collapse=", ")))
    )
  })
}
