#' @import shiny
#' @importFrom dplyr filter arrange desc distinct mutate select left_join pull
#' @importFrom DBI dbGetQuery dbExecute
#' @importFrom pool poolWithTransaction
#' @import shinyjs
app_server <- function(input, output, session, pool) {

  # ---- 1. Authentication ----
  res_auth <- shinymanager::secure_server(
    check_credentials = check_credentials_db(pool)
  )

  # ---- 2. Load Metadata (Species List) ----
  species_list <- reactive({
    # Wir laden jetzt ALLE Namensspalten
    # Annahme: Deine Tabelle hat 'species_long_en' und 'scientific_name'
    DBI::dbGetQuery(pool, "SELECT species_short, species_long_de, species_long_en, scientific_name FROM public.lut_species_code ORDER BY species_short")
  })

  # Beobachtet Dropdown UND Liste
  observe({
    req(species_list(), input$species_lang)

    df <- species_list()

    # Sprache wählen basierend auf UI Input
    # Wir bauen das Label dynamisch zusammen: "CODE - Name"
    labels <- switch(input$species_lang,
                     "de"  = paste0(df$species_short, " - ", df$species_long_de),
                     "en"  = paste0(df$species_short, " - ", df$species_long_en),
                     "sci" = paste0(df$species_short, " - ", df$scientific_name)
    )

    # Update des MultiInputs
    choices <- setNames(df$species_short, labels)


    current_selected <- input$inSelect

    shinyWidgets::updateMultiInput(session, "inSelect", choices = choices, selected = current_selected)
  })


  # ---- 3. Project Selection Logic ----

  # Which projects is the user allowed to see?
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
    if(nrow(user_projects()) == 0) return(div("No projects assigned.", style="color:red;"))

    selectInput(
      "selected_project",
      "Select Project:",
      choices = setNames(user_projects()$project_id, user_projects()$project_name_short)
    )
  })

  # ---- 4. Load Data from DB ----

  # Load ALL relevant data for the selected project
  # Links: Results -> Audio -> Deployment -> Project
  # UPDATED: Now fetches the required_annotation_type_id
  project_data <- reactive({
    req(input$selected_project)

    query <- "
      SELECT
        r.result_id,
        r.confidence as score,
        r.begin_time_s as start,
        r.end_time_s as end_sec,
        af.relative_path as path,
        af.audio_file_id,
        af.required_annotation_type_id,   -- The fixed mode required for this file
        lt.annotation_type_description,   -- Description of the mode
        sp.species_short as prediction,
        sp.species_id as prediction_species_id,
        d.deployment_name
      FROM import.results r
      JOIN import.audio_files af ON r.audio_file_id = af.audio_file_id
      JOIN import.deployments d ON af.deployment_id = d.deployment_id
      JOIN public.lut_species_code sp ON r.species_id = sp.species_id
      LEFT JOIN public.lut_annotation_type_code lt ON af.required_annotation_type_id = lt.annotation_type_id
      WHERE d.project_id = $1
      ORDER BY af.relative_path, r.begin_time_s
    "
    # Note: Consider pagination or LIMIT for very large projects
    DBI::dbGetQuery(pool, query, params = list(input$selected_project))
  })

  # Update Sequence Dropdown when data changes
  observeEvent(project_data(), {
    req(project_data())
    files <- unique(project_data()$path)
    updateSelectizeInput(session, "seq", choices = files, server = TRUE)
  })


  # ---- 5. Current File Logic ----

  # Filter loaded data for the current file
  current_file_df <- reactive({
    req(input$seq, project_data())
    project_data() |>
      dplyr::filter(path == input$seq) |>
      dplyr::arrange(desc(score))
  })

  # Video Player Update
  observeEvent(input$seq, {
    req(input$seq)

    # Path construction:
    # Assumes audio files are in a folder mapped via add_resource_path
    full_url <- paste0("spectograms/", input$seq)

    video::changeVideo("video", full_url)

    # Seek to first prediction
    first_pred <- current_file_df()$start[1]
    if(!is.na(first_pred)) video::seekVideo("video", seek = first_pred)

    video::playVideo("video")
  })

  # Auto-Select Species in MultiInput based on Predictions
  observeEvent(current_file_df(), {
    df <- current_file_df()
    if(nrow(df) > 0) {
      # Suggest all species found by BirdNET
      preds <- unique(df$prediction)
      shinyWidgets::updateMultiInput(session, "inSelect", selected = preds)
    }
  })

  # ---- 6. UI Locking & Status Check (Read-Only vs Update) ----

  observe({
    req(input$seq, project_data(), res_auth$user_id)

    # 1. Get File ID
    aid <- project_data()$audio_file_id[project_data()$path == input$seq][1]

    # 2. Check: Who set the status?
    # We fetch the user_id of the existing entry (if any) from annotation_status (the master record)
    existing_status <- DBI::dbGetQuery(pool,
                                       "SELECT user_id FROM import.annotation_status WHERE audio_file_id = $1 LIMIT 1",
                                       params = list(aid))

    # 3. Logic Branching
    if(nrow(existing_status) > 0) {

      # SOMEONE has processed this file. Who was it?
      owner_id <- existing_status$user_id

      if(owner_id == res_auth$user_id) {
        # CASE A: It's me! -> ALLOW UPDATE
        output$text1 <- renderText("Note: You are updating your own annotation.")
        shinyjs::enable("add_btn")
        updateActionButton(session, "add_btn", label = "Save Changes (Update)", icon = icon("edit"))

      } else {
        # CASE B: It's a colleague! -> LOCK (Read-Only)
        output$text1 <- renderText("WARNING: File locked by another user (Read-Only)!")
        shinyjs::disable("add_btn") # Disable button
        updateActionButton(session, "add_btn", label = "Locked", icon = icon("lock"))
      }

    } else {
      # CASE C: File is fresh -> ALLOW NEW
      output$text1 <- renderText("")
      shinyjs::enable("add_btn")
      updateActionButton(session, "add_btn", label = "Save & Next", icon = icon("paper-plane"))
    }
  })


  # ---- 7. Output Tables & UI Info ----

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

  # Show the required annotation mode (Task)
  output$task_info <- renderUI({
    req(current_file_df())
    # Get the description from the joined query
    mode_name <- current_file_df()$annotation_type_description[1]
    if(is.na(mode_name)) mode_name <- "Standard (Not defined)"

    tagList(
      tags$strong("Task Mode: "),
      tags$span(mode_name, style = "color: blue; font-weight: bold;")
    )
  })

  # ---- 8. SAVE Logic (Database Transaction) ----

  observeEvent(input$add_btn, {
    req(input$seq, res_auth$user_id)

    # Get current metadata
    df <- current_file_df()
    if(nrow(df) == 0) return()

    aid <- df$audio_file_id[1]
    start_time <- min(df$start)
    end_time <- max(df$end_sec)

    # Get the FIXED annotation type required for this file
    fixed_type_id <- df$required_annotation_type_id[1]
    # Fallback if DB is empty (should be caught by import, but safety first)
    if(is.na(fixed_type_id)) fixed_type_id <- 1

    selected_species_codes <- input$inSelect

    tryCatch({
      pool::poolWithTransaction(pool, function(conn) {

        # --- STEP 1: Clean Slate (Delete old entries for THIS user) ---
        # This removes the "block" on the strict constraint for this specific user/file combo,
        # allowing the INSERTs below to succeed.
        # Note: If it's a new file, these DELETEs do nothing, which is fine.
        # Security: The WHERE clause prevents deleting colleagues' data.

        DBI::dbExecute(conn,
                       "DELETE FROM import.ground_truth_annotations WHERE audio_file_id = $1 AND user_id = $2",
                       list(aid, res_auth$user_id))

        DBI::dbExecute(conn,
                       "DELETE FROM import.annotation_status WHERE audio_file_id = $1 AND user_id = $2",
                       list(aid, res_auth$user_id))


        # --- STEP 2: Insert Ground Truth (only if species present) ---
        if (!is.null(selected_species_codes) && length(selected_species_codes) > 0) {
          for(code in selected_species_codes) {

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
        }
        # Note: If no species selected ("Negative Check"), we skip GT insertion
        # but proceed to insert status below.

        # --- STEP 3: Insert Annotation Status (MANDATORY) ---
        # This marks the file as "Done" using the required mode.

        status_q <- "
            INSERT INTO import.annotation_status
            (audio_file_id, user_id, begin_time_s, end_time_s, annotation_type_id)
            VALUES ($1, $2, $3, $4, $5)
        "
        DBI::dbExecute(conn, status_q, params = list(
          aid,
          res_auth$user_id,
          start_time,
          end_time,
          fixed_type_id
        ))

      }) # End Transaction

      showNotification("Saved successfully!", type = "message")

      # Logic: Move to Next File
      current_choices <- project_data() |> dplyr::pull(path) |> unique()
      curr_idx <- which(current_choices == input$seq)
      if(curr_idx < length(current_choices)) {
        updateSelectizeInput(session, "seq", selected = current_choices[curr_idx + 1])
      } else {
        showNotification("Last file in project reached.", type = "warning")
      }

    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # User Info Header
  output$user_info <- renderUI({
    tags$div(
      tags$h3(paste0("Hello ", res_auth$first_name, "!")),
      tags$p(paste("Role:", paste(res_auth$pg_role, collapse=", ")))
    )
  })
}
