#' The application Server-Side Logic
#'
#' @param input Internal parameter for `{shiny}`.
#' @param output Internal parameter for `{shiny}`.
#' @param session Internal parameter for `{shiny}`.
#' @param pool The database connection pool.
#'
#' @import shiny
#' @importFrom dplyr filter arrange desc distinct mutate select left_join pull
#' @importFrom DBI dbGetQuery dbExecute
#' @importFrom pool poolWithTransaction
#' @import shinyjs
#' @import shinyWidgets
#' The application Server-Side Logic
#'
#' Änderungen/Fixes:
#' 1. Endlosschleife bei der Artenauswahl behoben (isolate).
#' 2. Spaltennamen korrigiert (prediction -> species_short).
#' 3. Robustere Checks gegen leere Daten.
app_server <- function(input, output, session, pool) {

  # ---- 1. Authentication ----
  res_auth <- shinymanager::secure_server(
    check_credentials = check_credentials_db(pool)
  )

  # ---- 2. Load Metadata (Species & Behavior) ----

  # A) Load Species List
  species_list <- reactive({
    req(input$selected_project)

    # Query lädt Whitelist basierend auf Project Settings
    query <- "
      SELECT DISTINCT sp.species_id, sp.species_short, sp.species_long_de, sp.species_long_en, sp.species_scientific
      FROM public.lut_species_code sp
      JOIN import.settings_species ss ON sp.species_id = ss.species_id
      JOIN import.settings s ON ss.settings_id = s.settings_id
      JOIN import.results r ON s.settings_id = r.settings_id
      JOIN import.audio_files af ON r.audio_file_id = af.audio_file_id
      JOIN import.deployments d ON af.deployment_id = d.deployment_id
      WHERE d.project_id = $1
      ORDER BY sp.species_short
    "
    df <- DBI::dbGetQuery(pool, query, params = list(input$selected_project))

    # Fallback: Wenn Liste leer, lade alle Arten (Sicherheitsnetz)
    if (nrow(df) == 0) {
      return(DBI::dbGetQuery(pool, "SELECT species_id, species_short, species_long_de, species_long_en, species_scientific FROM public.lut_species_code ORDER BY species_short"))
    } else {
      return(df)
    }
  })

  # B) Load Behavior List
  behavior_list <- reactive({
    DBI::dbGetQuery(pool, "SELECT behavior_id, behavior_long_de, behavior_long_en FROM public.lut_behavior_code ORDER BY behavior_id")
  })

  # ---- 3. Language & Labels Observer ----
  observe({
    req(species_list(), input$species_lang)
    df <- species_list()

    # Labels (Anzeige) bleiben wie sie sind
    labels <- switch(input$species_lang,
                     "de"  = df$species_long_de,
                     "en"  = df$species_long_en,
                     "sci" = df$species_scientific
    )
    # Fallback für leere Namen (optional, falls species_short NA ist, nimm ID)
    labels[is.na(labels) | labels == ""] <- as.character(df$species_id[is.na(labels) | labels == ""])

    # --- ÄNDERUNG HIER: ---
    # Vorher: choices <- setNames(df$species_short, labels)
    # Jetzt: Wir nutzen die ID als internen Wert!
    # Wichtig: as.character(), weil Inputs oft Strings erwarten
    choices <- setNames(as.character(df$species_id), labels)

    choices <- choices[order(names(choices))]

    current_selected <- isolate(input$inSelect)

    # Optional: Prüfen ob alte Auswahl noch gültig ist (da sich Values von Kürzel auf ID geändert haben)
    # Fürs erste lassen wir es so, beim Neuladen ist es eh leer.
    shinyWidgets::updateMultiInput(session, "inSelect", choices = choices, selected = current_selected)
  })

  # ---- 4. Project Selection ----
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
    selectInput("selected_project", "Select Project:", choices = setNames(user_projects()$project_id, user_projects()$project_name_short))
  })

  # ---- 5. Load Data from DB (Das ist dein "get_data_from_db") ----
  project_data <- reactive({
    req(input$selected_project)

    # Diese Query ist komplex, weil sie alles nötige joint (Pfade, Metadaten, Deployment)
    query <- "
      SELECT
        r.result_id,
        r.confidence as score,
        CAST(r.begin_time_ms AS FLOAT) / 1000.0 as start,
        CAST(r.end_time_ms AS FLOAT) / 1000.0 as end_sec,
        -- Pfad Konstruktion
        CAST(s.spectrogram_id AS TEXT) || '.mp4' as path,
        s.buffer_ms,
        af.audio_file_id,
        af.required_annotation_type_id,
        lt.annotation_type_description,
        sp.species_id,
        sp.species_short,
        sp.species_long_de,
        sp.species_long_en,
        sp.species_scientific,
        d.deployment_name
      FROM import.results r
      JOIN import.spectrograms s ON r.result_id = s.result_id
      JOIN import.audio_files af ON r.audio_file_id = af.audio_file_id
      JOIN import.deployments d ON af.deployment_id = d.deployment_id
      JOIN public.lut_species_code sp ON r.species_id = sp.species_id
      LEFT JOIN public.lut_annotation_type_code lt ON af.required_annotation_type_id = lt.annotation_type_id
      WHERE d.project_id = $1
      ORDER BY af.relative_path, r.begin_time_ms
    "
    DBI::dbGetQuery(pool, query, params = list(input$selected_project))
  })

  # Update File List
  observeEvent(project_data(), {
    req(project_data())
    files <- unique(project_data()$path)
    updateSelectizeInput(session, "seq", choices = files, server = TRUE)
  })

  # ---- 6. Current File Logic ----
  current_file_df <- reactive({
    req(input$seq, project_data())
    project_data() |>
      dplyr::filter(path == input$seq) |>
      dplyr::arrange(desc(score))
  })

  # Video Player Logic
  observeEvent(input$seq, {
    req(input$seq, project_data())
    row_data <- project_data() |> dplyr::filter(path == input$seq)

    if(nrow(row_data) > 0) {
      buffer_val <- row_data$buffer_ms[1]
      seek_target <- max(0, buffer_val - 2)

      # Video Pfad: Wir nehmen an, dass 'spectrograms' als Resource Path gesetzt ist
      video::changeVideo("video", paste0("spectrograms/", input$seq))

      shinyjs::delay(750, { # Delay etwas erhöht für Stabilität
        try({
          video::seekVideo("video", seek = seek_target)
          video::playVideo("video")
        }, silent = TRUE)
      })
    }
  })

  # ---- Smart Species Selection ----
  observeEvent(input$seq, {
    req(input$seq, project_data(), res_auth$user_id)

    current_file_info <- project_data() |> dplyr::filter(path == input$seq)
    if(nrow(current_file_info) == 0) return()

    aid <- current_file_info$audio_file_id[1]
    start_ms <- as.integer(current_file_info$start[1] * 1000)

    # 1. STATUS CHECK: Habe ich diesen Schnipsel schon mal angefasst?
    q_status <- "SELECT 1 FROM import.annotation_status WHERE audio_file_id = $1 AND user_id = $2 AND begin_time_ms = $3"
    has_status <- DBI::dbGetQuery(pool, q_status, params = list(aid, res_auth$user_id, start_ms))

    if(nrow(has_status) > 0) {
      # CASE A: Wiederkehrer (Bereits bearbeitet)
      # Wir laden die gespeicherten Arten (kann auch LEER sein, wenn alles abgewählt wurde!)

      q_gt <- "SELECT species_id FROM import.ground_truth_annotations WHERE audio_file_id = $1 AND user_id = $2 AND begin_time_ms = $3"
      existing_annotations <- DBI::dbGetQuery(pool, q_gt, params = list(aid, res_auth$user_id, start_ms))

      selected_species <- as.character(existing_annotations$species_id)
      # Keine Notification mehr nötig, oder text ändern zu "Status geladen"

    } else {
      # CASE B: Frischer Start (Noch nie gespeichert)
      # Wir laden die BirdNET Vorschläge als Starthilfe
      selected_species <- unique(current_file_info$species_id)
      selected_species <- as.character(selected_species[!is.na(selected_species)])
    }

    shinyWidgets::updateMultiInput(session, "inSelect", selected = selected_species)
  })

  # ---- 7. Dynamic Behavior UI ----
  output$dynamic_behavior_ui <- renderUI({
    req(behavior_list(), input$seq, res_auth$user_id)

    selected_ids <- input$inSelect # Das sind jetzt IDs (Strings)!

    if (is.null(selected_ids) || length(selected_ids) == 0) {
      return(tags$p("Wähle eine Art aus, um Verhalten zuzuweisen.", style="color: #888; font-style: italic;"))
    }

    # Listen vorbereiten
    df_sp <- species_list()
    # (Label Logik gekürzt...)

    df_beh <- behavior_list()
    beh_labels <- switch(input$species_lang, "de"=df_beh$behavior_long_de, "en"=df_beh$behavior_long_en, "sci"=df_beh$behavior_long_en)
    beh_choices <- setNames(df_beh$behavior_id, beh_labels)

    # DB Check
    aid_query <- project_data() |> dplyr::filter(path == input$seq)
    saved_behaviors <- data.frame(species_id = integer(), behavior_id = integer())

    if(nrow(aid_query) > 0) {
      current_aid <- aid_query$audio_file_id[1]
      start_ms <- as.integer(aid_query$start[1] * 1000) # << ZEIT HOLEN

      # CHANGE: Auch hier nach Zeit filtern!
      q_beh <- "
        SELECT species_id, behavior_id
        FROM import.ground_truth_annotations
        WHERE audio_file_id = $1
        AND user_id = $2
        AND begin_time_ms = $3 -- << FIX
      "
      saved_behaviors <- DBI::dbGetQuery(pool, q_beh, params = list(current_aid, res_auth$user_id, start_ms))
    }

    # UI Loop
    ui_list <- lapply(selected_ids, function(code_id) {

      # Input ID basiert auf der Species-ID (z.B. "beh_12")
      input_id <- paste0("beh_", code_id)

      # Name für Anzeige finden
      curr_sp_row <- df_sp[df_sp$species_id == as.numeric(code_id), ]
      display_name <- "Unbekannt"
      if(nrow(curr_sp_row) > 0) {
        if(input$species_lang == "de") display_name <- curr_sp_row$species_long_de
        else if(input$species_lang == "en") display_name <- curr_sp_row$species_long_en
        else display_name <- curr_sp_row$species_scientific
      }

      # Value Logic
      val_ui <- isolate(input[[input_id]])
      # CHANGE: Vergleich mit ID
      val_db <- saved_behaviors$behavior_id[saved_behaviors$species_id == as.numeric(code_id)]

      # Default: Gesang
      default_row <- df_beh[grep("Gesang", df_beh$behavior_long_de, ignore.case = TRUE), ]
      selected_val <- if(nrow(default_row) > 0) default_row$behavior_id[1] else 1

      if (!is.null(val_ui)) selected_val <- val_ui
      else if (length(val_db) > 0 && !is.na(val_db[1])) selected_val <- val_db[1]

      div(
        class = "panel panel-default", style = "margin-bottom: 5px; border-left: 4px solid #337ab7;",
        div(class = "panel-body", style = "padding: 8px;",
            fluidRow(
              column(6, tags$strong(display_name, style="line-height: 30px;")),
              column(6, selectInput(input_id, NULL, choices = beh_choices, selected = selected_val, width = "100%", selectize = FALSE))
            )
        )
      )
    })
    do.call(tagList, ui_list)
  })

  # ---- 8. Status Check (Locking) ----
  observe({
    # Wir brauchen seq, daten und user
    req(input$seq, project_data(), res_auth$user_id)

    # 1. Daten für aktuellen Schnipsel holen
    # Wir brauchen die Zeiten, um genau DIESEN Schnipsel zu prüfen
    current_row <- project_data() |>
      dplyr::filter(path == input$seq)

    if(nrow(current_row) == 0) return()

    aid <- current_row$audio_file_id[1]

    # Zeiten in Millisekunden umrechnen (wie beim Speichern)
    start_ms <- as.integer(current_row$start[1] * 1000)

    # 2. Check: Gibt es einen Status für GENAU DIESEN Start-Zeitpunkt?
    # Vorher hatten wir nur "WHERE audio_file_id = $1". Das war zu grob.
    query_status <- "
      SELECT user_id
      FROM import.annotation_status
      WHERE audio_file_id = $1
      AND begin_time_ms = $2  -- << HIER IST DER TRICK
      LIMIT 1
    "

    existing_status <- DBI::dbGetQuery(pool, query_status, params = list(aid, start_ms))

    # 3. Logik (Wer hat's gemacht?)
    if(nrow(existing_status) > 0) {
      owner_id <- existing_status$user_id

      if(owner_id == res_auth$user_id) {
        # CASE A: Du hast diesen Schnipsel schon gemacht
        output$text1 <- renderText("Info: Du hast diesen Schnipsel bereits bearbeitet (Update).")
        shinyjs::enable("add_btn")
        updateActionButton(session, "add_btn", label = "Update", icon = icon("edit"))
      } else {
        # CASE B: Ein Kollege hat diesen Schnipsel gemacht
        output$text1 <- renderText("LOCKED: Schnipsel von anderem User bearbeitet!")
        shinyjs::disable("add_btn")
        updateActionButton(session, "add_btn", label = "Locked", icon = icon("lock"))
      }
    } else {
      # CASE C: Neu / Offen
      output$text1 <- renderText("") # Kein Text = Alles gut
      shinyjs::enable("add_btn")
      updateActionButton(session, "add_btn", label = "Speichern & Weiter", icon = icon("paper-plane"))
    }
  })

  # ---- 9. Tabelle ----
  output$table_bnet <- DT::renderDataTable({
    req(current_file_df(), input$species_lang)
    df <- current_file_df()

    # Simple Label Logik
    labs <- switch(input$species_lang, "de"=df$species_long_de, "en"=df$species_long_en, "sci"=df$species_scientific)
    labs[is.na(labs)] <- df$species_short[is.na(labs)]

    df_show <- df |> dplyr::mutate(prediction = labs) |> dplyr::select(prediction, score, start, end_sec)
    DT::datatable(df_show, options = list(dom = 't', pageLength = 5)) |> DT::formatRound("score", 2)
  })



  # ---- 10. SAVE Logic (KORRIGIERT) ----
  observeEvent(input$add_btn, {
    req(input$seq, res_auth$user_id)

    # 1. Daten aus der UI holen
    df <- current_file_df()
    if(nrow(df) == 0) return()

    aid <- df$audio_file_id[1]

    # Zeit berechnen
    start_ms <- as.integer(min(df$start) * 1000)
    end_ms   <- as.integer(max(df$end_sec) * 1000)

    # Annotation Type (Fallback auf 1)
    fixed_type_id <- df$required_annotation_type_id[1]
    if(is.na(fixed_type_id)) fixed_type_id <- 1

    # ---------------------------------------------------------
    # SCHRITT A: Daten VORBEREITEN (JETZT INNERHALB DER KLAMMER!)
    # ---------------------------------------------------------
    inserts_to_do <- list()
    selected_ids <- input$inSelect # Hier ist der Zugriff erlaubt!

    if (!is.null(selected_ids) && length(selected_ids) > 0) {
      for(id_str in selected_ids) {

        # String zu ID wandeln
        sid <- as.integer(id_str)

        if(!is.na(sid)) {
          # Behavior ID holen
          input_id <- paste0("beh_", sid)
          beh_val <- input[[input_id]]

          # Behavior Wert verarbeiten
          if(is.null(beh_val) || beh_val == "" || beh_val == "NA") {
            beh_val_sql <- NA_integer_
          } else {
            beh_val_sql <- as.integer(beh_val)
          }

          inserts_to_do[[length(inserts_to_do) + 1]] <- list(
            sid = sid,
            bid = beh_val_sql
          )
        }
      }
    }

    # ---------------------------------------------------------
    # SCHRITT B: TRANSAKTION (Schreiben)
    # ---------------------------------------------------------
    tryCatch({
      pool::poolWithTransaction(pool, function(conn) {

        # 1. Aufräumen (Löschen)
        # FEHLER WAR: "DELETE ... WHERE audio_file_id = $1" -> Das löscht ALLES in der Datei!
        # FIX: "DELETE ... WHERE ... AND begin_time_ms = $3" -> Löscht nur DIESEN Schnipsel.

        del_q_gt <- "DELETE FROM import.ground_truth_annotations WHERE audio_file_id = $1 AND user_id = $2 AND begin_time_ms = $3"
        DBI::dbExecute(conn, del_q_gt, list(aid, res_auth$user_id, start_ms))

        del_q_st <- "DELETE FROM import.annotation_status WHERE audio_file_id = $1 AND user_id = $2 AND begin_time_ms = $3"
        DBI::dbExecute(conn, del_q_st, list(aid, res_auth$user_id, start_ms))

        # 2. Schreiben (Bleibt gleich, da wir start_ms schon übergeben haben)
        if (length(inserts_to_do) > 0) {
          for(item in inserts_to_do) {
            DBI::dbExecute(conn,
                           "INSERT INTO import.ground_truth_annotations (audio_file_id, user_id, species_id, behavior_id, begin_time_ms, end_time_ms, is_present) VALUES ($1, $2, $3, $4, $5, $6, TRUE)",
                           list(aid, res_auth$user_id, item$sid, item$bid, start_ms, end_ms)
            )
          }
        }

        # 3. Status schreiben (Bleibt gleich)
        DBI::dbExecute(conn,
                       "INSERT INTO import.annotation_status (audio_file_id, user_id, begin_time_ms, end_time_ms, annotation_type_id) VALUES ($1, $2, $3, $4, $5)",
                       list(aid, res_auth$user_id, start_ms, end_ms, fixed_type_id)
        )
      })

      showNotification("Gespeichert!", type = "message")

      # Next File Logik
      current_choices <- unique(project_data()$path)
      curr_idx <- which(current_choices == input$seq)

      if(length(curr_idx) > 0 && curr_idx < length(current_choices)) {
        next_file <- current_choices[curr_idx + 1]
        updateSelectizeInput(session, "seq", selected = next_file)
      } else {
        showNotification("Letztes File erreicht!", type = "warning")
      }

    }, error = function(e) {
      print("!!! DB FEHLER !!!")
      print(e)
      showNotification(paste("Fehler beim Speichern:", e$message), type = "error", duration = NULL)
    })
  })


  output$user_info <- renderUI({
    h4(paste("User:", res_auth$first_name))
  })
}
