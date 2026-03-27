#' The application Server-Side Logic
#'
#' Änderungen/Fixes:
#' 1. Endlosschleife bei der Artenauswahl behoben (isolate).
#' 2. Spaltennamen korrigiert (prediction -> species_short).
#' 3. Robustere Checks gegen leere Daten.
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
  certainty_list <- reactive({
    DBI::dbGetQuery(pool, "SELECT certainty_id, certainty_long_de, certainty_long_en FROM public.lut_certainty_code ORDER BY certainty_id")
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

  # ---- 4b. Annotation-Modus pro User+Projekt ----
  project_mode <- reactive({
    req(input$selected_project, res_auth$user_id)
    tryCatch({
      q <- "SELECT annotation_mode FROM public.project_users WHERE project_id = $1 AND user_id = $2"
      res <- DBI::dbGetQuery(pool, q, params = list(input$selected_project, res_auth$user_id))
      if(nrow(res) == 0) return("full")
      res$annotation_mode
    }, error = function(e) "full")
  })

  # ---- 4c. Zielart-Auswahl (Filter + Sort) ----
  output$target_species_ui <- renderUI({
    req(project_mode(), species_list(), input$species_lang)
    df <- species_list()
    labels <- switch(input$species_lang,
                     "de"  = df$species_long_de,
                     "en"  = df$species_long_en,
                     "sci" = df$species_scientific)
    labels[is.na(labels) | labels == ""] <- as.character(df$species_id[is.na(labels) | labels == ""])
    choices <- setNames(as.character(df$species_id), labels)
    choices <- choices[order(names(choices))]

    if(project_mode() == "binary") {
      tagList(
        selectInput("target_species", "Zielart / Target Species:", choices = choices),
        tags$small("Modus: Binary (nur Zielart bewerten)", style = "color: #888;")
      )
    } else {
      selectInput("target_species", "Sortieren nach Art / Sort by Species:",
                  choices = c("\u2014 Alle \u2014" = "", choices))
    }
  })

  output$mode_info_ui <- renderUI({
    req(project_mode())
    if(project_mode() == "binary") {
      div(style = "padding: 6px 12px; margin-bottom: 10px; background: #d9edf7; border-left: 4px solid #31708f; border-radius: 4px;",
          icon("crosshairs"), tags$strong(" Modus: Binary"),
          tags$small(" — nur Zielart bewerten", style = "color: #31708f;"))
    } else {
      div(style = "padding: 6px 12px; margin-bottom: 10px; background: #dff0d8; border-left: 4px solid #3c763d; border-radius: 4px;",
          icon("list-check"), tags$strong(" Modus: Full"),
          tags$small(" — alle Arten bestimmen", style = "color: #3c763d;"))
    }
  })



  # ---- 5. Load Data from DB  ----
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
        --  CAST(s.spectrogram_id AS TEXT) || '.mp3' as path, for mp3 js version change all!
        CAST(s.spectrogram_id AS TEXT) || '.mp4' as path,
        s.buffer_ms,
        af.audio_file_id,
        af.sample_rate,
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
    result <- DBI::dbGetQuery(pool, query, params = list(input$selected_project))

    result |>
      dplyr::arrange(species_id, desc(score))
  })

  # Update File List
  filtered_files <- reactive({
    req(project_data())
    df <- project_data()
    mode <- project_mode()

    target_id <- if(!is.null(input$target_species) && input$target_species != "") {
      as.integer(input$target_species)
    } else NULL

    if(!is.null(target_id)) {
      df <- df |>
        dplyr::filter(species_id == target_id) |>
        dplyr::arrange(dplyr::desc(score))
    }

    if(!is.null(input$score_start)) {
      # score in DB ist smallint (x10000), input ist 0-1
      score_threshold <- as.integer(input$score_start * 10000)
      df <- df |> dplyr::filter(score <= score_threshold)
    }

    paths <- unique(df$path)

    # Bereits bearbeitete ausblenden (nur wenn Art gewählt)
    if(!is.null(target_id)) {
      if(mode == "binary") {
        q_done <- "
          SELECT CAST(s.spectrogram_id AS TEXT) || '.mp4' as path
          FROM import.annotation_status ast
          JOIN import.spectrograms s
            ON ast.audio_file_id = s.audio_file_id
            AND ast.begin_time_ms = s.begin_time_ms
          WHERE ast.target_species_id = $1
        "
        done <- DBI::dbGetQuery(pool, q_done, params = list(target_id))$path
      } else {
        q_done <- "
          SELECT CAST(s.spectrogram_id AS TEXT) || '.mp4' as path
          FROM import.annotation_status ast
          JOIN import.spectrograms s
            ON ast.audio_file_id = s.audio_file_id
            AND ast.begin_time_ms = s.begin_time_ms
          WHERE ast.target_species_id IS NULL
        "
        done <- DBI::dbGetQuery(pool, q_done)$path
      }
      paths <- paths[!paths %in% done]
    }

    paths
  })

  observe({
    req(filtered_files())
    updateSelectizeInput(session, "seq", choices = filtered_files(), server = TRUE)
  })


  # ---- 6. Current File Logic ----
  current_file_df <- reactive({
    req(input$seq, project_data())
    project_data() |>
      dplyr::filter(path == input$seq) |>
      dplyr::arrange(desc(score))
  })

  #Video Player Logic
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

  # =====================================================================
  # ALTERNATIVE: Interactive JS Spectrogram (Wavesurfer)
  # Currently deactivated in favor of the more stable MP4 video version.
  # Uncomment if needed and ensure the paths are updated to .mp3 files.
  # =====================================================================

  # observeEvent(input$seq, {
  #   req(input$seq, project_data())
  #   row_data <- project_data() |> dplyr::filter(path == input$seq)
  #
  #   if(nrow(row_data) > 0) {
  #     buffer_val <- row_data$buffer_ms[1]  # Sekunden bis Detektion
  #     seek_target <- max(0, buffer_val - 2)
  #     analysis_range <- 3  # BirdNET Fenster in Sekunden
  #
  #     sr <- row_data$sample_rate[1]
  #     max_freq <- if(is.na(sr)) 15000L else as.integer(sr / 2)
  #
  #     session$sendCustomMessage("ws_load", list(
  #       url = paste0("spectrograms/", input$seq),
  #       seek = seek_target,
  #       detection_start = buffer_val,
  #       detection_end = buffer_val + analysis_range,
  #       freq_max = max_freq
  #     ))
  #
  #   }
  # })

  #C: "Smart Species Selection"
  observeEvent(input$seq, {
    req(input$seq, project_data(), res_auth$user_id)

    current_file_info <- project_data() |> dplyr::filter(path == input$seq)
    if(nrow(current_file_info) == 0) return()

    aid <- current_file_info$audio_file_id[1]
    start_ms <- as.integer(current_file_info$start[1] * 1000)
    mode <- project_mode()

    # Status Check je nach Modus
    if(mode == "binary" && !is.null(input$target_species) && input$target_species != "") {
      q_status <- "SELECT 1 FROM import.annotation_status WHERE audio_file_id = $1 AND begin_time_ms = $2 AND target_species_id = $3"
      has_status <- DBI::dbGetQuery(pool, q_status, params = list(aid, start_ms, as.integer(input$target_species)))
    } else {
      q_status <- "SELECT 1 FROM import.annotation_status WHERE audio_file_id = $1 AND user_id = $2 AND begin_time_ms = $3"
      has_status <- DBI::dbGetQuery(pool, q_status, params = list(aid, res_auth$user_id, start_ms))
    }

    if(nrow(has_status) > 0) {
      q_gt <- "SELECT species_id FROM import.ground_truth_annotations WHERE audio_file_id = $1 AND user_id = $2 AND begin_time_ms = $3"
      existing_annotations <- DBI::dbGetQuery(pool, q_gt, params = list(aid, res_auth$user_id, start_ms))
      selected_species <- as.character(existing_annotations$species_id)
    } else {
      if(mode == "binary" && !is.null(input$target_species) && input$target_species != "") {
        target_id <- as.integer(input$target_species)
        if(target_id %in% current_file_info$species_id) {
          selected_species <- as.character(target_id)
        } else {
          selected_species <- character(0)
        }
      } else {
        selected_species <- unique(current_file_info$species_id)
        selected_species <- as.character(selected_species[!is.na(selected_species)])
      }
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

    df_cert <- certainty_list()
    cert_labels <- switch(input$species_lang, "de"=df_cert$certainty_long_de, "en"=df_cert$certainty_long_en, "sci"=df_cert$certainty_long_en)
    cert_choices <- setNames(df_cert$certainty_id, cert_labels)


    # DB Check
    aid_query <- project_data() |> dplyr::filter(path == input$seq)
    saved_behaviors <- data.frame(species_id = integer(), behavior_id = integer())

    if(nrow(aid_query) > 0) {
      current_aid <- aid_query$audio_file_id[1]
      start_ms <- as.integer(aid_query$start[1] * 1000) # << ZEIT HOLEN

      # CHANGE: Auch hier nach Zeit filtern!
      q_beh <- "
        SELECT species_id, behavior_id, certainty_id
        FROM import.ground_truth_annotations
        WHERE audio_file_id = $1
        AND user_id = $2
        AND begin_time_ms = $3
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

      # --- Certainty Value Logic (NEU, kommt direkt danach) ---
      cert_input_id <- paste0("cert_", code_id)
      val_cert_ui <- isolate(input[[cert_input_id]])
      val_cert_db <- saved_behaviors$certainty_id[saved_behaviors$species_id == as.numeric(code_id)]
      selected_cert <- 1L
      if (!is.null(val_cert_ui)) selected_cert <- val_cert_ui
      else if (length(val_cert_db) > 0 && !is.na(val_cert_db[1])) selected_cert <- val_cert_db[1]


      div(
        class = "panel panel-default", style = "margin-bottom: 5px; border-left: 4px solid #337ab7;",
        div(class = "panel-body", style = "padding: 8px;",
            fluidRow(
              column(4, tags$strong(display_name, style="line-height: 30px;")),
              column(4, selectInput(input_id, NULL, choices = beh_choices, selected = selected_val, width = "100%", selectize = FALSE)),
              column(4, selectInput(cert_input_id, NULL, choices = cert_choices, selected = selected_cert, width = "100%", selectize = FALSE))
            )
        )
      )
    })
    do.call(tagList, ui_list)
  })

  # ---- 8. Status Check (Locking) ----
  observe({
    req(input$seq, project_data(), res_auth$user_id)
    current_row <- project_data() |> dplyr::filter(path == input$seq)
    if(nrow(current_row) == 0) return()

    aid <- current_row$audio_file_id[1]
    start_ms <- as.integer(current_row$start[1] * 1000)
    mode <- project_mode()

    if(mode == "binary" && !is.null(input$target_species) && input$target_species != "") {
      query_status <- "
        SELECT user_id FROM import.annotation_status
        WHERE audio_file_id = $1 AND begin_time_ms = $2 AND target_species_id = $3
        LIMIT 1
      "
      existing_status <- DBI::dbGetQuery(pool, query_status, params = list(aid, start_ms, as.integer(input$target_species)))
    } else {
      query_status <- "
        SELECT user_id FROM import.annotation_status
        WHERE audio_file_id = $1 AND begin_time_ms = $2 AND target_species_id IS NULL
        LIMIT 1
      "
      existing_status <- DBI::dbGetQuery(pool, query_status, params = list(aid, start_ms))
    }

    if(nrow(existing_status) > 0) {
      owner_id <- existing_status$user_id
      if(owner_id == res_auth$user_id) {
        output$text1 <- renderText("Info: Du hast diesen Schnipsel bereits bearbeitet (Update).")
        shinyjs::enable("add_btn")
        updateActionButton(session, "add_btn", label = "Update", icon = icon("edit"))
      } else {
        output$text1 <- renderText("LOCKED: Schnipsel von anderem User bearbeitet!")
        shinyjs::disable("add_btn")
        updateActionButton(session, "add_btn", label = "Locked", icon = icon("lock"))
      }
    } else {
      output$text1 <- renderText("")
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

    df <- current_file_df()
    if(nrow(df) == 0) return()

    aid <- df$audio_file_id[1]
    start_ms <- as.integer(min(df$start) * 1000)
    end_ms   <- as.integer(max(df$end_sec) * 1000)
    fixed_type_id <- df$required_annotation_type_id[1]
    if(is.na(fixed_type_id)) fixed_type_id <- 1

    mode <- project_mode()
    target_species_val <- if(mode == "binary" && !is.null(input$target_species) && input$target_species != "") {
      as.integer(input$target_species)
    } else NULL

    inserts_to_do <- list()
    selected_ids <- input$inSelect

    if (!is.null(selected_ids) && length(selected_ids) > 0) {
      for(id_str in selected_ids) {
        sid <- as.integer(id_str)
        if(!is.na(sid)) {
          input_id <- paste0("beh_", sid)
          beh_val <- input[[input_id]]
          if(is.null(beh_val) || beh_val == "" || beh_val == "NA") {
            beh_val_sql <- NA_integer_
          } else {
            beh_val_sql <- as.integer(beh_val)
          }
          cert_input_id <- paste0("cert_", sid)
          cert_val <- input[[cert_input_id]]
          cert_val_sql <- if(is.null(cert_val) || cert_val == "") 1L else as.integer(cert_val)

          inserts_to_do[[length(inserts_to_do) + 1]] <- list(sid = sid, bid = beh_val_sql, cid = cert_val_sql)

        }
      }
    }

    tryCatch({
      pool::poolWithTransaction(pool, function(conn) {

        # DELETE je nach Modus
        if(!is.null(target_species_val)) {
          # Binary: Nur Zielart löschen
          DBI::dbExecute(conn,
                         "DELETE FROM import.ground_truth_annotations WHERE audio_file_id = $1 AND user_id = $2 AND begin_time_ms = $3 AND species_id = $4",
                         list(aid, res_auth$user_id, start_ms, target_species_val))
          DBI::dbExecute(conn,
                         "DELETE FROM import.annotation_status WHERE audio_file_id = $1 AND user_id = $2 AND begin_time_ms = $3 AND target_species_id = $4",
                         list(aid, res_auth$user_id, start_ms, target_species_val))
        } else {
          # Full: Alles für diesen Schnipsel löschen
          DBI::dbExecute(conn,
                         "DELETE FROM import.ground_truth_annotations WHERE audio_file_id = $1 AND user_id = $2 AND begin_time_ms = $3",
                         list(aid, res_auth$user_id, start_ms))
          DBI::dbExecute(conn,
                         "DELETE FROM import.annotation_status WHERE audio_file_id = $1 AND user_id = $2 AND begin_time_ms = $3 AND target_species_id IS NULL",
                         list(aid, res_auth$user_id, start_ms))
        }

        # INSERT ground_truth
        if (length(inserts_to_do) > 0) {
          for(item in inserts_to_do) {
            DBI::dbExecute(conn,
                           "INSERT INTO import.ground_truth_annotations
               (audio_file_id, user_id, species_id, behavior_id, certainty_id, begin_time_ms, end_time_ms, is_present)
               VALUES ($1, $2, $3, $4, $5, $6, $7, TRUE)",
                           list(aid, res_auth$user_id, item$sid, item$bid, item$cid, start_ms, end_ms))
          }
        }

        # INSERT annotation_status MIT target_species_id
        target_sid_sql <- if(!is.null(target_species_val)) target_species_val else NA_integer_
        DBI::dbExecute(conn,
                       "INSERT INTO import.annotation_status (audio_file_id, user_id, begin_time_ms, end_time_ms, annotation_type_id, target_species_id) VALUES ($1, $2, $3, $4, $5, $6)",
                       list(aid, res_auth$user_id, start_ms, end_ms, fixed_type_id, target_sid_sql))
      })

      showNotification("Gespeichert!", type = "message")

      # Navigation: filtered_files statt project_data
      current_choices <- filtered_files()
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


  # ---- Modell-Check Modal ----
  observeEvent(input$model_check_btn, {
    req(species_list(), input$species_lang)

    df <- species_list()
    labels <- switch(input$species_lang,
                     "de"  = df$species_long_de,
                     "en"  = df$species_long_en,
                     "sci" = df$species_scientific)
    labels[is.na(labels) | labels == ""] <- as.character(df$species_id[is.na(labels) | labels == ""])
    choices <- setNames(as.character(df$species_id), labels)
    choices <- choices[order(names(choices))]

    showModal(modalDialog(
      title = "Modell-Check: BirdNET Score → True Positive",
      size = "l",
      easyClose = TRUE,

      fluidRow(
        column(6, selectInput("mc_species", "Art auswählen:", choices = choices)),
        column(6, br(), actionButton("mc_run", "Berechnen",
                                     icon = icon("calculator"),
                                     style = "background-color: #337ab7; color: white; margin-top: 5px;"))
      ),
      hr(),
      plotOutput("mc_plot", height = "450px"),

      footer = modalButton("Schließen")
    ))
  })

  # Berechnung NUR wenn Button im Modal gedrückt wird
  observeEvent(input$mc_run, {
    req(input$mc_species, input$selected_project)

    sp_id <- as.integer(input$mc_species)
    df_sp <- species_list()

    # Artname für Plot-Titel
    sp_row <- df_sp[df_sp$species_id == sp_id, ]
    sp_name <- if(nrow(sp_row) > 0) {
      switch(input$species_lang,
             "de"  = sp_row$species_long_de,
             "en"  = sp_row$species_long_en,
             "sci" = sp_row$species_scientific)
    } else paste("Species", sp_id)

    # Daten holen
    glm_df <- get_glm_data(pool, as.integer(input$selected_project), sp_id)

    output$mc_plot <- renderPlot({
      if(nrow(glm_df) == 0) {
        plot.new()
        text(0.5, 0.5, "Keine annotierten Daten für diese Art.",
             cex = 1.4, col = "red")
      } else {
        plot_glm_check(glm_df, species_name = sp_name)
      }
    })
  })

}
