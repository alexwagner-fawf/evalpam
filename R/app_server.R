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
app_server <- function(input, output, session, pool) {

  # ---- 1. Authentication ----
  res_auth <- shinymanager::secure_server(
    check_credentials = check_credentials_db(pool)
  )

  # ---- 2. Load Metadata (Species & Behavior) ----

  # A) Load Species List (Filtered by Project Settings/Whitelist!)
  species_list <- reactive({
    req(input$selected_project) # Wait for project selection

    # Logic:
    # 1. Get all species.
    # 2. Check via Audio->Deployment->Project which settings are used.
    # 3. Check via JOIN on settings_species if there is a whitelist.
    # 4. If settings_species has entries, we filter.

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

    if (nrow(df) == 0) {
      # FALLBACK: If whitelist is empty (or query returns nothing), load ALL species.
      # This prevents the app from crashing on new projects without specific settings.
      return(DBI::dbGetQuery(pool, "SELECT species_id, species_short, species_long_de, species_long_en, species_scientific FROM public.lut_species_code ORDER BY species_short"))
    } else {
      return(df)
    }
  })

  # B) Load Behavior List (This was missing previously!)
  behavior_list <- reactive({
    DBI::dbGetQuery(pool, "SELECT behavior_id, behavior_long_de, behavior_long_en FROM public.lut_behavior_code ORDER BY behavior_id")
  })


  # ---- 3. Language & Labels Observer ----
  # Updates the MultiInput choices whenever the language changes
  observe({
    req(species_list(), input$species_lang)

    df <- species_list()

    # Select label column based on language input
    labels <- switch(input$species_lang,
                     "de"  = df$species_long_de,
                     "en"  = df$species_long_en,
                     "sci" = df$species_scientific
    )

    # Fallback: If a name is missing, use the Short Code
    labels[is.na(labels) | labels == ""] <- df$species_short[is.na(labels) | labels == ""]

    # Create named vector for choices:
    # Name (Visible) = Label (e.g., "Blackbird")
    # Value (Internal) = Short Code (e.g., "TURMER")
    choices <- setNames(df$species_short, labels)

    # Sort alphabetically by the visible name
    choices <- choices[order(names(choices))]

    # Keep current selection active
    current_selected <- input$inSelect

    shinyWidgets::updateMultiInput(session, "inSelect", choices = choices, selected = current_selected)
  })


  # ---- 4. Project Selection Logic ----

  # Fetch projects assigned to the logged-in user
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

  # Render Project Dropdown
  output$project_selector_ui <- renderUI({
    req(user_projects())
    if(nrow(user_projects()) == 0) return(div("No projects assigned.", style="color:red;"))

    selectInput(
      "selected_project",
      "Select Project:",
      choices = setNames(user_projects()$project_id, user_projects()$project_name_short)
    )
  })


  # ---- 5. Load Data from DB ----

  # Load ALL relevant data for the selected project
  # Joins: Results -> Audio -> Deployment -> Species -> Annotation Type
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
    # Note: For production with massive data, consider pagination or LIMIT
    DBI::dbGetQuery(pool, query, params = list(input$selected_project))
  })

  # Update Sequence Dropdown when data changes
  observeEvent(project_data(), {
    req(project_data())
    files <- unique(project_data()$path)
    updateSelectizeInput(session, "seq", choices = files, server = TRUE)
  })


  # ---- 6. Current File Logic ----

  # Filter loaded data for the specific file selected
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
    # Assumes audio files are in a folder mapped via add_resource_path "spectograms"
    full_url <- paste0("spectograms/", input$seq)

    video::changeVideo("video", full_url)

    # Seek to first prediction start time
    first_pred <- current_file_df()$start[1]
    if(!is.na(first_pred)) video::seekVideo("video", seek = first_pred)

    video::playVideo("video")
  })

  # ---- Smart Species Selection (Restore vs. BirdNET) ----
  observeEvent(input$seq, {
    req(input$seq, project_data(), res_auth$user_id)

    # 1. Get File ID
    current_file_info <- project_data() |>
      dplyr::filter(path == input$seq)

    if(nrow(current_file_info) == 0) return()
    aid <- current_file_info$audio_file_id[1]

    # 2. Check DB: Are there existing annotations for this user & file?
    query_gt <- "
      SELECT sp.species_short
      FROM import.ground_truth_annotations gt
      JOIN public.lut_species_code sp ON gt.species_id = sp.species_id
      WHERE gt.audio_file_id = $1 AND gt.user_id = $2
    "
    existing_annotations <- DBI::dbGetQuery(pool, query_gt, params = list(aid, res_auth$user_id))

    # 3. Decision Logic
    if(nrow(existing_annotations) > 0) {
      # CASE A: Restore (User already worked here)
      # We ignore BirdNET and load exactly what is in the DB
      selected_species <- existing_annotations$species_short

      # Optional: Visual Feedback
      showNotification("Loaded saved annotations.", type = "message", duration = 2)

    } else {
      # CASE B: Fresh Start (User's first time on this file)
      # We use BirdNET predictions as a starting point
      selected_species <- unique(current_file_info$prediction)
      selected_species <- selected_species[!is.na(selected_species)] # Remove NAs
    }

    # 4. Update UI
    shinyWidgets::updateMultiInput(session, "inSelect", selected = selected_species)
  })


  # ---- 7. Dynamic Behavior UI Generation (Dropdowns & Restore) ----

  output$dynamic_behavior_ui <- renderUI({
    # Dependencies: We need the lists, the file, and the user (for restore)
    req(behavior_list(), input$seq, res_auth$user_id)

    selected_codes <- input$inSelect

    # 1. Abort if no species selected
    if (is.null(selected_codes) || length(selected_codes) == 0) {
      return(tags$p("Select a species to assign behavior.", style="color: #888; font-style: italic;"))
    }

    # 2. Prepare Labels for Species
    df_sp <- species_list() # This is the FILTERED list
    labels_sp <- switch(input$species_lang,
                        "de"  = df_sp$species_long_de,
                        "en"  = df_sp$species_long_en,
                        "sci" = df_sp$species_scientific
    )
    labels_sp[is.na(labels_sp) | labels_sp == ""] <- df_sp$species_short[is.na(labels_sp) | labels_sp == ""]
    names(labels_sp) <- df_sp$species_short

    # 3. Prepare Behavior List for Dropdown
    df_beh <- behavior_list()
    beh_labels <- switch(input$species_lang,
                         "de" = df_beh$behavior_long_de,
                         "en" = df_beh$behavior_long_en,
                         "sci" = df_beh$behavior_long_en)
    beh_choices <- setNames(df_beh$behavior_id, beh_labels)

    # 4. DEFAULT Logic: Find ID for "Song" (Gesang)
    # We search case-insensitive for "Gesang" in the German column as a stable anchor
    default_row <- df_beh[grep("Gesang", df_beh$behavior_long_de, ignore.case = TRUE), ]
    default_beh_id <- if(nrow(default_row) > 0) default_row$behavior_id[1] else 1

    # 5. PRE-LOAD: Load existing behaviors from DB
    # This ensures that "Call" remains "Call" upon reload
    aid_query <- project_data() |> dplyr::filter(path == input$seq)
    saved_behaviors <- data.frame(species_short = character(), behavior_id = integer())

    if(nrow(aid_query) > 0) {
      current_aid <- aid_query$audio_file_id[1]
      q_beh <- "
        SELECT sp.species_short, gt.behavior_id
        FROM import.ground_truth_annotations gt
        JOIN public.lut_species_code sp ON gt.species_id = sp.species_id
        WHERE gt.audio_file_id = $1 AND gt.user_id = $2
      "
      saved_behaviors <- DBI::dbGetQuery(pool, q_beh, params = list(current_aid, res_auth$user_id))
    }

    # 6. UI Loop: Create a card for each selected species
    ui_list <- lapply(selected_codes, function(code) {

      input_id <- paste0("beh_", code)

      # Find Label (or fallback to code)
      display_name <- if (code %in% names(labels_sp)) labels_sp[code] else code

      # --- Decision: Which value should be selected? ---
      # 1. Did the user just change it in the UI? (isolate input)
      # 2. Is there something in the DB? (saved_behaviors)
      # 3. Default ("Song")

      val_ui <- isolate(input[[input_id]])
      val_db <- saved_behaviors$behavior_id[saved_behaviors$species_short == code]

      selected_val <- default_beh_id # Start default

      if (!is.null(val_ui)) {
        selected_val <- val_ui
      } else if (length(val_db) > 0 && !is.na(val_db[1])) {
        selected_val <- val_db[1]
      }

      # --- HTML Structure ---
      div(
        class = "panel panel-default",
        style = "margin-bottom: 10px; border-left: 5px solid #337ab7;", # Blue border left
        div(
          class = "panel-body",
          style = "padding: 10px;",
          fluidRow(
            # Left: Species Name
            column(6,
                   tags$strong(display_name, style = "line-height: 34px; font-size: 1.1em;")
            ),
            # Right: Dropdown
            column(6,
                   selectInput(
                     inputId = input_id,
                     label = NULL,
                     choices = beh_choices,
                     selected = selected_val,
                     width = "100%",
                     selectize = FALSE # Native HTML Select (often faster/cleaner in loops)
                   )
            )
          )
        )
      )
    })

    do.call(tagList, ui_list)
  })


  # ---- 8. UI Locking & Status Check ----

  observe({
    req(input$seq, project_data(), res_auth$user_id)

    # 1. Get File ID
    aid <- project_data()$audio_file_id[project_data()$path == input$seq][1]

    # 2. Check: Who processed this file?
    existing_status <- DBI::dbGetQuery(pool,
                                       "SELECT user_id FROM import.annotation_status WHERE audio_file_id = $1 LIMIT 1",
                                       params = list(aid))

    # 3. Logic Branching
    if(nrow(existing_status) > 0) {
      owner_id <- existing_status$user_id

      if(owner_id == res_auth$user_id) {
        # CASE A: Own file -> Update allowed
        output$text1 <- renderText("Note: You are updating your own annotation.")
        shinyjs::enable("add_btn")
        updateActionButton(session, "add_btn", label = "Save Changes (Update)", icon = icon("edit"))
      } else {
        # CASE B: Colleague's file -> Read-Only
        output$text1 <- renderText("WARNING: File locked by another user (Read-Only)!")
        shinyjs::disable("add_btn")
        updateActionButton(session, "add_btn", label = "Locked", icon = icon("lock"))
      }
    } else {
      # CASE C: Fresh file -> New Entry
      output$text1 <- renderText("")
      shinyjs::enable("add_btn")
      updateActionButton(session, "add_btn", label = "Save & Next", icon = icon("paper-plane"))
    }
  })


  # ---- 9. Output Tables & Info ----

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

  output$task_info <- renderUI({
    req(current_file_df())
    mode_name <- current_file_df()$annotation_type_description[1]
    if(is.na(mode_name)) mode_name <- "Standard (Not defined)"

    tagList(
      tags$strong("Task Mode: "),
      tags$span(mode_name, style = "color: blue; font-weight: bold;")
    )
  })


  # ---- 10. SAVE Logic (Database Transaction) ----

  observeEvent(input$add_btn, {
    req(input$seq, res_auth$user_id)

    df <- current_file_df()
    if(nrow(df) == 0) return()

    aid <- df$audio_file_id[1]
    start_time <- min(df$start)
    end_time <- max(df$end_sec)

    # Get required annotation type (or default to 1)
    fixed_type_id <- df$required_annotation_type_id[1]
    if(is.na(fixed_type_id)) fixed_type_id <- 1

    selected_species_codes <- input$inSelect

    tryCatch({
      pool::poolWithTransaction(pool, function(conn) {

        # --- STEP 1: Cleanup (Idempotency) ---
        # Delete existing entries for this user/file combo to allow clean insert
        DBI::dbExecute(conn, "DELETE FROM import.ground_truth_annotations WHERE audio_file_id = $1 AND user_id = $2", list(aid, res_auth$user_id))
        DBI::dbExecute(conn, "DELETE FROM import.annotation_status WHERE audio_file_id = $1 AND user_id = $2", list(aid, res_auth$user_id))


        # --- STEP 2: Insert Ground Truth (Dynamic Behavior) ---
        if (!is.null(selected_species_codes) && length(selected_species_codes) > 0) {

          for(code in selected_species_codes) {

            # A) Resolve Species ID
            sid_query <- "SELECT species_id FROM public.lut_species_code WHERE species_short = $1"
            sid <- DBI::dbGetQuery(conn, sid_query, params=list(code))$species_id

            # B) Resolve Behavior ID (Dynamic Input!)
            # Construct the ID used in renderUI: "beh_TURMER"
            input_id <- paste0("beh_", code)
            selected_behavior_id <- input[[input_id]]

            # Fallback if input is missing (should not happen with radio buttons)
            if(is.null(selected_behavior_id)) selected_behavior_id <- NA

            # C) Insert
            if(length(sid) > 0) {
              insert_q <- "
                  INSERT INTO import.ground_truth_annotations
                  (audio_file_id, user_id, species_id, behavior_id, begin_time_s, end_time_s, is_present)
                  VALUES ($1, $2, $3, $4, $5, $6, TRUE)
                "
              DBI::dbExecute(conn, insert_q, params = list(
                aid, res_auth$user_id, sid, selected_behavior_id, start_time, end_time
              ))
            }
          }
        }

        # --- STEP 3: Insert Annotation Status (Mandatory) ---
        status_q <- "INSERT INTO import.annotation_status (audio_file_id, user_id, begin_time_s, end_time_s, annotation_type_id) VALUES ($1, $2, $3, $4, $5)"
        DBI::dbExecute(conn, status_q, params = list(aid, res_auth$user_id, start_time, end_time, fixed_type_id))

      }) # End Transaction

      showNotification("Saved successfully!", type = "message")

      # --- Logic: Move to Next File ---
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
