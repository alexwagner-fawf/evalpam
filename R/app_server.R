#' The application Server-Side Logic
#'
#' Key behaviours:
#' 1. `save_trigger` is a manual reactiveVal used to invalidate DB-dependent
#'    reactives after every save (Shiny cannot detect external DB changes).
#' 2. Species selection uses internal `species_id` values; labels are derived
#'    on the fly from the chosen language.
#' 3. Annotation modes:
#'    - "binary": only the target/queue species is evaluated. Extra species in
#'      the multi-select are ignored on save. Abiotic sounds are skipped too.
#'    - "full": the entire clip is labelled. A chosen target species acts
#'      purely as a queue/sort key; occupancy still counts only that species.
#'      With no target species set, this is free full annotation without any
#'      occupancy auto-stop.
#' 4. Occupancy groups (`import.occupancy_groups` + `import.spectrogram_groups`)
#'    drive the auto-stop: once a group has `target_count` certain hits for the
#'    active queue species, its remaining spectrograms are filtered out.
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

  # Manual trigger to invalidate DB-dependent reactives after every save.
  # Shiny does not see external state changes (DB writes), so we bump this
  # counter explicitly to force re-evaluation of `filtered_files()`,
  # `occupancy_status()` and similar reactives.
  save_trigger <- reactiveVal(0L)

  # ---- 2. Load Metadata (Species, Behavior, Certainty, Abiotic Sounds) ----

  # A) Species whitelist for the current project (driven by settings_species)
  species_list <- reactive({
    req(input$selected_project)

    # Joins through settings -> results -> audio_files -> deployments to find
    # only species that are actually relevant to the selected project.
    query <- "
      SELECT DISTINCT sp.species_id, sp.species_short, sp.species_long_de,
                      sp.species_long_en, sp.species_scientific
      FROM public.lut_species_code sp
      JOIN import.settings_species ss ON sp.species_id = ss.species_id
      JOIN import.settings s          ON ss.settings_id = s.settings_id
      JOIN import.results r           ON s.settings_id = r.settings_id
      JOIN import.audio_files af      ON r.audio_file_id = af.audio_file_id
      JOIN import.deployments d       ON af.deployment_id = d.deployment_id
      WHERE d.project_id = $1
      ORDER BY sp.species_short
    "
    df <- DBI::dbGetQuery(pool, query, params = list(input$selected_project))

    # Safety net: if the whitelist is empty for any reason, fall back to all
    # known species so the UI still has something to display.
    if (nrow(df) == 0) {
      return(DBI::dbGetQuery(pool,
                             "SELECT species_id, species_short, species_long_de, species_long_en, species_scientific
         FROM public.lut_species_code ORDER BY species_short"))
    } else {
      return(df)
    }
  })

  # B) Static lookup tables (behavior, certainty, abiotic sounds).
  # These never change at runtime; wrapping them in reactive() is fine because
  # they will be cached after the first call.
  behavior_list <- reactive({
    DBI::dbGetQuery(pool,
                    "SELECT behavior_id, behavior_long_de, behavior_long_en
       FROM public.lut_behavior_code ORDER BY behavior_id")
  })
  certainty_list <- reactive({
    DBI::dbGetQuery(pool,
                    "SELECT certainty_id, certainty_long_de, certainty_long_en
       FROM public.lut_certainty_code ORDER BY certainty_id")
  })
  abiotic_list <- reactive({
    DBI::dbGetQuery(pool,
                    "SELECT abiotic_sound_id, abiotic_sound_long_de, abiotic_sound_long_en
       FROM public.lut_abiotic_sound_code ORDER BY abiotic_sound_id")
  })

  # ---- 3. Language & Labels Observer ----
  # Rebuilds the multi-select choices whenever the language toggle changes.
  # Choice *values* are species IDs (as strings, since Shiny inputs are strings);
  # choice *labels* are the localised names. Current selection is preserved.
  observe({
    req(species_list(), input$species_lang)
    df <- species_list()

    labels <- switch(input$species_lang,
                     "de"  = df$species_long_de,
                     "en"  = df$species_long_en,
                     "sci" = df$species_scientific
    )
    # Fallback: if a name is missing, show the raw species_id.
    labels[is.na(labels) | labels == ""] <-
      as.character(df$species_id[is.na(labels) | labels == ""])

    choices <- setNames(as.character(df$species_id), labels)
    choices <- choices[order(names(choices))]

    # isolate() prevents this observer from feeding back into itself when
    # the selection state changes.
    current_selected <- isolate(input$inSelect)

    shinyWidgets::updateMultiInput(session, "inSelect",
                                   choices = choices, selected = current_selected)
  })

  # ---- 4. Project Selection ----
  user_projects <- reactive({
    req(res_auth$user_id)
    DBI::dbGetQuery(pool,
                    "SELECT p.project_id, p.project_name_short
       FROM import.projects p
       JOIN public.project_users pu ON p.project_id = pu.project_id
       WHERE pu.user_id = $1
       ORDER BY p.project_name_short",
                    params = list(res_auth$user_id))
  })

  output$project_selector_ui <- renderUI({
    req(user_projects())
    if (nrow(user_projects()) == 0) {
      return(div("No projects assigned.", style = "color:red;"))
    }
    selectInput("selected_project", "Select Project:",
                choices = setNames(user_projects()$project_id,
                                   user_projects()$project_name_short))
  })

  # ---- 4b. Annotation mode per (user, project) ----
  # Reads from public.project_users.annotation_mode. Falls back to "full" if
  # either no row exists or the query errors out.
  project_mode <- reactive({
    req(input$selected_project, res_auth$user_id)
    tryCatch({
      q <- "SELECT annotation_mode FROM public.project_users
            WHERE project_id = $1 AND user_id = $2"
      res <- DBI::dbGetQuery(pool, q,
                             params = list(input$selected_project, res_auth$user_id))
      if (nrow(res) == 0) return("full")
      res$annotation_mode
    }, error = function(e) "full")
  })

  # ---- Queue / target species helpers ----
  # The "target species" input doubles as the queue/task species:
  # - binary: species being evaluated for presence/absence
  # - full:   optional sort/queue species; the whole clip is still annotated,
  #           but occupancy counts only for this species
  # - full + empty target: free full annotation, no auto-stop
  current_target_species <- reactive({
    if (!is.null(input$target_species) && input$target_species != "") {
      as.integer(input$target_species)
    } else {
      NULL
    }
  })

  occupancy_active <- reactive({
    isTRUE(input$use_occupancy_filter) && !is.null(current_target_species())
  })

  # ---- Occupancy status per group, for the active queue species ----
  # Important: hits are counted only for ground-truth rows whose status row
  # was saved against the *same* target_species_id. This prevents "side-effect"
  # hits from full-mode annotations of other species from leaking into a
  # different species' queue.
  occupancy_status <- reactive({
    save_trigger()  # Manual invalidation dependency
    req(input$selected_project)
    if (!occupancy_active()) return(NULL)

    target_id <- current_target_species()

    q <- "
      SELECT
        og.group_id,
        og.group_name,
        og.target_count,
        COUNT(DISTINCT (gt.audio_file_id, gt.begin_time_ms)) AS n_hits,
        COUNT(DISTINCT sg.spectrogram_id) AS n_total_spectrograms
      FROM import.occupancy_groups og
      JOIN import.spectrogram_groups sg USING (group_id)
      JOIN import.spectrograms s        ON s.spectrogram_id = sg.spectrogram_id
      LEFT JOIN import.annotation_status ast
        ON ast.audio_file_id     = s.audio_file_id
       AND ast.begin_time_ms     = s.begin_time_ms
       AND ast.target_species_id = $2
      LEFT JOIN import.ground_truth_annotations gt
        ON gt.audio_file_id      = ast.audio_file_id
       AND gt.begin_time_ms      = ast.begin_time_ms
       AND gt.user_id            = ast.user_id
       AND gt.species_id         = ast.target_species_id
       AND gt.certainty_id       = 1
       AND gt.is_present         = TRUE
      WHERE og.project_id = $1
      GROUP BY og.group_id, og.group_name, og.target_count
      ORDER BY og.group_name
    "

    tryCatch(
      DBI::dbGetQuery(pool, q,
                      params = list(as.integer(input$selected_project), target_id)),
      error = function(e) NULL
    )
  })

  # ---- Occupancy info box ----
  # Shows per-group progress (n_hits / target_count) and highlights which group
  # the currently selected spectrogram belongs to. Multiple groups can light up
  # if a spectrogram is in several groups (e.g. plot-wide + morning-only).
  output$occupancy_info_ui <- renderUI({
    if (!occupancy_active()) return(NULL)
    req(input$target_species, input$target_species != "")

    st <- occupancy_status()
    if (is.null(st) || nrow(st) == 0) return(NULL)

    # Which groups contain the currently selected spectrogram?
    current_groups <- integer(0)
    if (!is.null(input$seq) && input$seq != "") {
      spec_id <- tools::file_path_sans_ext(input$seq)
      current_groups <- tryCatch(
        DBI::dbGetQuery(pool,
                        "SELECT group_id FROM import.spectrogram_groups WHERE spectrogram_id = $1",
                        params = list(as.integer(spec_id)))$group_id,
        error = function(e) integer(0)
      )
    }

    n_done <- sum(st$n_hits >= st$target_count)

    group_rows <- lapply(seq_len(nrow(st)), function(i) {
      is_current <- st$group_id[i] %in% current_groups
      is_done    <- st$n_hits[i] >= st$target_count[i]
      color      <- if (is_done) "#3c763d" else if (is_current) "#31708f" else "#888"
      weight     <- if (is_current) "bold" else "normal"
      bg         <- if (is_current) "#d9edf7" else "transparent"
      icon_chr   <- if (is_done) "\u2705" else if (is_current) "\U0001F3AF" else "\u25CB"

      tags$div(
        style = sprintf("color: %s; font-weight: %s; font-size: 12px; padding: 2px 4px; background: %s; border-radius: 3px;",
                        color, weight, bg),
        sprintf("%s %s: %d / %d", icon_chr,
                st$group_name[i],
                as.integer(st$n_hits[i]),
                as.integer(st$target_count[i]))
      )
    })

    div(style = "padding: 8px 12px; margin-bottom: 10px;
                 background: #fcf8e3; border-left: 4px solid #f0ad4e;
                 border-radius: 4px; font-size: 13px;",
        icon("bullseye"),
        tags$strong(sprintf(" Occupancy: %d/%d groups completed",
                            as.integer(n_done), nrow(st))),
        tags$div(style = "margin-top: 6px;", group_rows)
    )
  })

  # ---- 4c. Target species selector (filter + sort) ----
  # The label changes by mode but the input ID stays the same so all
  # downstream reactives can read input$target_species uniformly.
  # current_sel is preserved across re-renders (e.g. when the language
  # toggle rebuilds the choices) so the user's selection doesn't snap back
  # to the first alphabetical entry.
  output$target_species_ui <- renderUI({
    req(project_mode(), species_list(), input$species_lang)
    df <- species_list()
    labels <- switch(input$species_lang,
                     "de"  = df$species_long_de,
                     "en"  = df$species_long_en,
                     "sci" = df$species_scientific)
    labels[is.na(labels) | labels == ""] <-
      as.character(df$species_id[is.na(labels) | labels == ""])
    choices <- setNames(as.character(df$species_id), labels)
    choices <- choices[order(names(choices))]

    current_sel <- isolate(input$target_species)

    if (project_mode() == "binary") {
      tagList(
        selectInput("target_species", "Target species:",
                    choices = choices, selected = current_sel),
        tags$small("Mode: Binary (only target species is evaluated)",
                   style = "color: #888;")
      )
    } else {
      selectInput("target_species", "Sort by species (queue):",
                  choices = c("\u2014 All \u2014" = "", choices),
                  selected = current_sel)
    }
  })

  output$mode_info_ui <- renderUI({
    req(project_mode())
    if (project_mode() == "binary") {
      div(style = "padding: 6px 12px; margin-bottom: 10px; background: #d9edf7; border-left: 4px solid #31708f; border-radius: 4px;",
          icon("crosshairs"), tags$strong(" Mode: Binary"),
          tags$small(" \u2014 evaluate target species only", style = "color: #31708f;"))
    } else {
      div(style = "padding: 6px 12px; margin-bottom: 10px; background: #dff0d8; border-left: 4px solid #3c763d; border-radius: 4px;",
          icon("list-check"), tags$strong(" Mode: Full"),
          tags$small(" \u2014 label whole clip; target species optional as queue",
                     style = "color: #3c763d;"))
    }
  })


  # ---- 5. Load project data ----
  # Pulls everything the UI needs in one query: detections, spectrogram paths,
  # audio/deployment metadata. Sorted by (species, score desc) so the queue
  # for any chosen species starts with its highest-confidence detections.
  project_data <- reactive({
    req(input$selected_project)

    query <- "
      SELECT
        r.result_id,
        r.confidence AS score,
        CAST(r.begin_time_ms AS FLOAT) / 1000.0 AS start,
        CAST(r.end_time_ms   AS FLOAT) / 1000.0 AS end_sec,
        CAST(s.spectrogram_id AS TEXT) || '.mp3' AS path,
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
      JOIN import.spectrograms s     ON r.result_id = s.result_id
      JOIN import.audio_files af     ON r.audio_file_id = af.audio_file_id
      JOIN import.deployments d      ON af.deployment_id = d.deployment_id
      JOIN public.lut_species_code sp ON r.species_id = sp.species_id
      LEFT JOIN public.lut_annotation_type_code lt
        ON af.required_annotation_type_id = lt.annotation_type_id
      WHERE d.project_id = $1
      ORDER BY af.relative_path, r.begin_time_ms
    "
    result <- DBI::dbGetQuery(pool, query, params = list(input$selected_project))

    result |> dplyr::arrange(species_id, desc(score))
  })

  # ---- Filtered file list (the queue) ----
  # Applies, in order: target-species filter, score threshold, "already done"
  # filter, and the occupancy auto-stop. Returns a vector of spectrogram paths.
  filtered_files <- reactive({
    save_trigger()  # Manual invalidation dependency
    req(project_data())
    df <- project_data()

    target_id <- current_target_species()

    if (!is.null(target_id)) {
      df <- df |>
        dplyr::filter(species_id == target_id) |>
        dplyr::arrange(dplyr::desc(score))
    }

    if (!is.null(input$score_start)) {
      # DB stores confidence as smallint (raw_conf * 10000); the slider is 0-1.
      score_threshold <- as.integer(input$score_start * 10000)
      df <- df |> dplyr::filter(score <= score_threshold)
    }

    paths <- unique(df$path)

    # "Already done" filter: hide spectrograms that were already saved against
    # the current queue species. target_species_id in annotation_status now
    # always means "queue/task species" (binary OR full-with-queue).
    if (!is.null(target_id)) {
      q_done <- "
        SELECT CAST(s.spectrogram_id AS TEXT) || '.mp3' AS path
        FROM import.annotation_status ast
        JOIN import.spectrograms s
          ON ast.audio_file_id = s.audio_file_id
         AND ast.begin_time_ms = s.begin_time_ms
        WHERE ast.target_species_id = $1
      "
      done <- DBI::dbGetQuery(pool, q_done, params = list(target_id))$path
      paths <- paths[!paths %in% done]

      # Occupancy auto-stop: drop spectrograms whose group has already reached
      # its target_count of certain hits for this queue species.
      if (occupancy_active()) {
        q_done_groups <- "
          SELECT DISTINCT CAST(s.spectrogram_id AS TEXT) || '.mp3' AS path
          FROM import.spectrogram_groups sg
          JOIN import.spectrograms s USING (spectrogram_id)
          WHERE sg.group_id IN (
            SELECT og.group_id
            FROM import.occupancy_groups og
            JOIN import.spectrogram_groups sg2 USING (group_id)
            JOIN import.spectrograms s2 ON s2.spectrogram_id = sg2.spectrogram_id
            LEFT JOIN import.annotation_status ast
              ON ast.audio_file_id     = s2.audio_file_id
             AND ast.begin_time_ms     = s2.begin_time_ms
             AND ast.target_species_id = $1
            LEFT JOIN import.ground_truth_annotations gt
              ON gt.audio_file_id      = ast.audio_file_id
             AND gt.begin_time_ms      = ast.begin_time_ms
             AND gt.user_id            = ast.user_id
             AND gt.species_id         = ast.target_species_id
             AND gt.certainty_id       = 1
             AND gt.is_present         = TRUE
            WHERE og.project_id = $2
            GROUP BY og.group_id, og.target_count
            HAVING COUNT(DISTINCT (gt.audio_file_id, gt.begin_time_ms))
                   >= og.target_count
          )
        "
        done_in_groups <- DBI::dbGetQuery(
          pool, q_done_groups,
          params = list(target_id, as.integer(input$selected_project))
        )$path
        paths <- paths[!paths %in% done_in_groups]
      }
    }

    paths
  })

  # Sync the selectizeInput with the current filtered list. Keeps the current
  # selection if it's still in the list; otherwise jumps to the first entry.
  observe({
    choices <- filtered_files()
    current <- isolate(input$seq)

    selected <- if (!is.null(current) && current %in% choices) {
      current
    } else if (length(choices) > 0) {
      choices[1]
    } else {
      character(0)
    }

    updateSelectizeInput(session, "seq",
                         choices = choices, selected = selected, server = TRUE)
  })


  # ---- 6. Current file row(s) ----
  current_file_df <- reactive({
    req(input$seq, project_data())
    project_data() |>
      dplyr::filter(path == input$seq) |>
      dplyr::arrange(desc(score))
  })

  # Frequency inputs are numericInput, which fires on every keystroke.
  # Without debouncing, each intermediate digit would trigger a full
  # WaveSurfer destroy/recreate. 600 ms is enough to let users finish typing.
  # input$seq is NOT debounced \u2014 file changes must feel immediate.
  freq_max_d <- debounce(reactive(input$freq_max_display), 600)
  freq_min_d <- debounce(reactive(input$freq_min_display), 600)

  # ---- WaveSurfer load helper ----
  # Shared by the two observers below (seq change vs. freq change) so the
  # load logic isn't duplicated.
  do_ws_load <- function(seq_val, fmax, fmin) {
    row_data <- project_data() |> dplyr::filter(path == seq_val)
    if (nrow(row_data) == 0) return(invisible(NULL))

    buffer_val     <- row_data$buffer_ms[1] #/ 1000   # ms -> s?
    seek_target    <- max(0, buffer_val - 2)
    analysis_range <- 3L
    sr             <- row_data$sample_rate[1]

    # af.sample_rate may reflect BirdNET's analysis rate (e.g. 8000 Hz)
    # rather than the recording's actual sample rate (typically 48000 Hz).
    # WaveSurfer clamps frequencyMax against the decoded audio's nyquist
    # internally, so a generous upper bound is enough here.
    nyquist_cap   <- 24000L
    user_freq_max <- if (!is.null(fmax) && !is.na(fmax)) as.integer(fmax) else nyquist_cap
    user_freq_min <- if (!is.null(fmin) && !is.na(fmin)) as.integer(fmin) else 0L
    eff_freq_max  <- min(nyquist_cap, user_freq_max)
    eff_freq_min  <- max(0L, user_freq_min)

    spec_path  <- Sys.getenv("spectrogram_folder")
    if (spec_path == "") spec_path <- "spectrograms"
    local_file <- file.path(spec_path, seq_val)

    # If the local cache is missing, fall back to the BYTEA blob in the DB.
    if (!file.exists(local_file)) {
      spec_id  <- tools::file_path_sans_ext(seq_val)
      blob_row <- tryCatch(
        DBI::dbGetQuery(pool,
                        "SELECT audio_data FROM import.spectrograms WHERE spectrogram_id = $1",
                        params = list(as.integer(spec_id))),
        error = function(e) NULL
      )
      if (!is.null(blob_row) && nrow(blob_row) > 0 &&
          !is.null(blob_row$audio_data[[1]])) {
        dir.create(spec_path, showWarnings = FALSE, recursive = TRUE)
        writeBin(blob_row$audio_data[[1]], local_file)
      }
    }

    session$sendCustomMessage("ws_load", list(
      url             = paste0("spectrograms/", seq_val),
      seek            = seek_target,
      detection_start = buffer_val,
      detection_end   = buffer_val + analysis_range,
      freq_max        = eff_freq_max,
      freq_min        = eff_freq_min,
      sample_rate     = if (is.na(sr)) 44100L else as.integer(sr)
    ))
  }

  # Recording changed -> load immediately. isolate() on the freq values keeps
  # this observer from re-firing when the debounced freq reactives settle.
  observeEvent(input$seq, {
    req(input$seq, project_data())
    do_ws_load(input$seq, isolate(freq_max_d()), isolate(freq_min_d()))
    updateSelectizeInput(session, "abiotic_sounds", selected = character(0))
  })

  # Frequency range changed by user -> reload current recording.
  # ignoreInit = TRUE prevents double-firing at startup: the debounced
  # reactives compute their initial values right when input$seq's observer
  # is already loading the first file.
  observeEvent(list(freq_max_d(), freq_min_d()), {
    req(input$seq, project_data())
    do_ws_load(input$seq, freq_max_d(), freq_min_d())
  }, ignoreInit = TRUE)

  # ---- Spectrogram region selection -> filtered audio playback ----
  # Receives {spec_id, t_start, t_end, f_min, f_max} from the canvas overlay
  # in wavesurfer_init.js, applies a time-trim + bandpass filter, and sends
  # the result back as a base64 data-URL for immediate playback.
  observeEvent(input$spec_selection, {
    sel <- input$spec_selection
    req(sel$spec_id, !is.null(sel$t_start), !is.null(sel$t_end))

    spec_path  <- Sys.getenv("spectrogram_folder")
    if (spec_path == "") spec_path <- "spectrograms"
    local_file <- file.path(spec_path, paste0(sel$spec_id, ".mp3"))

    # Same DB fallback as in do_ws_load.
    if (!file.exists(local_file)) {
      blob_row <- tryCatch(
        DBI::dbGetQuery(pool,
                        "SELECT audio_data FROM import.spectrograms WHERE spectrogram_id = $1",
                        params = list(as.integer(sel$spec_id))),
        error = function(e) NULL
      )
      if (!is.null(blob_row) && nrow(blob_row) > 0 &&
          !is.null(blob_row$audio_data[[1]])) {
        dir.create(spec_path, showWarnings = FALSE, recursive = TRUE)
        writeBin(blob_row$audio_data[[1]], local_file)
      } else return()
    }

    filtered_b64 <- tryCatch({
      wave <- tuneR::readMP3(local_file)
      sr   <- wave@samp.rate

      # Time trim
      t_start <- max(0,                          as.numeric(sel$t_start))
      t_end   <- min(length(wave@left) / sr,     as.numeric(sel$t_end))
      if (t_end > t_start)
        wave <- seewave::cutw(wave, from = t_start, to = t_end, output = "Wave")

      # Bandpass filter (only when a meaningful frequency band is selected)
      f_min <- as.numeric(sel$f_min)
      f_max <- as.numeric(sel$f_max)
      nyq   <- sr / 2
      if (f_max > f_min && f_max <= nyq)
        wave <- seewave::ffilter(wave, from = f_min, to = f_max, output = "Wave")

      # WAV -> MP3 -> base64 data-URL
      tmp_wav <- tempfile(fileext = ".wav")
      tmp_mp3 <- tempfile(fileext = ".mp3")
      on.exit({ unlink(tmp_wav); unlink(tmp_mp3) }, add = TRUE)
      tuneR::writeWave(wave, tmp_wav)
      av::av_audio_convert(tmp_wav, tmp_mp3, verbose = FALSE)
      paste0("data:audio/mpeg;base64,", base64enc::base64encode(tmp_mp3))
    }, error = function(e) {
      warning("spec_selection filter failed: ", e$message)
      NULL
    })

    if (!is.null(filtered_b64))
      session$sendCustomMessage("ws_play_selection", list(dataUrl = filtered_b64))
  })

  # ---- Smart species pre-selection ----
  # When a new spectrogram is opened, populate inSelect either from a previous
  # save by the current user (edit mode) or, on first visit, from the BirdNET
  # detections in that clip. The status query is scoped by queue context.
  observeEvent(input$seq, {
    req(input$seq, project_data(), res_auth$user_id)

    current_file_info <- project_data() |> dplyr::filter(path == input$seq)
    if (nrow(current_file_info) == 0) return()

    aid       <- current_file_info$audio_file_id[1]
    start_ms  <- as.integer(current_file_info$start[1] * 1000)
    mode      <- project_mode()
    target_id <- current_target_species()

    # With a target/queue species, look up the status row for that task.
    # Without one, treat this as free full annotation (target_species_id IS NULL).
    if (!is.null(target_id)) {
      q_status <- "
        SELECT 1 FROM import.annotation_status
        WHERE audio_file_id = $1 AND begin_time_ms = $2 AND target_species_id = $3
        LIMIT 1
      "
      has_status <- DBI::dbGetQuery(pool, q_status,
                                    params = list(aid, start_ms, target_id))
    } else {
      q_status <- "
        SELECT 1 FROM import.annotation_status
        WHERE audio_file_id = $1 AND user_id = $2 AND begin_time_ms = $3
          AND target_species_id IS NULL
        LIMIT 1
      "
      has_status <- DBI::dbGetQuery(pool, q_status,
                                    params = list(aid, res_auth$user_id, start_ms))
    }

    if (nrow(has_status) > 0) {
      # Edit mode: load whatever species this user previously saved here.
      q_gt <- "
        SELECT species_id FROM import.ground_truth_annotations
        WHERE audio_file_id = $1 AND user_id = $2 AND begin_time_ms = $3
          AND species_id IS NOT NULL
      "
      existing <- DBI::dbGetQuery(pool, q_gt,
                                  params = list(aid, res_auth$user_id, start_ms))
      selected_species <- as.character(existing$species_id)
    } else {
      if (mode == "binary" && !is.null(target_id)) {
        # Binary: pre-select target species if BirdNET found it in this clip.
        # User can deselect it to record a confirmed absence (status saved
        # without a corresponding ground-truth row).
        if (target_id %in% current_file_info$species_id) {
          selected_species <- as.character(target_id)
        } else {
          selected_species <- character(0)
        }
      } else {
        # Full mode: pre-select all BirdNET species found in this clip.
        selected_species <- unique(current_file_info$species_id)
        selected_species <- as.character(selected_species[!is.na(selected_species)])
      }
    }

    shinyWidgets::updateMultiInput(session, "inSelect", selected = selected_species)
  })


  # ---- Abiotic sounds language sync ----
  observe({
    req(abiotic_list(), input$species_lang)
    df <- abiotic_list()

    labels <- switch(input$species_lang,
                     "de"  = df$abiotic_sound_long_de,
                     "en"  = df$abiotic_sound_long_en,
                     "sci" = df$abiotic_sound_long_en)   # fallback for "sci"

    choices <- setNames(as.character(df$abiotic_sound_id), labels)

    current_placeholder <- switch(input$species_lang,
                                  "de"  = "Wind, Regen, Verkehr...",
                                  "en"  = "Wind, rain, traffic...",
                                  "sci" = "Wind, rain, traffic...")

    updateSelectizeInput(session, "abiotic_sounds",
                         choices = choices, server = TRUE,
                         options = list(placeholder = current_placeholder))
  })

  # ---- 7. Dynamic Behavior + Certainty UI ----
  # For each species currently selected in inSelect, render a row with a
  # behavior dropdown and a certainty dropdown. Pre-fills values from the DB
  # if this user already saved annotations for the current spectrogram.
  output$dynamic_behavior_ui <- renderUI({
    req(behavior_list(), input$seq, res_auth$user_id)

    selected_ids <- input$inSelect   # values are species_id strings

    if (is.null(selected_ids) || length(selected_ids) == 0) {
      return(tags$p("Select a species to assign behaviour.",
                    style = "color: #888; font-style: italic;"))
    }

    df_sp   <- species_list()
    df_beh  <- behavior_list()
    df_cert <- certainty_list()

    beh_labels  <- switch(input$species_lang,
                          "de" = df_beh$behavior_long_de,
                          "en" = df_beh$behavior_long_en,
                          "sci" = df_beh$behavior_long_en)
    beh_choices <- setNames(df_beh$behavior_id, beh_labels)

    cert_labels  <- switch(input$species_lang,
                           "de" = df_cert$certainty_long_de,
                           "en" = df_cert$certainty_long_en,
                           "sci" = df_cert$certainty_long_en)
    cert_choices <- setNames(df_cert$certainty_id, cert_labels)

    # Fetch existing annotations for this user + this exact spectrogram (time-scoped)
    aid_query <- project_data() |> dplyr::filter(path == input$seq)
    saved_behaviors <- data.frame(species_id  = integer(),
                                  behavior_id = integer(),
                                  certainty_id = integer())

    if (nrow(aid_query) > 0) {
      current_aid <- aid_query$audio_file_id[1]
      start_ms    <- as.integer(aid_query$start[1] * 1000)

      q_beh <- "
        SELECT species_id, behavior_id, certainty_id
        FROM import.ground_truth_annotations
        WHERE audio_file_id = $1 AND user_id = $2 AND begin_time_ms = $3
      "
      saved_behaviors <- DBI::dbGetQuery(pool, q_beh,
                                         params = list(current_aid, res_auth$user_id, start_ms))
    }

    ui_list <- lapply(selected_ids, function(code_id) {

      input_id      <- paste0("beh_",  code_id)
      cert_input_id <- paste0("cert_", code_id)

      # Display name for the species in the current language
      curr_sp_row <- df_sp[df_sp$species_id == as.numeric(code_id), ]
      display_name <- "Unknown"
      if (nrow(curr_sp_row) > 0) {
        display_name <- switch(input$species_lang,
                               "de"  = curr_sp_row$species_long_de,
                               "en"  = curr_sp_row$species_long_en,
                               "sci" = curr_sp_row$species_scientific)
      }

      # Behavior selection: prefer current UI value, then DB value, then
      # default to "Song" if available.
      val_ui <- isolate(input[[input_id]])
      val_db <- saved_behaviors$behavior_id[
        saved_behaviors$species_id == as.numeric(code_id)
      ]

      default_row <- df_beh[grep("Gesang", df_beh$behavior_long_de,
                                 ignore.case = TRUE), ]
      selected_val <- if (nrow(default_row) > 0) default_row$behavior_id[1] else 1L

      if (!is.null(val_ui)) selected_val <- val_ui
      else if (length(val_db) > 0 && !is.na(val_db[1])) selected_val <- val_db[1]

      # Certainty selection: prefer UI, then DB, default to "certain" (1).
      val_cert_ui <- isolate(input[[cert_input_id]])
      val_cert_db <- saved_behaviors$certainty_id[
        saved_behaviors$species_id == as.numeric(code_id)
      ]
      selected_cert <- 1L
      if (!is.null(val_cert_ui)) selected_cert <- val_cert_ui
      else if (length(val_cert_db) > 0 && !is.na(val_cert_db[1]))
        selected_cert <- val_cert_db[1]

      div(
        class = "panel panel-default",
        style = "margin-bottom: 5px; border-left: 4px solid #337ab7;",
        div(class = "panel-body", style = "padding: 8px;",
            fluidRow(
              column(4, tags$strong(display_name, style = "line-height: 30px;")),
              column(4, selectInput(input_id, NULL, choices = beh_choices,
                                    selected = selected_val,
                                    width = "100%", selectize = FALSE)),
              column(4, selectInput(cert_input_id, NULL, choices = cert_choices,
                                    selected = selected_cert,
                                    width = "100%", selectize = FALSE))
            )
        )
      )
    })
    do.call(tagList, ui_list)
  })

  # ---- 8. Status check (per-task locking) ----
  # Determines whether the current spectrogram is locked, editable by this
  # user, or fresh. Locking is per queue task: the same spectrogram can be
  # locked for one target species while being free for another.
  observe({
    req(input$seq, project_data(), res_auth$user_id)
    current_row <- project_data() |> dplyr::filter(path == input$seq)
    if (nrow(current_row) == 0) return()

    aid       <- current_row$audio_file_id[1]
    start_ms  <- as.integer(current_row$start[1] * 1000)
    target_id <- current_target_species()

    if (!is.null(target_id)) {
      query_status <- "
        SELECT user_id FROM import.annotation_status
        WHERE audio_file_id = $1 AND begin_time_ms = $2 AND target_species_id = $3
        LIMIT 1
      "
      existing_status <- DBI::dbGetQuery(pool, query_status,
                                         params = list(aid, start_ms, target_id))
    } else {
      query_status <- "
        SELECT user_id FROM import.annotation_status
        WHERE audio_file_id = $1 AND begin_time_ms = $2 AND target_species_id IS NULL
        LIMIT 1
      "
      existing_status <- DBI::dbGetQuery(pool, query_status,
                                         params = list(aid, start_ms))
    }

    if (nrow(existing_status) > 0) {
      owner_id <- existing_status$user_id
      if (owner_id == res_auth$user_id) {
        output$text1 <- renderText("Info: you already annotated this clip (Update mode).")
        shinyjs::enable("add_btn")
        updateActionButton(session, "add_btn", label = "Update", icon = icon("edit"))
      } else {
        output$text1 <- renderText("LOCKED: clip is being annotated by another user!")
        shinyjs::disable("add_btn")
        updateActionButton(session, "add_btn", label = "Locked", icon = icon("lock"))
      }
    } else {
      output$text1 <- renderText("")
      shinyjs::enable("add_btn")
      updateActionButton(session, "add_btn",
                         label = "Save & Next", icon = icon("paper-plane"))
    }
  })


  # ---- 9. BirdNET detections table ----
  output$table_bnet <- DT::renderDataTable({
    req(current_file_df(), input$species_lang)
    df <- current_file_df()

    labs <- switch(input$species_lang,
                   "de"  = df$species_long_de,
                   "en"  = df$species_long_en,
                   "sci" = df$species_scientific)
    labs[is.na(labs)] <- df$species_short[is.na(labs)]

    df_show <- df |>
      dplyr::mutate(prediction = labs, score = score / 10000) |>
      dplyr::select(prediction, score, start, end_sec)

    DT::datatable(df_show, options = list(dom = "t", pageLength = 5)) |>
      DT::formatRound("score", 2)
  })


  # ---- 10. Save logic ----
  # Persists a single clip's annotations transactionally:
  #   1) figure out the next file BEFORE invalidating reactives,
  #   2) DELETE the old ground-truth + status rows for this user/clip/task,
  #   3) INSERT the new rows,
  #   4) trigger reactive re-evaluation,
  #   5) check whether this save just completed an occupancy group,
  #   6) navigate to the next file.
  observeEvent(input$add_btn, {
    req(input$seq, res_auth$user_id)

    df <- current_file_df()
    if (nrow(df) == 0) return()

    aid           <- df$audio_file_id[1]
    start_ms      <- as.integer(min(df$start) * 1000)
    end_ms        <- as.integer(max(df$end_sec) * 1000)
    fixed_type_id <- df$required_annotation_type_id[1]
    if (is.na(fixed_type_id)) fixed_type_id <- 1L

    mode               <- project_mode()
    target_species_val <- current_target_species()

    inserts_to_do <- list()

    # In binary mode the only species saved is the target species itself.
    # Anything extra in inSelect (smart pre-selection etc.) is dropped.
    selected_ids_raw <- input$inSelect
    if (mode == "binary" && !is.null(target_species_val)) {
      selected_ids <- intersect(selected_ids_raw, as.character(target_species_val))
    } else {
      selected_ids <- selected_ids_raw
    }

    # ---- Block 1: species rows ----
    if (!is.null(selected_ids) && length(selected_ids) > 0) {
      for (id_str in selected_ids) {
        sid <- as.integer(id_str)
        if (!is.na(sid)) {
          beh_val <- input[[paste0("beh_", sid)]]
          beh_val_sql <- if (is.null(beh_val) || beh_val == "" || beh_val == "NA") {
            NA_integer_
          } else as.integer(beh_val)

          cert_val     <- input[[paste0("cert_", sid)]]
          cert_val_sql <- if (is.null(cert_val) || cert_val == "") 1L else as.integer(cert_val)

          inserts_to_do[[length(inserts_to_do) + 1]] <- list(
            sid   = sid,
            bid   = beh_val_sql,
            cid   = cert_val_sql,
            ab_id = NA_integer_
          )
        }
      }
    }

    # ---- Block 2: abiotic sounds ----
    # Binary mode evaluates only the target species, so abiotic sounds are
    # only persisted in full mode.
    if (mode != "binary" &&
        !is.null(input$abiotic_sounds) &&
        length(input$abiotic_sounds) > 0) {
      for (ab_id_str in input$abiotic_sounds) {
        ab_id <- as.integer(ab_id_str)
        if (!is.na(ab_id)) {
          inserts_to_do[[length(inserts_to_do) + 1]] <- list(
            sid   = NA_integer_,
            bid   = NA_integer_,
            cid   = 1L,
            ab_id = ab_id
          )
        }
      }
    }

    tryCatch({
      pool::poolWithTransaction(pool, function(conn) {

        # ---- DELETE old rows (mode-dependent) ----
        if (mode == "binary" && !is.null(target_species_val)) {
          # Replace only the target-species rows of this task. If the user
          # didn't tick the target species, only a status row remains:
          # "task reviewed, species not present".
          DBI::dbExecute(conn,
                         "DELETE FROM import.ground_truth_annotations
             WHERE audio_file_id = $1 AND user_id = $2 AND begin_time_ms = $3
               AND species_id = $4",
                         list(aid, res_auth$user_id, start_ms, target_species_val))

          DBI::dbExecute(conn,
                         "DELETE FROM import.annotation_status
             WHERE audio_file_id = $1 AND user_id = $2 AND begin_time_ms = $3
               AND target_species_id = $4",
                         list(aid, res_auth$user_id, start_ms, target_species_val))
        } else {
          # Full mode: replace the whole clip annotation for this user. This
          # is important so that removed species / behaviours / abiotic sounds
          # actually disappear from the DB.
          DBI::dbExecute(conn,
                         "DELETE FROM import.ground_truth_annotations
             WHERE audio_file_id = $1 AND user_id = $2 AND begin_time_ms = $3",
                         list(aid, res_auth$user_id, start_ms))

          if (!is.null(target_species_val)) {
            # Full + queue species: replace this queue's status row
            DBI::dbExecute(conn,
                           "DELETE FROM import.annotation_status
               WHERE audio_file_id = $1 AND user_id = $2 AND begin_time_ms = $3
                 AND target_species_id = $4",
                           list(aid, res_auth$user_id, start_ms, target_species_val))
          } else {
            # Free full annotation: replace the queue-less status row
            DBI::dbExecute(conn,
                           "DELETE FROM import.annotation_status
               WHERE audio_file_id = $1 AND user_id = $2 AND begin_time_ms = $3
                 AND target_species_id IS NULL",
                           list(aid, res_auth$user_id, start_ms))
          }
        }

        # ---- INSERT new ground-truth rows ----
        if (length(inserts_to_do) > 0) {
          for (item in inserts_to_do) {
            DBI::dbExecute(conn,
                           "INSERT INTO import.ground_truth_annotations
                 (audio_file_id, user_id, species_id, behavior_id, certainty_id,
                  begin_time_ms, end_time_ms, abiotic_sound_id, is_present)
               VALUES ($1, $2, $3, $4, $5, $6, $7, $8, TRUE)",
                           list(aid, res_auth$user_id, item$sid, item$bid, item$cid,
                                start_ms, end_ms, item$ab_id))
          }
        }

        # ---- INSERT annotation_status with queue/task species ----
        target_sid_sql <- if (!is.null(target_species_val)) target_species_val else NA_integer_
        DBI::dbExecute(conn,
                       "INSERT INTO import.annotation_status
             (audio_file_id, user_id, begin_time_ms, end_time_ms,
              annotation_type_id, target_species_id)
           VALUES ($1, $2, $3, $4, $5, $6)",
                       list(aid, res_auth$user_id, start_ms, end_ms,
                            fixed_type_id, target_sid_sql))
      })

      # ---- Determine the next file from the OLD list, before invalidating ----
      # Once we trigger save_trigger, filtered_files() will recompute and
      # potentially drop the just-saved spectrogram (and possibly its whole
      # group). Capture the natural successor first.
      old_choices <- filtered_files()
      curr_idx <- which(old_choices == input$seq)
      candidate_next <- if (length(curr_idx) > 0 && curr_idx < length(old_choices)) {
        old_choices[curr_idx + 1]
      } else NULL

      showNotification("Saved!", type = "message")

      # Invalidate DB-dependent reactives so counters and the queue refresh.
      save_trigger(save_trigger() + 1L)

      # ---- Occupancy auto-stop notification ----
      # Pop up only if a group hits exactly target_count BECAUSE of this save:
      # - the queue species was just ticked positive in this clip
      # - certainty is "certain"
      # - some group now contains this very clip as one of its certain hits
      #   AND has reached target_count
      if (occupancy_active()) {
        target_cert <- input[[paste0("cert_", target_species_val)]]
        current_target_positive <- !is.null(target_species_val) &&
          as.character(target_species_val) %in% selected_ids &&
          (is.null(target_cert) || as.integer(target_cert) == 1L)

        if (isTRUE(current_target_positive)) {
          q_just_done <- "
            SELECT og.group_name, og.target_count,
                   COUNT(DISTINCT (gt.audio_file_id, gt.begin_time_ms)) AS n_hits
            FROM import.occupancy_groups og
            JOIN import.spectrogram_groups sg USING (group_id)
            JOIN import.spectrograms s        ON s.spectrogram_id = sg.spectrogram_id
            LEFT JOIN import.annotation_status ast
              ON ast.audio_file_id     = s.audio_file_id
             AND ast.begin_time_ms     = s.begin_time_ms
             AND ast.target_species_id = $3
            LEFT JOIN import.ground_truth_annotations gt
              ON gt.audio_file_id      = ast.audio_file_id
             AND gt.begin_time_ms      = ast.begin_time_ms
             AND gt.user_id            = ast.user_id
             AND gt.species_id         = ast.target_species_id
             AND gt.certainty_id       = 1
             AND gt.is_present         = TRUE
            WHERE og.group_id IN (
              SELECT sg2.group_id
              FROM import.spectrogram_groups sg2
              JOIN import.spectrograms s2 USING (spectrogram_id)
              WHERE s2.audio_file_id = $1 AND s2.begin_time_ms = $2
            )
            GROUP BY og.group_id, og.group_name, og.target_count
            HAVING COUNT(DISTINCT (gt.audio_file_id, gt.begin_time_ms)) = og.target_count
               AND COUNT(DISTINCT (gt.audio_file_id, gt.begin_time_ms))
                     FILTER (WHERE gt.audio_file_id = $1 AND gt.begin_time_ms = $2) > 0
          "

          just_done <- tryCatch(
            DBI::dbGetQuery(pool, q_just_done,
                            params = list(aid, start_ms, target_species_val)),
            error = function(e) data.frame()
          )

          if (nrow(just_done) > 0) {
            for (i in seq_len(nrow(just_done))) {
              showNotification(
                sprintf("\U0001F389 Group '%s' completed (%d/%d certain hits) \u2014 remaining clips will be skipped.",
                        just_done$group_name[i],
                        as.integer(just_done$n_hits[i]),
                        as.integer(just_done$target_count[i])),
                type = "message", duration = 7
              )
            }
          }
        }
      }

      # ---- Navigate ----
      # Prefer the candidate_next captured before invalidation; if it's no
      # longer in the new list (e.g. its group just closed), jump to the
      # first remaining clip; if the list is empty, announce we're done.
      new_choices <- filtered_files()
      if (!is.null(candidate_next) && candidate_next %in% new_choices) {
        updateSelectizeInput(session, "seq", selected = candidate_next)
      } else if (length(new_choices) > 0) {
        updateSelectizeInput(session, "seq", selected = new_choices[1])
      } else {
        showNotification("All clips processed!", type = "warning")
      }

    }, error = function(e) {
      print("!!! DB ERROR !!!")
      print(e)
      showNotification(paste("Save error:", e$message),
                       type = "error", duration = NULL)
    })
  })


  output$user_info <- renderUI({
    h4(paste("User:", res_auth$first_name))
  })


  # ---- Model check modal ----
  observeEvent(input$model_check_btn, {
    req(species_list(), input$species_lang)

    df <- species_list()
    labels <- switch(input$species_lang,
                     "de"  = df$species_long_de,
                     "en"  = df$species_long_en,
                     "sci" = df$species_scientific)
    labels[is.na(labels) | labels == ""] <-
      as.character(df$species_id[is.na(labels) | labels == ""])
    choices <- setNames(as.character(df$species_id), labels)
    choices <- choices[order(names(choices))]

    showModal(modalDialog(
      title = "Model check: BirdNET score \u2192 true positive",
      size = "l", easyClose = TRUE,
      fluidRow(
        column(6, selectInput("mc_species", "Choose species:", choices = choices)),
        column(6, br(), actionButton("mc_run", "Compute",
                                     icon = icon("calculator"),
                                     style = "background-color: #337ab7; color: white; margin-top: 5px;"))
      ),
      hr(),
      plotOutput("mc_plot", height = "450px"),
      footer = modalButton("Close")
    ))
  })

  # Compute only when the modal's Run button is clicked.
  observeEvent(input$mc_run, {
    req(input$mc_species, input$selected_project)

    sp_id <- as.integer(input$mc_species)
    df_sp <- species_list()

    sp_row <- df_sp[df_sp$species_id == sp_id, ]
    sp_name <- if (nrow(sp_row) > 0) {
      switch(input$species_lang,
             "de"  = sp_row$species_long_de,
             "en"  = sp_row$species_long_en,
             "sci" = sp_row$species_scientific)
    } else paste("Species", sp_id)

    glm_df <- get_glm_data(pool, as.integer(input$selected_project), sp_id)

    output$mc_plot <- renderPlot({
      if (nrow(glm_df) == 0) {
        plot.new()
        text(0.5, 0.5, "No annotated data for this species.",
             cex = 1.4, col = "red")
      } else {
        plot_glm_check(glm_df, species_name = sp_name)
      }
    })
  })

}
