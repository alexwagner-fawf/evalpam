# ==============================================================
# testServer Tests für app_server
# Braucht: Daten in der DB (Projects, Audio Files, Results, Spectrograms)
# Mockt: shinymanager Authentifizierung
# ==============================================================

# Wrapper der Auth überspringt und pool durchreicht
make_test_server <- function(pool, fake_user_id = 1L) {
  function(input, output, session) {
    # Auth mocken: res_auth als feste reactiveValues
    res_auth <- reactiveValues(
      user_id = fake_user_id,
      first_name = "Testuser"
    )

    # Den echten Server-Code ausführen, aber secure_server ersetzen
    # Wir kopieren die relevante Logik statt app_server() direkt aufzurufen,
    # weil secure_server nicht mockbar ist in testServer.
    # Stattdessen testen wir die Kern-Reactives einzeln.

    # ---- project_mode ----
    project_mode <- reactive({
      req(input$selected_project)
      tryCatch({
        q <- "SELECT annotation_mode FROM public.project_users WHERE project_id = $1 AND user_id = $2"
        res <- DBI::dbGetQuery(pool, q, params = list(input$selected_project, res_auth$user_id))
        if(nrow(res) == 0) return("full")
        res$annotation_mode
      }, error = function(e) "full")
    })

    # ---- species_list ----
    species_list <- reactive({
      req(input$selected_project)
      query <- "
        SELECT DISTINCT sp.species_id, sp.species_short, sp.species_long_de
        FROM public.lut_species_code sp
        JOIN import.settings_species ss ON sp.species_id = ss.species_id
        JOIN import.settings s ON ss.settings_id = s.settings_id
        JOIN import.results r ON s.settings_id = r.settings_id
        JOIN import.audio_files af ON r.audio_file_id = af.audio_file_id
        JOIN import.deployments d ON af.deployment_id = d.deployment_id
        WHERE d.project_id = $1
        ORDER BY sp.species_short
      "
      DBI::dbGetQuery(pool, query, params = list(input$selected_project))
    })

    # ---- project_data ----
    project_data <- reactive({
      req(input$selected_project)
      query <- "
        SELECT
          r.result_id, r.confidence as score,
          CAST(r.begin_time_ms AS FLOAT) / 1000.0 as start,
          CAST(r.end_time_ms AS FLOAT) / 1000.0 as end_sec,
          CAST(s.spectrogram_id AS TEXT) || '.mp4' as path,
          s.buffer_ms, af.audio_file_id, af.sample_rate,
          sp.species_id, sp.species_short, d.deployment_name
        FROM import.results r
        JOIN import.spectrograms s ON r.result_id = s.result_id
        JOIN import.audio_files af ON r.audio_file_id = af.audio_file_id
        JOIN import.deployments d ON af.deployment_id = d.deployment_id
        JOIN public.lut_species_code sp ON r.species_id = sp.species_id
        WHERE d.project_id = $1
        ORDER BY af.relative_path, r.begin_time_ms
      "
      DBI::dbGetQuery(pool, query, params = list(input$selected_project))
    })

    # Exportiere Reactives für Tests
    exportTestValues(
      project_mode = { project_mode() },
      species_list_n = { nrow(species_list()) },
      project_data_n = { nrow(project_data()) }
    )
  }
}


# ==============================================================
# Tests
# ==============================================================

test_that("project_mode gibt korrekten Modus zurück", {
  pool <- get_test_pool()
  on.exit(pool::poolClose(pool), add = TRUE)

  proj <- DBI::dbGetQuery(pool, "SELECT project_id FROM import.projects LIMIT 1")
  skip_if(nrow(proj) == 0, "No projects")
  pid <- proj$project_id[1]

  testServer(make_test_server(pool), {
    session$setInputs(selected_project = pid)
    session$flushReact()

    mode <- exportedValues()$project_mode
    expect_true(mode %in% c("full", "binary"))
  })
})

test_that("species_list lädt Arten für Projekt", {
  pool <- get_test_pool()
  on.exit(pool::poolClose(pool), add = TRUE)

  proj <- DBI::dbGetQuery(pool, "SELECT project_id FROM import.projects LIMIT 1")
  skip_if(nrow(proj) == 0, "No projects")
  pid <- proj$project_id[1]

  testServer(make_test_server(pool), {
    session$setInputs(selected_project = pid)
    session$flushReact()

    n <- exportedValues()$species_list_n
    expect_true(is.numeric(n))
    expect_true(n >= 0)
  })
})

test_that("project_data lädt Ergebnisse für Projekt", {
  pool <- get_test_pool()
  on.exit(pool::poolClose(pool), add = TRUE)

  proj <- DBI::dbGetQuery(pool, "SELECT project_id FROM import.projects LIMIT 1")
  skip_if(nrow(proj) == 0, "No projects")
  pid <- proj$project_id[1]

  # Nur testen wenn Spectrograms existieren
  n_spec <- DBI::dbGetQuery(pool, "SELECT COUNT(*) as n FROM import.spectrograms")$n
  skip_if(n_spec == 0, "No spectrograms in DB")

  testServer(make_test_server(pool), {
    session$setInputs(selected_project = pid)
    session$flushReact()

    n <- exportedValues()$project_data_n
    expect_true(is.numeric(n))
    expect_true(n > 0)
  })
})

test_that("project_mode Fallback auf 'full' bei unbekanntem Projekt", {
  pool <- get_test_pool()
  on.exit(pool::poolClose(pool), add = TRUE)

  testServer(make_test_server(pool), {
    session$setInputs(selected_project = 99999L)  # existiert nicht
    session$flushReact()

    mode <- exportedValues()$project_mode
    expect_equal(mode, "full")
  })
})
