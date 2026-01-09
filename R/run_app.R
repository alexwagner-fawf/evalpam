#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @importFrom pool poolClose
run_app <- function(
    onStart = NULL,
    options = list(),
    enableBookmarking = NULL,
    uiPattern = "/",
    ...
) {

  # 1. Datenbank-Pool erstellen (Verbindung öffnen)
  pool <- evalpam:::set_db_pool()

  # 2. HIER NEU: Daten initial laden
  # Das füllt options(evalpam.data = ...), damit die UI Zugriff darauf hat.
  # Voraussetzung: Du hast die Funktion 'data_setup_from_db' irgendwo definiert (z.B. in R/fct_data_setup.R).
  evalpam:::data_setup(pool)

  # 3. Cleanup: Verbindung schließen, wenn App stoppt
  onStop(function() {
    pool::poolClose(pool)
  })

  with_golem_options(
    app = shinyApp(
      # UI mit Login-Schutz
      ui = shinymanager::secure_app(app_ui()),

      # Server: Hier reichen wir den Pool weiter
      server = function(input, output, session) {
        # Damit das funktioniert, muss deine app_server Funktion so definiert sein:
        # app_server <- function(input, output, session, pool) { ... }
        app_server(input, output, session, pool = pool)
      },

      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
