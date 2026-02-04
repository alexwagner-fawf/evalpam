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


  # 3. Cleanup: Verbindung schließen, wenn App stoppt
  onStop(function() {
    pool::poolClose(pool)
  })

  with_golem_options(
    app = shinyApp(
      # UI mit Login-Schutz
      ui = shinymanager::secure_app(app_ui()),
      server = function(input, output, session) {

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
