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

  # Fail fast with an actionable message. Without this, a failed connection
  # (pool == FALSE) is passed down silently and every login attempt dies with
  # the opaque S4 error "unable to find an inherited method for 'dbGetQuery'
  # ... signature 'logical'". The most common cause on a headless shiny-server
  # is that the DB password is not reachable from the app process: keyring
  # falls back to the 'env' backend and su/--login strips the environment, so
  # neither the keychain nor a project-local .Renviron is read. Store the
  # password in the *run_as* user's ~/.Renviron (evalpam_pw=<base64>) or set
  # KEYRING_BACKEND=file + KEYRING_FILE_PASSWORD there so R reads it at startup.
  if (!isTRUE(tryCatch(pool::dbIsValid(pool), error = function(e) FALSE))) {
    stop(
      "evalpam: could not establish the database pool at startup. ",
      "Check that the DB server is reachable and that the password is available ",
      "to this process (OS keychain via keyring, or evalpam_pw in the run_as ",
      "user's ~/.Renviron). See the log line 'Database connection failed, ",
      "check credentials for selected user' above.",
      call. = FALSE
    )
  }


  # 3. Cleanup: Verbindung schließen, wenn App stoppt
  onStop(function() {
    pool::poolClose(pool)
  })

  with_golem_options(
    app = shinyApp(
      # UI mit Login-Schutz und custom CSS
      ui = shinymanager::secure_app(
        app_ui(),
        head_auth = tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css"))
      ),
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
