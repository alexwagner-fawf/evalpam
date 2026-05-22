# Try the OS keychain first; fall back to the legacy base64 .Renviron entry
# so that existing deployments keep working until setup_app() is re-run.
.resolve_db_password <- function(pg_user) {
  pw <- tryCatch(
    keyring::key_get("evalpam", pg_user),
    error = function(e) NULL
  )
  if (!is.null(pw)) return(pw)

  legacy <- Sys.getenv("evalpam_pw", unset = "")
  if (nzchar(legacy)) {
    warning(
      "Database password read from legacy .Renviron entry (base64). ",
      "Re-run setup_app() to migrate to the OS keychain.",
      call. = FALSE
    )
    return(rawToChar(base64enc::base64decode(legacy)))
  }

  stop(
    "No database password found in the OS keychain or .Renviron.\n",
    "Run setup_app() to store the password securely.\n",
    "On headless Linux servers, set KEYRING_BACKEND=file and ",
    "KEYRING_FILE_PASSWORD before calling setup_app()."
  )
}


#' set_db_pool
#'
#' @description Establish a pool connection using credentials or from golem-config.yml. Password is retrieved from the OS keychain via keyring.
#'
#' @param user character, user name for postgres connection, if NULL it will read from golem-config yml description
#' @param host character, host for postgres connection, if NULL it will read from golem-config yml description
#' @param port integer, port for postgres connection, if NULL it will read from golem-config yml description
#' @param dbname character, dbname for postgres connection, if NULL it will read from golem-config yml description
#' @param password character, password for postgres connection, if NULL it is read from the OS keychain via keyring
#'
#' @return A pool connection, FALSE if connection did not work. In running shiny session, a shinyalert will appear.
#'
#' @noRd
#' @export
set_db_pool <- function(user = NULL,
                        host = NULL,
                        port = NULL,
                        dbname = NULL,
                        password = NULL,
                        fail_with_error = FALSE) {
  tryCatch({
    pool::dbPool(RPostgres::Postgres(),
                 user = ifelse(is.null(user), get_golem_config("pg_user"), user),
                 host = ifelse(is.null(host), get_golem_config("pg_host"), host),
                 port = ifelse(is.null(port), get_golem_config("pg_port"), port),
                 dbname = ifelse(is.null(dbname), get_golem_config("pg_dbname"), dbname),
                 password = if (is.null(password)) {
                   .resolve_db_password(get_golem_config("pg_user"))
                 } else {
                   password
                 }
    )
  },
  error = function(e){
    if(isRunning()){


      text <- ifelse(golem::app_dev(),
                     paste0("Is the Server running and are credentials correct?
                             If you did not set any credentials, check golem-config.yml",
                            system.file("golem-config.yml", package = "evalpam")),
                     paste0("Could not connect to the server, please contact your admin: ",
                            get_golem_config("admin_mailaddress")))

      shinyalert::shinyalert("No database connection possible!",
                             text = text,
                             type = "error")
    }
    message(paste0("Database connection failed, check credentials for selected user"))

    if(fail_with_error) stop("Database connection invalid, check credentials")
    return(FALSE)
  })
}

