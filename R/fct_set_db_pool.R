#' set_db_pool
#'
#' @description Establish a pool connection using credentials or from golem-config.yml. Password is currently stored in the .Renviron file.
#'
#' @param user character, user name for postgres connection, if NULL it will read from golem-config yml description
#' @param host character, host for postgres connection, if NULL it will read from golem-config yml description
#' @param port integer, port for postgres connection, if NULL it will read from golem-config yml description
#' @param dbname character, dbname for postgres connection, if NULL it will read from golem-config yml description
#' @param password character, password name for postgres connection, if NULL it will read from .Renviron (with obfuscation)
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
                 password = ifelse(is.null(password),
                                   rawToChar(base64enc::base64decode(Sys.getenv("evalpam_pw"))),
                                             password)
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

