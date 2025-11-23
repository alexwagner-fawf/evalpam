#' set_db_pool
#'
#' @description Establish a pool connection using credentials from golem-config.yml. Password is currently stored in the .Renviron file.
#'
#' @return A pool connection, FALSE if connection did not work. In running shiny session, a shinyalert will appear.
#'
#' @noRd
set_db_pool <- function() {
  tryCatch({
    pool::dbPool(RPostgres::Postgres(),
                 user = get_golem_config("pg_user"),
                 host = get_golem_config("pg_host"),
                 port = get_golem_config("pg_port"),
                 dbname = get_golem_config("pg_dbname"),
                 password = rawToChar(base64enc::base64decode(Sys.getenv("evalpam_pw")))
    )
  },
  error = function(e){
    if(isRunning()){
      shinyalert::shinyalert("No database connection possible!",
                             text = "Is the Server running and are credentials in golem-config.yml correct?",
                             type = "error")
    }
    message("Database connection failed, check credentials for evalpam_user")
    return(FALSE)
  })
}

