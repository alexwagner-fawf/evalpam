#' set_db_pool
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
set_db_pool <- function() {
  pool::dbPool(RPostgres::Postgres(),
               user = get_golem_config("pg_user"),
               host = get_golem_config("pg_host"),
               port = get_golem_config("pg_port"),
               dbname = get_golem_config("pg_dbname"),
               password = Sys.getenv("evalpam_pw")
               )
}

