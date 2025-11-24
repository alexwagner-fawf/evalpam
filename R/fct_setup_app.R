#' setup_db
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
setup_app <- function(user = "postgres",
                     host = "localhost",
                     port = 5432,
                     maintenance_dbname = "postgres",
                     password = "postgres",
                     evalpam_username = "evalpam_user",
                     evalpam_pw = "new_evalpam_pw",
                     evalpam_dbname = "evalpam_db",
                     admin_mailaddress = "emailaddress_required",
                     setup_db = FALSE,
                     renviron_dir = NULL) {

  pool <- set_db_pool(user = user,
                      host = host,
                      port = port,
                      dbname = maintenance_dbname,
                      password = password)


  on.exit({
    pool::poolClose(pool)
  })

  if(!"Pool" %in% S3Class(pool)){
    stop("Database connection invalid, check credentials")
  }

  if(is.null(evalpam_pw)){
    stop("provide password for postgres evalpam user")
  }

  # setup golem config and Renviron for database connection

    golem::amend_golem_config("pg_host", host)
    golem::amend_golem_config("pg_port", port)
    golem::amend_golem_config("pg_dbname", evalpam_dbname)
    golem::amend_golem_config("pg_user", evalpam_username)
    golem::amend_golem_config("admin_mailaddress", admin_mailaddress)

    if(is.null(renviron_dir)) renviron_dir <- here::here()

    setup_renviron(filepath = paste0(renviron_dir, "/.Renviron"),
                   overwrite = TRUE,
                   evalpam_pw = evalpam_pw)

  if(setup_db){
    setup_db(pool)
  }

  # test connection with config
  pool::poolClose(pool)
  pool <- set_db_pool(fail_with_error = TRUE)


}



setup_db <- function(pool){
  on.exit({
    pool::poolClose(pool)
  })

  sql_dir <- app_sys("sql")

  sql_files <- list.files(
    sql_dir,
    pattern = "\\.sql$",
    full.names = TRUE
  ) |>
    sort()

  if (sql_dir == "") {
    stop("No SQL directory found in package. Expected /inst/sql.")
  }

  for(sql_file in sql_files){
    message("============================")
    message("Running: ", basename(sql_file))
    sql <- readLines(sql_file, warn = FALSE) |>
      paste(collapse = "\n")


    statements <- unlist(strsplit(sql, ";"))

    for(statement in statements){
      statement <- glue::glue_sql(statement,
                                  DB_PASSWORD_EVALPAM_USER = rawToChar(base64enc::base64decode(Sys.getenv("evalpam_pw"))),
                                  DB_EVALPAM_USER = get_golem_config("pg_user"),
                                  .con = pool)

      tryCatch({
        DBI::dbExecute(pool, statement)
      },
      error = function(e){
        print(e)
        message(paste("Statement skipped: Error when running", statement))
      })
    }

    if(basename(sql_file) == "00_create_db.sql"){
      pool::poolClose(pool)
      pool <- set_db_pool(user = user,
                          host = host,
                          port = port,
                          dbname = evalpam_dbname,
                          password = password)
    }
  }
}
