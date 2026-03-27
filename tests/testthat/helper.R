# tests/testthat/helper.R
get_test_pool <- function() {
  skip_if_not(nzchar(Sys.getenv("evalpam_pw")), "DB password not set")
  tryCatch(
    set_db_pool(fail_with_error = TRUE),
    error = function(e) skip(paste("DB not available:", e$message))
  )
}

with_test_transaction <- function(pool) {
  con <- pool::poolCheckout(pool)
  DBI::dbBegin(con)
  withr::defer({
    DBI::dbRollback(con)
    pool::poolReturn(con)
  }, envir = parent.frame())
  con
}
