#' check_credentials
#'
#' @description A fct function
#' @param pool a valid pool connection
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
check_credentials_db <- function(pool) {
  function(user, password) {

    row <- DBI::dbGetQuery(
      pool,
      "SELECT password_hash
       FROM app_users
       WHERE pameval_user = $1 AND active = TRUE",
      params = list(user)
    )

    if (nrow(row) == 1 && bcrypt::checkpw(password, row$password_hash)) {
      return(list(result = TRUE, user = user))
    }else{
      return(list(result = FALSE, user = user))
    }

  }
}
