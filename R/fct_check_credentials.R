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

      pg_role <- DBI::dbGetQuery(
        pool,
        "SELECT pg_role FROM app_user_roles WHERE pameval_user = $1",
        params = list(user)
      ) |>
        dplyr::pull(pg_role)

      if(length(pg_role) == 0 || is.null(pg_role)) {
        return(list(result = FALSE, message = "No role assigned"))
      } else {
        # SUCCESS
        return(list(
          result = TRUE,
          user_info = data.frame(user = user,
                                 admin = isTRUE(pg_role == "evalpam_admin"),
                                 pg_role = as.character(pg_role))
        ))
      }

    } else {
      return(list(result = FALSE, user = user))
    }
  }
}
