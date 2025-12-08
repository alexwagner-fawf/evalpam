#' check_credentials_db
#'
#' @description Validate user credentials using the optimized schema
#' @param pool a valid pool connection
#'
#' @noRd
check_credentials_db <- function(pool) {
  function(user, password) {

    # 1. Fetch user_id + password hash
    row <- DBI::dbGetQuery(
      pool,
      "SELECT user_id, password_hash, first_name, user_id, expire_date
       FROM app_users
       WHERE username = $1 AND active = TRUE",
      params = list(user)
    )

    # User not found or inactive
    if (nrow(row) != 1 || !bcrypt::checkpw(password, row$password_hash)) {
      return(list(result = FALSE, user = user))
    }

    if(!is.na(row$expire_date)){
      if(row$expire_date <= Sys.Date()){
        if(isRunning()) shinyalert::shinyalert(title = "Expired User", text = "Please contact your admin")
        message("expired user login")
        return(list(result = FALSE, user = user))
      }
    }

    user_id <- row$user_id

    pg_roles <- DBI::dbGetQuery(
      pool,
      "SELECT pg_role
       FROM app_user_roles
       WHERE user_id = $1",
      params = list(user_id)
    ) |>
      dplyr::pull(pg_role)

    if (length(pg_roles) == 0 || is.null(pg_roles)) {
      return(list(result = FALSE, message = "No role assigned"))
    }

    return(list(
      result = TRUE,
      user_info = data.frame(
        user_id = user_id,
        user = user,
        first_name = row$first_name,
        admin = "evalpam_admin" %in% pg_roles,
        pg_role = as.character(pg_roles)
      )
    ))
  }
}
