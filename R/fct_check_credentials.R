#' check_credentials_db
#'
#' @description Validate user credentials using the optimized schema
#' @param pool a valid pool connection
#'
#' @noRd
check_credentials_db <- function(pool) {
  function(user, password) {
    tryCatch(
      .check_credentials_impl(pool, user, password),
      error = function(e) {
        message("[evalpam auth] unexpected error for user '", user, "': ", conditionMessage(e))
        list(result = FALSE, user = user)
      }
    )
  }
}

.check_credentials_impl <- function(pool, user, password) {

  # 1. Fetch user row — select user_id once (was duplicated, causing ambiguity)
  row <- DBI::dbGetQuery(
    pool,
    "SELECT user_id, password_hash, first_name, expire_date
     FROM app_users
     WHERE username = $1 AND active = TRUE",
    params = list(user)
  )

  if (nrow(row) != 1) {
    message("[evalpam auth] user not found or inactive: '", user, "'")
    return(list(result = FALSE, user = user))
  }

  if (!bcrypt::checkpw(password, row$password_hash)) {
    message("[evalpam auth] wrong password for user: '", user, "'")
    return(list(result = FALSE, user = user))
  }

  if (!is.na(row$expire_date) && row$expire_date <= Sys.Date()) {
    message("[evalpam auth] account expired: '", user, "'")
    if (isRunning())
      shinyalert::shinyalert(title = "Expired User", text = "Please contact your admin")
    return(list(result = FALSE, user = user))
  }

  user_id <- row$user_id[[1L]]

  pg_roles <- DBI::dbGetQuery(
    pool,
    "SELECT pg_role FROM app_user_roles WHERE user_id = $1",
    params = list(user_id)
  ) |>
    dplyr::pull(pg_role)

  if (length(pg_roles) == 0L) {
    message("[evalpam auth] no role assigned for user: '", user, "'")
    return(list(result = FALSE, user = user))
  }

  message("[evalpam auth] login OK: '", user, "' roles=[", paste(pg_roles, collapse = ","), "]")

  list(
    result    = TRUE,
    user_info = data.frame(
      user_id    = user_id,
      user       = user,
      first_name = row$first_name,
      admin      = "evalpam_admin" %in% pg_roles,
      pg_role    = pg_roles[[1L]]        # shinymanager expects a single-row data.frame
    )
  )
}
