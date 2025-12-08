# ============================================================
# User Management Functions for app_users / app_user_roles
# ============================================================

#' Add a User to the Database
#'
#' Creates a new entry in `app_users` and assigns at least one role in
#' `app_user_roles`. Ensures that:
#' - username is unique
#' - password meets minimum length requirements
#' - PostgreSQL role exists
#' - created_at is set automatically by the database
#'
#' @param pool A DBI connection pool.
#' @param username Character. Unique username for the new user.
#' @param password Character. Plain-text password (hashed automatically).
#' @param pg_role Character. PostgreSQL role to assign.
#' @param first_name Optional character.
#' @param last_name Optional character.
#' @param email Optional character. Stored case-insensitively (CITEXT).
#' @param expire_date Optional Date. When user account expires.
#' @param active Logical. Whether the account is active (default TRUE).
#'
#' @return Invisible list containing success status and metadata.
#'
#' @export
add_users <- function(pool, username, password, pg_role,
                      first_name = NULL,
                      last_name = NULL,
                      email = NULL,
                      expire_date = NULL,
                      active = TRUE) {

  # ---- Validation ----
  if (is.null(username) || username == "")
    stop("username cannot be empty")

  if (is.null(password) || nchar(password) < 8)
    stop("password must be at least 8 characters long")

  if (is.null(pg_role) || pg_role == "")
    stop("pg_role cannot be empty")

  # ---- Role Exists? ----
  role_exists <- DBI::dbGetQuery(
    pool,
    "SELECT COUNT(*) AS count FROM pg_roles WHERE rolname = $1",
    params = list(pg_role)
  )$count > 0

  if (!role_exists)
    stop("PostgreSQL role '", pg_role, "' does not exist.")

  # ---- Username Exists? ----
  user_exists <- DBI::dbGetQuery(
    pool,
    "SELECT COUNT(*) AS count FROM app_users WHERE username = $1",
    params = list(username)
  )$count > 0

  if (user_exists)
    stop("User '", username, "' already exists.")

  # ---- Password Hashing ----
  hashed_password <- bcrypt::hashpw(password)

  # ---- Insert Transaction ----
  tryCatch({
    pool::poolWithTransaction(pool, function(conn) {

      # Insert new user (created_at handled by DB)
      DBI::dbExecute(
        conn,
        "INSERT INTO app_users
           (username, password_hash, active, first_name, last_name, email, expire_date)
         VALUES ($1, $2, $3, $4, $5, $6, $7)",
        params = list(
          username, hashed_password, active,
          first_name, last_name, email, expire_date
        )
      )

      # Fetch user_id
      user_id <- DBI::dbGetQuery(
        conn,
        "SELECT user_id FROM app_users WHERE username = $1",
        params = list(username)
      )$user_id

      # Assign initial role
      DBI::dbExecute(
        conn,
        "INSERT INTO app_user_roles (user_id, pg_role)
         VALUES ($1, $2)",
        params = list(user_id, pg_role)
      )
    })

    invisible(list(
      success = TRUE,
      user = username,
      role = pg_role
    ))

  }, error = function(e) {
    stop("Failed to create user: ", e$message)
  })
}



#' Update an Existing User
#'
#' Allows updating of:
#' - password (rehashes automatically)
#' - active status
#' - first/last name
#' - email
#' - expire_date
#' - roles (adds new role; does not remove existing roles)
#'
#' @param pool A DBI connection pool.
#' @param username Character. Username whose record is updated.
#' @param password Optional character. New password.
#' @param pg_role Optional character. New PostgreSQL role to add.
#' @param active Optional logical.
#' @param first_name Optional character.
#' @param last_name Optional character.
#' @param email Optional character.
#' @param expire_date Optional Date.
#'
#' @return Invisible list with success status.
#'
#' @export
update_user <- function(pool, username,
                        password = NULL,
                        pg_role = NULL,
                        active = NULL,
                        first_name = NULL,
                        last_name = NULL,
                        email = NULL,
                        expire_date = NULL) {

  # ---- Fetch user_id ----
  user_row <- DBI::dbGetQuery(
    pool,
    "SELECT user_id FROM app_users WHERE username = $1",
    params = list(username)
  )

  if (nrow(user_row) == 0)
    stop("User '", username, "' does not exist.")

  user_id <- user_row$user_id

  # ---- Transaction ----
  tryCatch({
    pool::poolWithTransaction(pool, function(conn) {

      # Password update
      if (!is.null(password)) {
        if (nchar(password) < 8)
          stop("Password must be at least 8 characters long")
        hashed_password <- bcrypt::hashpw(password)

        DBI::dbExecute(
          conn,
          "UPDATE app_users SET password_hash = $1 WHERE user_id = $2",
          params = list(hashed_password, user_id)
        )
      }

      # Active status
      if (!is.null(active)) {
        DBI::dbExecute(
          conn,
          "UPDATE app_users SET active = $1 WHERE user_id = $2",
          params = list(active, user_id)
        )
      }

      # Personal info
      if (!is.null(first_name))
        DBI::dbExecute(conn,
                       "UPDATE app_users SET first_name = $1 WHERE user_id = $2",
                       params = list(first_name, user_id))

      if (!is.null(last_name))
        DBI::dbExecute(conn,
                       "UPDATE app_users SET last_name = $1 WHERE user_id = $2",
                       params = list(last_name, user_id))

      if (!is.null(email))
        DBI::dbExecute(conn,
                       "UPDATE app_users SET email = $1 WHERE user_id = $2",
                       params = list(email, user_id))

      if (!is.null(expire_date))
        DBI::dbExecute(conn,
                       "UPDATE app_users SET expire_date = $1 WHERE user_id = $2",
                       params = list(expire_date, user_id))

      # Role update
      if (!is.null(pg_role)) {

        # Verify role exists
        role_exists <- DBI::dbGetQuery(
          conn,
          "SELECT COUNT(*) AS count FROM pg_roles WHERE rolname = $1",
          params = list(pg_role)
        )$count > 0

        if (!role_exists)
          stop("PostgreSQL role '", pg_role, "' does not exist.")

        # Add new role (does not overwrite)
        DBI::dbExecute(
          conn,
          "INSERT INTO app_user_roles (user_id, pg_role)
           VALUES ($1, $2)
           ON CONFLICT DO NOTHING",
          params = list(user_id, pg_role)
        )
      }
    })

    invisible(list(success = TRUE, user = username))

  }, error = function(e) {
    stop("Failed to update user: ", e$message)
  })
}



#' Delete a User
#'
#' Removes the user from `app_users`.
#' All associated roles in `app_user_roles` are deleted automatically due to
#' `ON DELETE CASCADE`.
#'
#' @param pool A DBI connection pool.
#' @param username Character. Username to delete.
#' @param confirm Logical. Must be TRUE to proceed.
#'
#' @return Invisible list with deletion status.
#'
#' @export
delete_user <- function(pool, username, confirm = FALSE) {

  if (!confirm)
    stop("Set confirm = TRUE to delete user '", username, "'.")

  # Fetch user_id
  user_row <- DBI::dbGetQuery(
    pool,
    "SELECT user_id FROM app_users WHERE username = $1",
    params = list(username)
  )

  if (nrow(user_row) == 0)
    return(invisible(list(success = FALSE, message = "User not found")))

  user_id <- user_row$user_id

  tryCatch({
    pool::poolWithTransaction(pool, function(conn) {
      # CASCADE will delete roles automatically
      DBI::dbExecute(
        conn,
        "DELETE FROM app_users WHERE user_id = $1",
        params = list(user_id)
      )
    })

    invisible(list(success = TRUE, user = username))

  }, error = function(e) {
    stop("Failed to delete user: ", e$message)
  })
}



#' List All Users With Roles
#'
#' Returns a joined table of:
#' - usernames
#' - names
#' - email
#' - active flag
#' - expire_date
#' - created_at timestamp
#' - assigned PostgreSQL roles
#'
#' @param pool A DBI connection pool.
#'
#' @return A data frame of all users and their roles.
#'
#' @export
list_users <- function(pool) {
  tryCatch({
    DBI::dbGetQuery(pool, "
      SELECT
        u.user_id,
        u.username,
        u.first_name,
        u.last_name,
        u.email,
        u.active,
        u.expire_date,
        u.created_at,
        r.pg_role
      FROM app_users u
      LEFT JOIN app_user_roles r ON u.user_id = r.user_id
      ORDER BY u.username
    ")
  }, error = function(e) {
    stop("Failed to list users: ", e$message)
  })
}
