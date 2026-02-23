# ============================================================
# User Management Functions for app_users / app_user_roles
# ============================================================

# Helper to convert an integer vector to a Postgres array literal: "{1,2,3}"
to_pg_int_array <- function(ids) {
  paste0("{", paste(ids, collapse = ","), "}")
}


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
#' @param project_ids Optional integer vector. Project IDs to assign to the user.
#'
#' @return Invisible list containing success status and metadata.
#'
#' @export
add_users <- function(pool, username,
                      password,
                      pg_role,
                      first_name  = NULL,
                      last_name   = NULL,
                      email       = NULL,
                      expire_date = NULL,
                      active      = TRUE,
                      project_ids = NULL) {

  # ---- Validation ----
  if (is.null(username) || username == "")
    stop("username cannot be empty")

  if (is.null(password) || nchar(password) < 8)
    stop("password must be at least 8 characters long")

  if (is.null(pg_role) || pg_role == "")
    stop("pg_role cannot be empty")

  if (!is.null(project_ids)) {
    project_ids <- as.integer(project_ids)
    if (any(is.na(project_ids)))
      stop("project_ids must be coercible to integers with no NAs")
  }

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

  # ---- Project IDs Exist? ----
  if (!is.null(project_ids) && length(project_ids) > 0) {
    found_projects <- DBI::dbGetQuery(
      pool,
      "SELECT project_id FROM import.projects WHERE project_id = ANY($1::int[])",
      params = list(to_pg_int_array(project_ids))
    )$project_id

    missing <- setdiff(project_ids, found_projects)
    if (length(missing) > 0)
      stop("The following project_ids do not exist: ", paste(missing, collapse = ", "))
  }

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

      # Assign projects
      if (!is.null(project_ids) && length(project_ids) > 0) {
        for (pid in project_ids) {
          DBI::dbExecute(
            conn,
            "INSERT INTO public.project_users (project_id, user_id)
             VALUES ($1, $2)
             ON CONFLICT DO NOTHING",
            params = list(pid, user_id)
          )
        }
      }
    })

    invisible(list(
      success  = TRUE,
      user     = username,
      role     = pg_role,
      projects = project_ids
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
#' - projects (add and/or remove project assignments)
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
#' @param add_project_ids Optional integer vector. Project IDs to assign to the user.
#' @param remove_project_ids Optional integer vector. Project IDs to remove from the user.
#'
#' @return Invisible list with success status.
#'
#' @export
update_user <- function(pool, username,
                        password           = NULL,
                        pg_role            = NULL,
                        active             = NULL,
                        first_name         = NULL,
                        last_name          = NULL,
                        email              = NULL,
                        expire_date        = NULL,
                        add_project_ids    = NULL,
                        remove_project_ids = NULL) {

  # ---- Coerce & validate project_ids ----
  if (!is.null(add_project_ids)) {
    add_project_ids <- as.integer(add_project_ids)
    if (any(is.na(add_project_ids)))
      stop("add_project_ids must be coercible to integers with no NAs")
  }

  if (!is.null(remove_project_ids)) {
    remove_project_ids <- as.integer(remove_project_ids)
    if (any(is.na(remove_project_ids)))
      stop("remove_project_ids must be coercible to integers with no NAs")
  }

  # ---- Fetch user_id ----
  user_row <- DBI::dbGetQuery(
    pool,
    "SELECT user_id FROM app_users WHERE username = $1",
    params = list(username)
  )

  if (nrow(user_row) == 0)
    stop("User '", username, "' does not exist.")

  user_id <- user_row$user_id

  # ---- Validate add_project_ids exist ----
  if (!is.null(add_project_ids) && length(add_project_ids) > 0) {
    found_projects <- DBI::dbGetQuery(
      pool,
      "SELECT project_id FROM import.projects WHERE project_id = ANY($1::int[])",
      params = list(to_pg_int_array(add_project_ids))
    )$project_id

    missing <- setdiff(add_project_ids, found_projects)
    if (length(missing) > 0)
      stop("The following project_ids do not exist: ", paste(missing, collapse = ", "))
  }

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

        role_exists <- DBI::dbGetQuery(
          conn,
          "SELECT COUNT(*) AS count FROM pg_roles WHERE rolname = $1",
          params = list(pg_role)
        )$count > 0

        if (!role_exists)
          stop("PostgreSQL role '", pg_role, "' does not exist.")

        DBI::dbExecute(
          conn,
          "INSERT INTO app_user_roles (user_id, pg_role)
           VALUES ($1, $2)
           ON CONFLICT DO NOTHING",
          params = list(user_id, pg_role)
        )
      }

      # Add projects
      if (!is.null(add_project_ids) && length(add_project_ids) > 0) {
        for (pid in add_project_ids) {
          DBI::dbExecute(
            conn,
            "INSERT INTO public.project_users (project_id, user_id)
             VALUES ($1, $2)
             ON CONFLICT DO NOTHING",
            params = list(pid, user_id)
          )
        }
      }

      # Remove projects
      if (!is.null(remove_project_ids) && length(remove_project_ids) > 0) {
        DBI::dbExecute(
          conn,
          "DELETE FROM public.project_users
           WHERE user_id = $1
             AND project_id = ANY($2::int[])",
          params = list(user_id, to_pg_int_array(remove_project_ids))
        )
      }
    })

    invisible(list(
      success          = TRUE,
      user             = username,
      added_projects   = add_project_ids,
      removed_projects = remove_project_ids
    ))

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
