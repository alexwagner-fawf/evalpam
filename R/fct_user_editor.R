#' Add User to Database
#'
#' @description Adds a user to app_users and app_user_roles tables
#'
#' @param pool Database connection pool
#' @param pameval_user Username (primary key)
#' @param password Plain text password (will be hashed)
#' @param pg_role PostgreSQL role to assign
#' @param active Boolean, whether user is active (default: TRUE)
#'
#' @return List with success status and messages
#'
#' @export
add_users <- function(pool, pameval_user, password, pg_role, active = TRUE) {

  # === VALIDATION ===
  if(is.null(pameval_user) || pameval_user == "") {
    stop("pameval_user cannot be empty")
  }

  if(is.null(password) || nchar(password) < 8) {
    stop("password must be at least 8 characters long")
  }

  if(is.null(pg_role) || pg_role == "") {
    stop("pg_role cannot be empty")
  }

  # === CHECK IF ROLE EXISTS ===
  role_exists <- tryCatch({
    result <- DBI::dbGetQuery(
      pool,
      "SELECT COUNT(*) as count FROM pg_roles WHERE rolname = $1",
      params = list(pg_role)
    )
    result$count[1] > 0
  }, error = function(e) {
    stop("Failed to check if role exists: ", e$message)
  })

  if(!role_exists) {
    stop("PostgreSQL role '", pg_role, "' does not exist. Create role first.")
  }

  # === CHECK IF USER ALREADY EXISTS ===
  user_exists <- tryCatch({
    result <- DBI::dbGetQuery(
      pool,
      "SELECT COUNT(*) as count FROM app_users WHERE pameval_user = $1",
      params = list(pameval_user)
    )
    result$count[1] > 0
  }, error = function(e) {
    stop("Failed to check if user exists: ", e$message)
  })

  if(user_exists) {
    stop("User '", pameval_user, "' already exists. Use update_user() to modify.")
  }

  # === HASH PASSWORD ===
  hashed_password <- bcrypt::hashpw(password)

  # === INSERT WITH TRANSACTION ===
  tryCatch({
    pool::poolWithTransaction(pool, function(conn) {
      # Insert into app_users
      DBI::dbExecute(
        conn,
        "INSERT INTO app_users (pameval_user, password_hash, active, created_at) VALUES ($1, $2, $3, $4)",
        params = list(pameval_user, hashed_password, active, Sys.time())
      )

      # Insert into app_user_roles
      DBI::dbExecute(
        conn,
        "INSERT INTO app_user_roles (pameval_user, pg_role) VALUES ($1, $2)",
        params = list(pameval_user, pg_role)
      )

      # Grant permissions based on role
      if(pg_role == "evalpam_admin") {
        # Admin gets full access (SELECT, INSERT, UPDATE, DELETE)
        DBI::dbExecute(
          conn,
          glue::glue_sql("GRANT SELECT, INSERT, UPDATE, DELETE ON app_users TO {`pg_role`}", .con = conn)
        )

        DBI::dbExecute(
          conn,
          glue::glue_sql("GRANT SELECT, INSERT, UPDATE, DELETE ON app_user_roles TO {`pg_role`}", .con = conn)
        )

        message("✓ Granted full access (SELECT, INSERT, UPDATE, DELETE) to role '", pg_role, "'")
      } else {
        # Regular users get read-only access
        DBI::dbExecute(
          conn,
          glue::glue_sql("GRANT SELECT ON app_users TO {`pg_role`}", .con = conn)
        )

        DBI::dbExecute(
          conn,
          glue::glue_sql("GRANT SELECT ON app_user_roles TO {`pg_role`}", .con = conn)
        )

        message("✓ Granted SELECT access on app_users and app_user_roles to role '", pg_role, "'")
      }
    })

    message("✓ User '", pameval_user, "' created successfully with role '", pg_role, "'")

    return(invisible(list(
      success = TRUE,
      user = pameval_user,
      role = pg_role,
      active = active
    )))

  }, error = function(e) {
    # poolWithTransaction handles rollback automatically
    stop("Failed to create user: ", e$message)
  })
}


#' Update existing user
#'
#' @description Updates user password, role, or active status
#'
#' @export
update_user <- function(pool, pameval_user, password = NULL, pg_role = NULL, active = NULL) {

  # Check if user exists
  user_exists <- tryCatch({
    result <- DBI::dbGetQuery(
      pool,
      "SELECT COUNT(*) as count FROM app_users WHERE pameval_user = $1",
      params = list(pameval_user)
    )
    result$count[1] > 0
  }, error = function(e) {
    stop("Failed to check if user exists: ", e$message)
  })

  if(!user_exists) {
    stop("User '", pameval_user, "' does not exist. Use add_users() to create.")
  }

  tryCatch({
    pool::poolWithTransaction(pool, function(conn) {
      # Update password if provided
      if(!is.null(password)) {
        if(nchar(password) < 8) {
          stop("Password must be at least 8 characters long")
        }
        hashed_password <- bcrypt::hashpw(password)
        DBI::dbExecute(
          conn,
          "UPDATE app_users SET password_hash = $1 WHERE pameval_user = $2",
          params = list(hashed_password, pameval_user)
        )
        message("Password updated")
      }

      # Update active status if provided
      if(!is.null(active)) {
        DBI::dbExecute(
          conn,
          "UPDATE app_users SET active = $1 WHERE pameval_user = $2",
          params = list(active, pameval_user)
        )
        message("✓ Active status updated to: ", active)
      }

      # Update role if provided
      if(!is.null(pg_role)) {
        # Check if new role exists
        role_exists <- tryCatch({
          result <- DBI::dbGetQuery(
            conn,
            "SELECT COUNT(*) as count FROM pg_roles WHERE rolname = $1",
            params = list(pg_role)
          )
          result$count[1] > 0
        }, error = function(e) {
          stop("Failed to check if role exists: ", e$message)
        })

        if(!role_exists) {
          stop("PostgreSQL role '", pg_role, "' does not exist")
        }

        # Update or insert role
        DBI::dbExecute(
          conn,
          "INSERT INTO app_user_roles (pameval_user, pg_role)
           VALUES ($1, $2)
           ON CONFLICT (pameval_user)
           DO UPDATE SET pg_role = EXCLUDED.pg_role",
          params = list(pameval_user, pg_role)
        )
        message("✓ Role updated to: ", pg_role)
      }
    })

    return(invisible(list(success = TRUE, user = pameval_user)))

  }, error = function(e) {
    stop("Failed to update user: ", e$message)
  })
}


#' Delete user
#'
#' @description Removes user from app_users and app_user_roles
#'
#' @export
delete_user <- function(pool, pameval_user, confirm = FALSE) {

  if(!confirm) {
    stop("Set confirm = TRUE to delete user '", pameval_user, "'")
  }

  user_exists <- tryCatch({
    result <- DBI::dbGetQuery(
      pool,
      "SELECT COUNT(*) as count FROM app_users WHERE pameval_user = $1",
      params = list(pameval_user)
    )
    result$count[1] > 0
  }, error = function(e) {
    stop("Failed to check if user exists: ", e$message)
  })

  if(!user_exists) {
    message("User '", pameval_user, "' does not exist")
    return(invisible(list(success = FALSE, message = "User not found")))
  }

  tryCatch({
    pool::poolWithTransaction(pool, function(conn) {
      # Delete from app_user_roles first (foreign key constraint)
      DBI::dbExecute(
        conn,
        "DELETE FROM app_user_roles WHERE pameval_user = $1",
        params = list(pameval_user)
      )

      # Delete from app_users
      DBI::dbExecute(
        conn,
        "DELETE FROM app_users WHERE pameval_user = $1",
        params = list(pameval_user)
      )
    })

    message("✓ User '", pameval_user, "' deleted successfully")

    return(invisible(list(success = TRUE, user = pameval_user)))

  }, error = function(e) {
    stop("Failed to delete user: ", e$message)
  })
}


#' List all users
#'
#' @description Returns all users with their roles and active status
#'
#' @export
list_users <- function(pool) {
  tryCatch({
    DBI::dbGetQuery(pool, "
      SELECT
        u.pameval_user,
        u.active,
        r.pg_role,
        u.created_at
      FROM app_users u
      LEFT JOIN app_user_roles r ON u.pameval_user = r.pameval_user
      ORDER BY u.pameval_user
    ")
  }, error = function(e) {
    stop("Failed to list users: ", e$message)
  })
}
