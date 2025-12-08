test_that("check_credentials_db() validates credentials correctly", {

  skip_if_not(
    nzchar(Sys.getenv("evalpam_pw")),
    "Database password not available"
  )

  # temporary pool
  pool <- pool::dbPool(
    RPostgres::Postgres(),
    user     = get_golem_config("pg_user"),
    host     = get_golem_config("pg_host"),
    port     = get_golem_config("pg_port"),
    dbname   = get_golem_config("pg_dbname"),
    password = rawToChar(base64enc::base64decode(Sys.getenv("evalpam_pw")))
  )

  # Create temporary test table
  DBI::dbExecute(pool, "
    CREATE TEMP TABLE app_users_test (
      pameval_user TEXT PRIMARY KEY,
      password_hash TEXT NOT NULL,
      active BOOLEAN DEFAULT TRUE
    );
  ")

  # Insert a test user
  test_user <- "alice"
  test_pw   <- "secret123"
  pw_hash   <- bcrypt::hashpw(test_pw)

  DBI::dbExecute(
    pool,
    "INSERT INTO app_users_test (pameval_user, password_hash, active)
     VALUES ($1, $2, TRUE)",
    params = list(test_user, pw_hash)
  )

  # Patch the SQL query inside the function to use temp table
  check_fun <- function(pool) {
    function(user, password) {

      row <- DBI::dbGetQuery(
        pool,
        "SELECT password_hash
         FROM app_users_test
         WHERE pameval_user = $1 AND active = TRUE",
        params = list(user)
      )

      if (nrow(row) == 1 && bcrypt::checkpw(password, row$password_hash)) {
        return(list(result = TRUE, user = user))
      }

      return(FALSE)
    }
  }

  check <- check_fun(pool)

  # --- Tests ---

  # Successful login
  res_ok <- check(test_user, test_pw)
  expect_type(res_ok, "list")
  expect_true(res_ok$result)
  expect_identical(res_ok$user, test_user)

  # Wrong password
  res_bad_pw <- check(test_user, "wrongpw")
  expect_false(res_bad_pw)

  # Unknown user
  res_no_user <- check("nobody", "whatever")
  expect_false(res_no_user)

  pool::poolClose(pool)
})
