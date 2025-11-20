test_that("set_db_pool() returns a valid pool connection", {
  # Ensure environment variable is set for tests
  expect_true(nzchar(Sys.getenv("evalpam_pw")),
              info = "Environment variable 'evalpam_pw' must be set")

  # Check golem-config entries exist
  expect_true(nzchar(get_golem_config("pg_user")),
              info = "Missing pg_user in golem-config.yml")
  expect_true(nzchar(get_golem_config("pg_host")),
              info = "Missing pg_host in golem-config.yml")
  expect_true(nzchar(get_golem_config("pg_port")),
              info = "Missing pg_port in golem-config.yml")
  expect_true(nzchar(get_golem_config("pg_dbname")),
              info = "Missing pg_dbname in golem-config.yml")

  pool <- set_db_pool()

  # Test object type
  expect_s3_class(pool, "Pool")

  # Test that a connection can be obtained
  con <- pool::poolCheckout(pool)
  expect_s4_class(con, "PqConnection")
  pool::poolReturn(con)

  # Cleanup
  pool::poolClose(pool)
})
