test_that("add_users inserts a user and a role", {

  # dbGetQuery: role exists, username doesn't exist, then user_id lookup
  mock_db_query <- mockery::mock(
    data.frame(count = 1),
    data.frame(count = 0),
    data.frame(user_id = 10)
  )

  # dbExecute: insert user, insert role (provide at least 2)
  mock_db_exec <- mockery::mock(1, 1)

  conn <- structure(list(), class = "Pool")

  mockery::stub(add_users, "DBI::dbGetQuery", mock_db_query)
  mockery::stub(add_users, "DBI::dbExecute", mock_db_exec)
  mockery::stub(add_users, "pool::poolWithTransaction", function(pool, code) code(conn))

  result <- add_users(
    pool = conn,
    username = "testuser",
    password = "goodpassword",
    pg_role = "testrole"
  )

  expect_true(result$success)
  expect_equal(result$user, "testuser")
  expect_equal(result$role, "testrole")

  # Assert mocks were called at least once
  expect_true(length(mockery::mock_calls(mock_db_query)) > 0)
  expect_true(length(mockery::mock_calls(mock_db_exec)) > 0)

  # Optionally, assert exact number of calls
  expect_equal(length(mock_db_query), 3)  # 3 dbGetQuery calls mocked above
  expect_equal(length(mock_db_exec), 2)   # 2 dbExecute calls mocked above
})

test_that("update_user updates fields", {

  # dbGetQuery: fetch user_id, check role exists
  mock_db_query <- mockery::mock(
    data.frame(user_id = 10),
    data.frame(count = 1)
  )

  # dbExecute: update password, update other fields, insert role (supply enough)
  mock_db_exec <- mockery::mock(1, 1, 1)

  conn <- structure(list(), class = "Pool")

  mockery::stub(update_user, "DBI::dbGetQuery", mock_db_query)
  mockery::stub(update_user, "DBI::dbExecute", mock_db_exec)
  mockery::stub(update_user, "pool::poolWithTransaction", function(pool, code) code(conn))

  result <- update_user(
    pool = conn,
    username = "testuser",
    password = "goodpassword",
    pg_role = "newrole"
  )

  expect_true(result$success)

  # Assert mocks were invoked
  expect_true(length(mockery::mock_calls(mock_db_query)) > 0)
  expect_true(length(mockery::mock_calls(mock_db_exec)) > 0)

  # Optionally assert exact counts
  expect_equal(length(mock_db_query), 2)
  expect_equal(length(mock_db_exec), 2)
})

test_that("delete_user deletes a user", {

  mock_db_exec <- mockery::mock(1)
  mock_db_query <- mockery::mock(
    data.frame(user_id = 10)  # fetch user_id
  )

  conn <- structure(list(), class = "Pool")

  mockery::stub(delete_user, "DBI::dbGetQuery", mock_db_query)
  mockery::stub(delete_user, "DBI::dbExecute", mock_db_exec)
  mockery::stub(delete_user, "pool::poolWithTransaction", function(pool, code) code(conn))

  result <- delete_user(
    pool = conn,
    username = "testuser",
    confirm = TRUE
  )

  expect_true(result$success)
})


test_that("list_users returns data", {

  df <- data.frame(
    user_id = 10,
    username = "user",
    first_name = "A",
    last_name = "B",
    email = "a@b.com",
    active = TRUE,
    expire_date = as.Date("2030-01-01"),
    created_at = Sys.time(),
    pg_role = "role"
  )

  mock_db_query <- mockery::mock(df)
  conn <- structure(list(), class = "Pool")

  mockery::stub(list_users, "DBI::dbGetQuery", mock_db_query)

  result <- list_users(conn)

  expect_equal(result, df)
})

