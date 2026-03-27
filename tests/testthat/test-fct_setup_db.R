test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# ==============================================================
# 7. split_sql_statements: Dollar-Quoting
# ==============================================================
test_that("split_sql_statements respektiert $$-Blöcke", {
  sql <- "CREATE TABLE t1 (id int); CREATE FUNCTION test() RETURNS void AS $$ BEGIN PERFORM 1; END; $$ LANGUAGE plpgsql; CREATE TABLE t2 (id int)"
  stmts <- split_sql_statements(sql)

  expect_length(stmts, 3)
  expect_true(grepl("CREATE TABLE t1", stmts[1]))
  expect_true(grepl("\\$\\$.*END;.*\\$\\$", stmts[2]))
  expect_true(grepl("CREATE TABLE t2", stmts[3]))
})

test_that("split_sql_statements behandelt benannte Tags ($fn$)", {
  sql <- "SELECT 1; CREATE FUNCTION x() RETURNS void AS $fn$ BEGIN PERFORM 1; END; $fn$ LANGUAGE plpgsql; SELECT 2"
  stmts <- split_sql_statements(sql)

  expect_length(stmts, 3)
  expect_true(grepl("\\$fn\\$.*END;.*\\$fn\\$", stmts[2]))
})
