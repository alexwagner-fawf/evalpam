library(testthat)

# These validation checks run before any DB access, so no pool is needed.

test_that("stratified + prioritize is rejected", {
  expect_error(
    sample_results_table(
      confidence_selection_mode = "stratified",
      deployment_ids            = 1L,
      result_ids                = c(1L, 2L),
      result_id_mode            = "prioritize",
      pool                      = list()
    ),
    "not supported with"
  )
})

test_that("stratified + exclusive is allowed past validation (fails later at DB, not on args)", {
  # exclusive mode is fine with stratified; the error, if any, must come from
  # the DB access, not the argument guard.
  err <- tryCatch(
    sample_results_table(
      confidence_selection_mode = "stratified",
      deployment_ids            = 1L,
      result_ids                = c(1L, 2L),
      result_id_mode            = "exclusive",
      pool                      = list()
    ),
    error = function(e) conditionMessage(e)
  )
  expect_false(grepl("not supported with", err))
})

test_that("prioritize with top/random passes the stratified guard", {
  for (mode in c("top", "random")) {
    err <- tryCatch(
      sample_results_table(
        confidence_selection_mode = mode,
        deployment_ids            = 1L,
        result_ids                = c(1L, 2L),
        result_id_mode            = "prioritize",
        pool                      = list()
      ),
      error = function(e) conditionMessage(e)
    )
    expect_false(grepl("not supported with", err))
  }
})
