library(testthat)
library(mockery)

# ============================================================================
# .resolve_timestamp_duplicates() — pure logic, no DB
# ============================================================================

make_dup_df <- function(paths, timestamps, durations, dupls,
                        deployment_id = 1L) {
  data.frame(
    audio_file_id   = seq_along(paths),
    deployment_id   = deployment_id,
    relative_path   = paths,
    timestamp_start = as.POSIXct(timestamps, tz = "UTC"),
    duration_s      = durations,
    dupls           = dupls,
    stringsAsFactors = FALSE
  )
}

test_that("no-duplicate input passes through unchanged", {
  df <- make_dup_df(
    paths      = c("dep/SM_20240501_060000.wav", "dep/SM_20240501_070000.wav"),
    timestamps = c("2024-05-01 06:00:00",       "2024-05-01 07:00:00"),
    durations  = c(300, 300),
    dupls      = c(FALSE, FALSE)
  )
  res <- .resolve_timestamp_duplicates(df)
  expect_equal(nrow(res), 2L)
  expect_setequal(res$audio_file_id, 1:2)
})

test_that("keeps the longer file among a duplicate pair", {
  df <- make_dup_df(
    paths      = c("dep/SM_20240501_080000.wav", "dep/SM_20240501_080000_v2.wav"),
    timestamps = c("2024-05-01 08:00:00",        "2024-05-01 08:00:00"),
    durations  = c(60, 300),
    dupls      = c(TRUE, TRUE)
  )
  res <- .resolve_timestamp_duplicates(df)
  expect_equal(nrow(res), 1L)
  expect_equal(res$duration_s, 300)
})

test_that("three-way tie collapses to exactly one row", {
  df <- make_dup_df(
    paths      = c("dep/SM_20240501_090000.wav",
                   "dep/SM_20240501_090000_b.wav",
                   "dep/SM_20240501_090000_c.wav"),
    timestamps = rep("2024-05-01 09:00:00", 3),
    durations  = c(300, 300, 300),
    dupls      = rep(TRUE, 3)
  )
  res <- .resolve_timestamp_duplicates(df)
  expect_equal(nrow(res), 1L)
})

test_that("unique and duplicate rows are handled independently", {
  df <- make_dup_df(
    paths      = c("dep/SM_20240501_060000.wav",    # unique
                   "dep/SM_20240501_080000.wav",    # dup — short
                   "dep/SM_20240501_080000_b.wav"),  # dup — long
    timestamps = c("2024-05-01 06:00:00",
                   "2024-05-01 08:00:00",
                   "2024-05-01 08:00:00"),
    durations  = c(300, 60, 300),
    dupls      = c(FALSE, TRUE, TRUE)
  )
  res <- .resolve_timestamp_duplicates(df)
  expect_equal(nrow(res), 2L)
  expect_true(1L %in% res$audio_file_id)            # unique row survives
  expect_equal(max(res$duration_s), 300)             # longer dup survives
})

test_that("duplicate resolution is independent per deployment", {
  df <- data.frame(
    audio_file_id   = 1:4,
    deployment_id   = c(1L, 1L, 2L, 2L),
    relative_path   = c("d1/SM_20240501_060000.wav",   "d1/SM_20240501_060000_b.wav",
                        "d2/SM_20240601_070000.wav",   "d2/SM_20240601_070000_b.wav"),
    timestamp_start = as.POSIXct(c("2024-05-01 06:00:00", "2024-05-01 06:00:00",
                                    "2024-06-01 07:00:00", "2024-06-01 07:00:00"), tz = "UTC"),
    duration_s      = c(60, 300, 120, 60),
    dupls           = c(TRUE, TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  res <- .resolve_timestamp_duplicates(df)
  expect_equal(nrow(res), 2L)
  expect_equal(res$duration_s[res$deployment_id == 1L], 300)
  expect_equal(res$duration_s[res$deployment_id == 2L], 120)
})

test_that("re-parses timestamp from YYYYMMDD_HHMMSS filename suffix", {
  # Stored timestamps are corrupt (NA); must be derived from filename.
  df <- make_dup_df(
    paths      = c("dep/site_20240601_153000.wav", "dep/site_20240601_153000_v2.wav"),
    timestamps = c(NA_character_, NA_character_),
    durations  = c(60, 300),
    dupls      = c(TRUE, TRUE)
  )
  res <- suppressWarnings(.resolve_timestamp_duplicates(df))
  expect_equal(nrow(res), 1L)
  expect_equal(format(res$timestamp_start, tz = "UTC"), "2024-06-01 15:30:00")
  expect_equal(res$duration_s, 300)
})

test_that("midnight recording falls back to ymd() when HHMMSS part is missing", {
  # Filename ends in YYYYMMDD_000000; ymd_hms parses it fine in most locales,
  # so this test verifies the midnight case returns a valid timestamp (not NA).
  df <- make_dup_df(
    paths      = c("dep/SM_20240501_000000.wav", "dep/SM_20240501_000000_b.wav"),
    timestamps = c(NA_character_, NA_character_),
    durations  = c(60, 120),
    dupls      = c(TRUE, TRUE)
  )
  res <- suppressWarnings(.resolve_timestamp_duplicates(df))
  # If re-parsing worked, one row with a non-NA timestamp survives.
  expect_equal(nrow(res), 1L)
  expect_false(is.na(res$timestamp_start))
})

test_that("warns and drops rows whose timestamp cannot be re-parsed from filename", {
  df <- make_dup_df(
    paths      = c("dep/no_date.wav", "dep/also_no_date.wav"),
    timestamps = c(NA_character_, NA_character_),
    durations  = c(60, 60),
    dupls      = c(TRUE, TRUE)
  )
  expect_warning(.resolve_timestamp_duplicates(df), "NA timestamp")
  res <- suppressWarnings(.resolve_timestamp_duplicates(df))
  expect_equal(nrow(res), 0L)
})

test_that("partial parse failure: warns for bad rows and keeps parseable ones", {
  df <- data.frame(
    audio_file_id   = 1:4,
    deployment_id   = 1L,
    relative_path   = c("dep/SM_20240501_060000.wav",   # parseable
                        "dep/SM_20240501_060000_b.wav", # parseable dup
                        "dep/no_date.wav",              # unparseable dup
                        "dep/also_no_date.wav"),        # unparseable dup
    timestamp_start = as.POSIXct(c(NA, NA, NA, NA), tz = "UTC"),
    duration_s      = c(300, 60, 60, 60),
    dupls           = c(TRUE, TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  expect_warning(res <- .resolve_timestamp_duplicates(df), "NA timestamp")
  # The two parseable rows share a timestamp → collapse to 1; the bad pair → 0
  expect_equal(nrow(res), 1L)
  expect_equal(format(res$timestamp_start, tz = "UTC"), "2024-05-01 06:00:00")
})

# ============================================================================
# ingest_audio_files() — input validation guards (no DB needed)
# ============================================================================

test_that("errors when deployments_sf is not an sf object", {
  expect_error(
    ingest_audio_files(
      pool               = list(),
      project_name_short = "test",
      project_folder     = tempdir(),
      deployments_sf     = data.frame(deployment_name = "A")
    )
  )
})

test_that("errors when deployments_sf lacks a deployment_name column", {
  bad_sf <- sf::st_sf(geom = sf::st_sfc(sf::st_point(c(7, 49)), crs = 4326))
  expect_error(
    ingest_audio_files(
      pool               = list(),
      project_name_short = "test",
      project_folder     = tempdir(),
      deployments_sf     = bad_sf
    )
  )
})

test_that("errors informatively when no folder names match deployments_sf", {
  good_sf <- sf::st_sf(
    deployment_name = "dep_A",
    geom = sf::st_sfc(sf::st_point(c(7.5, 49.5)), crs = 4326)
  )

  # Stub every external call so we reach the join-check
  stub(ingest_audio_files, "DBI::dbReadTable",
       data.frame(project_name_short = character(0), project_id = integer(0)))
  stub(ingest_audio_files, "upsert_project", 1L)
  stub(ingest_audio_files, "retrieve_local_file_info", list(
    deployment_index       = tempfile(fileext = ".csv"),
    new_audio_file_indices = character(0),
    all_audio_file_indices = character(0)
  ))
  # Scanning returns a different deployment name → join produces 0 rows
  stub(ingest_audio_files, "readr::read_csv",
       data.frame(deployment_name = "completely_different",
                  deployment_path = "/mnt/audio",
                  start_datetime  = NA_character_,
                  end_datetime    = NA_character_,
                  valid           = NA,
                  stringsAsFactors = FALSE))

  expect_error(
    ingest_audio_files(
      pool               = list(),
      project_name_short = "test",
      project_folder     = tempdir(),
      deployments_sf     = good_sf
    ),
    regexp = "No deployments matched"
  )
})
