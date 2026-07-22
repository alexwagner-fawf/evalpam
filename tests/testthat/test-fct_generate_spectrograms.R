library(testthat)
library(mockery)

# ============================================================================
# generate_spectrograms() — no DB required (all external calls stubbed)
# ============================================================================

# Minimal sf deployment table returned by sf::st_read stub
make_deploy_sf <- function(n = 3, same_location = FALSE) {
  if (n == 0L) {
    return(sf::st_sf(
      deployment_id   = integer(0),
      project_id      = integer(0),
      deployment_name = character(0),
      start_datetime  = as.POSIXct(character(0), tz = "UTC"),
      geometry        = sf::st_sfc(crs = 4326)
    ))
  }
  lons <- if (same_location) rep(7.5, n) else seq(7.5, 7.5 + (n - 1) * 0.1, by = 0.1)
  lats <- if (same_location) rep(49.5, n) else rep(49.5, n)
  pts  <- lapply(seq_len(n), function(i) sf::st_point(c(lons[i], lats[i])))
  sf::st_sf(
    deployment_id   = seq_len(n),
    project_id      = 1L,
    deployment_name = paste0("dep_", seq_len(n)),
    start_datetime  = as.POSIXct(paste0("2024-0", seq_len(n), "-01"), tz = "UTC"),
    geometry        = sf::st_sfc(pts, crs = 4326)
  )
}

make_samples <- function(n = 5) {
  if (n == 0L) {
    return(data.frame(
      audio_file_id = integer(0),
      deployment_id = integer(0),
      begin_time_ms = integer(0),
      end_time_ms   = integer(0),
      confidence    = numeric(0),
      species_id    = integer(0),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    audio_file_id = 1L,
    deployment_id = 1L,
    begin_time_ms = seq(0L, (n - 1L) * 3000L, by = 3000L),
    end_time_ms   = seq(3000L, n * 3000L, by = 3000L),
    confidence    = seq(0.9, 0.9 - (n - 1) * 0.1, by = -0.1),
    species_id    = rep(1L, n),
    stringsAsFactors = FALSE
  )
}

# --------------------------------------------------------------------------
# 1. output_dir fallback behaviour
# --------------------------------------------------------------------------

test_that("output_dir defaults to spectogram_folder env var when set", {
  withr::with_envvar(c(spectogram_folder = "/env/audio"), {
    captured_dir <- NULL
    stub(generate_spectrograms, "sf::st_read",    make_deploy_sf())
    stub(generate_spectrograms, "sample_results_table", make_samples())
    stub(generate_spectrograms, "build_audio_clips_db", function(data, pool,
                                                                   padding_s,
                                                                   output_dir,
                                                                   export_to_db,
                                                                   verbose, ...) {
      captured_dir <<- output_dir
      invisible(NULL)
    })
    suppressMessages(
      generate_spectrograms(pool = list(), project_id = 1L, export_to_db = FALSE,
                            verbose = FALSE)
    )
    expect_equal(captured_dir, "/env/audio")
  })
})

test_that("output_dir falls back to ./spectograms when env var is unset", {
  withr::with_envvar(c(spectogram_folder = ""), {
    captured_dir <- NULL
    stub(generate_spectrograms, "sf::st_read",    make_deploy_sf())
    stub(generate_spectrograms, "sample_results_table", make_samples())
    stub(generate_spectrograms, "build_audio_clips_db", function(data, pool,
                                                                   padding_s,
                                                                   output_dir,
                                                                   export_to_db,
                                                                   verbose, ...) {
      captured_dir <<- output_dir
      invisible(NULL)
    })
    suppressMessages(
      generate_spectrograms(pool = list(), project_id = 1L, export_to_db = FALSE,
                            verbose = FALSE)
    )
    expect_match(captured_dir, "spectograms$")
  })
})

test_that("explicit output_dir is passed through unchanged", {
  captured_dir <- NULL
  stub(generate_spectrograms, "sf::st_read",    make_deploy_sf())
  stub(generate_spectrograms, "sample_results_table", make_samples())
  stub(generate_spectrograms, "build_audio_clips_db", function(data, pool,
                                                                 padding_s,
                                                                 output_dir,
                                                                 export_to_db,
                                                                 verbose, ...) {
    captured_dir <<- output_dir
    invisible(NULL)
  })
  suppressMessages(
    generate_spectrograms(pool = list(), project_id = 1L,
                          output_dir = "/custom/dir", export_to_db = FALSE,
                          verbose = FALSE)
  )
  expect_equal(captured_dir, "/custom/dir")
})

# --------------------------------------------------------------------------
# 2. Deployment selection: one per location vs explicit IDs
# --------------------------------------------------------------------------

test_that("one-per-location: three deployments at three locations → all three selected", {
  selected <- NULL
  stub(generate_spectrograms, "sf::st_read", make_deploy_sf(3, same_location = FALSE))
  stub(generate_spectrograms, "sample_results_table", function(deployment_ids, ...) {
    selected <<- deployment_ids
    make_samples()
  })
  stub(generate_spectrograms, "build_audio_clips_db", function(...) invisible(NULL))
  suppressMessages(
    generate_spectrograms(pool = list(), project_id = 1L, verbose = FALSE)
  )
  expect_equal(length(selected), 3L)
})

test_that("one-per-location: two co-located deployments → only earliest selected", {
  deploy_sf <- make_deploy_sf(2, same_location = TRUE)
  # Make second deployment clearly later
  deploy_sf$start_datetime[2] <- as.POSIXct("2024-06-01", tz = "UTC")

  selected <- NULL
  stub(generate_spectrograms, "sf::st_read", deploy_sf)
  stub(generate_spectrograms, "sample_results_table", function(deployment_ids, ...) {
    selected <<- deployment_ids
    make_samples()
  })
  stub(generate_spectrograms, "build_audio_clips_db", function(...) invisible(NULL))
  suppressMessages(
    generate_spectrograms(pool = list(), project_id = 1L, verbose = FALSE)
  )
  expect_equal(length(selected), 1L)
  expect_equal(selected, 1L)   # deployment_id 1 has the earlier start_datetime
})

test_that("explicit deployment_ids skips sf::st_read entirely", {
  st_read_called <- FALSE
  stub(generate_spectrograms, "sf::st_read", function(...) {
    st_read_called <<- TRUE
    make_deploy_sf()
  })
  stub(generate_spectrograms, "dplyr::tbl", function(pool, id) {
    structure(list(), class = "tbl_lazy")
  })
  stub(generate_spectrograms, "dplyr::filter", function(x, ...) x)
  stub(generate_spectrograms, "dplyr::collect",
       data.frame(deployment_id = 7L, deployment_name = "dep_7",
                  stringsAsFactors = FALSE))
  stub(generate_spectrograms, "sample_results_table", make_samples())
  stub(generate_spectrograms, "build_audio_clips_db", function(...) invisible(NULL))
  suppressMessages(
    generate_spectrograms(pool = list(), deployment_ids = 7L, verbose = FALSE)
  )
  expect_false(st_read_called)
})

# --------------------------------------------------------------------------
# 3. Deduplication: one clip per (audio_file_id, begin_time_ms)
# --------------------------------------------------------------------------

test_that("build_audio_clips_db is called once per deployment group", {
  # Two deployments, one clip each → two calls
  two_dep_samples <- data.frame(
    audio_file_id = c(1L, 2L),
    deployment_id = c(1L, 2L),
    begin_time_ms = c(0L, 0L),
    end_time_ms   = c(3000L, 3000L),
    confidence    = c(0.9, 0.8),
    species_id    = c(1L, 1L),
    stringsAsFactors = FALSE
  )
  call_count <- 0L
  stub(generate_spectrograms, "sf::st_read",    make_deploy_sf(2))
  stub(generate_spectrograms, "sample_results_table", two_dep_samples)
  stub(generate_spectrograms, "build_audio_clips_db", function(...) {
    call_count <<- call_count + 1L
    invisible(NULL)
  })
  suppressMessages(
    generate_spectrograms(pool = list(), project_id = 1L, verbose = FALSE)
  )
  expect_equal(call_count, 2L)
})

test_that("duplicate (audio_file_id, begin_time_ms) across species are deduplicated", {
  # Same window detected as two species → should produce exactly one clip
  dup_samples <- data.frame(
    audio_file_id = c(1L, 1L),
    deployment_id = c(1L, 1L),
    begin_time_ms = c(0L, 0L),
    end_time_ms   = c(3000L, 3000L),
    confidence    = c(0.9, 0.7),
    species_id    = c(1L, 2L),   # different species, same window
    stringsAsFactors = FALSE
  )
  clips_data <- NULL
  stub(generate_spectrograms, "sf::st_read",    make_deploy_sf(1))
  stub(generate_spectrograms, "sample_results_table", dup_samples)
  stub(generate_spectrograms, "build_audio_clips_db", function(data, ...) {
    clips_data <<- data
    invisible(NULL)
  })
  suppressMessages(
    generate_spectrograms(pool = list(), project_id = 1L, verbose = FALSE)
  )
  expect_equal(nrow(clips_data), 1L)
  expect_equal(clips_data$confidence, 0.9)   # higher-confidence row kept
})

# --------------------------------------------------------------------------
# 4. Error conditions
# --------------------------------------------------------------------------

test_that("errors when no deployments are found for project_id", {
  stub(generate_spectrograms, "sf::st_read", make_deploy_sf(0))
  expect_error(
    generate_spectrograms(pool = list(), project_id = 99L, verbose = FALSE),
    regexp = "No deployments"
  )
})

test_that("errors when sample_results_table returns 0 rows", {
  stub(generate_spectrograms, "sf::st_read", make_deploy_sf(1))
  stub(generate_spectrograms, "sample_results_table",
       make_samples(0))   # 0-row data frame
  expect_error(
    generate_spectrograms(pool = list(), project_id = 1L, verbose = FALSE),
    regexp = "0 rows"
  )
})

# --------------------------------------------------------------------------
# 5. Return value
# --------------------------------------------------------------------------

test_that("returns the number of unique clips generated (invisibly)", {
  samples <- make_samples(6)
  # All six rows have distinct begin_time_ms → 6 unique clips
  stub(generate_spectrograms, "sf::st_read",    make_deploy_sf(1))
  stub(generate_spectrograms, "sample_results_table", samples)
  stub(generate_spectrograms, "build_audio_clips_db", function(...) invisible(NULL))
  n <- suppressMessages(
    withVisible(
      generate_spectrograms(pool = list(), project_id = 1L, verbose = FALSE)
    )
  )
  expect_equal(n$value, 6L)
  expect_false(n$visible)
})
