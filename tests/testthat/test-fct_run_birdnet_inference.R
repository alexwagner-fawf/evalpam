# Regression tests for run_birdnet_inference()
#
# Guards the guarantee that BirdNET output is stored one row PER SPECIES per
# 3-s segment: the per-file dedup is keyed on species_id, so several species in
# the same time window must all survive. Only exact (window, species) duplicates
# collapse (keeping the highest-confidence copy).
#
# apply_birdnet_model() is stubbed so no audio/model backend is needed.

library(testthat)
library(mockery)

# possible_species_df: scientific name -> species_id, as get_possible_species()
# returns it.
make_possible_species <- function() {
  data.frame(
    species_scientific = c("Turdus merula", "Cyanistes caeruleus", "Parus major"),
    species_id         = c(10L, 20L, 30L),
    stringsAsFactors = FALSE
  )
}

# One audio file to iterate over.
make_audio_subset <- function() {
  data.frame(
    audio_file_id = 1L,
    full_path     = "/does/not/matter.wav",
    week          = -999,
    stringsAsFactors = FALSE
  )
}

# Build a stub apply_birdnet_model() that returns a fixed prediction_raw
# (the raw BirdNET frame shape: start, end, scientific_name, common_name,
# confidence).
stub_apply <- function(prediction_raw) {
  function(audio_file, birdnet_setup, birdnet_params, ...) {
    list(error = NULL,
         prediction_time = as.POSIXct("2024-05-01 10:00:00", tz = "UTC"),
         prediction_raw = prediction_raw)
  }
}

run_with <- function(prediction_raw) {
  local_apply <- stub_apply(prediction_raw)
  # Stub the call as seen inside run_birdnet_inference.
  stub(run_birdnet_inference, "apply_birdnet_model", local_apply)
  run_birdnet_inference(
    audio_files_subset  = make_audio_subset(),
    bnm                 = list(model = list(dummy = TRUE)),
    birdnet_params      = list(min_confidence = 0.2),
    settings_id         = 7L,
    possible_species_df = make_possible_species()
  )
}

# --------------------------------------------------------------------------
# 1. Multiple species in ONE segment are all retained
# --------------------------------------------------------------------------

test_that("two species in the same 3-s window produce two rows", {
  pred <- data.frame(
    start           = c(39,   39),
    end             = c(42,   42),
    scientific_name = c("Turdus merula", "Cyanistes caeruleus"),
    common_name     = c("Eurasian Blackbird", "Eurasian Blue Tit"),
    confidence      = c(0.9,  0.6),
    stringsAsFactors = FALSE
  )
  out <- run_with(pred)

  win <- out[out$begin_time_ms == 39000L & out$end_time_ms == 42000L, ]
  expect_equal(nrow(win), 2L)
  expect_setequal(win$species_id, c(10L, 20L))
  # same settings_id / audio_file_id, distinct species — i.e. NOT collapsed to top-1
  expect_equal(unique(win$settings_id), 7L)
  expect_equal(unique(win$audio_file_id), 1L)
})

# --------------------------------------------------------------------------
# 2. Multi-species across several segments; per-segment counts preserved
# --------------------------------------------------------------------------

test_that("species counts per segment match the raw prediction", {
  pred <- data.frame(
    start           = c(0,  39, 39, 51),
    end             = c(3,  42, 42, 54),
    scientific_name = c("Parus major",
                        "Turdus merula", "Cyanistes caeruleus",
                        "Parus major"),
    common_name     = c("Great Tit", "Eurasian Blackbird",
                        "Eurasian Blue Tit", "Great Tit"),
    confidence      = c(0.8, 0.9, 0.6, 0.7),
    stringsAsFactors = FALSE
  )
  out <- run_with(pred)

  expect_equal(nrow(out), 4L)   # nothing dropped
  counts <- tapply(out$species_id, out$begin_time_ms, function(x) length(unique(x)))
  expect_equal(as.integer(counts[["39000"]]), 2L)  # multi-species window
  expect_equal(as.integer(counts[["0"]]),     1L)
  expect_equal(as.integer(counts[["51000"]]), 1L)
})

# --------------------------------------------------------------------------
# 3. Exact (window, species) duplicates DO collapse, keeping highest confidence
# --------------------------------------------------------------------------

test_that("duplicate (window, species) rows collapse to the highest confidence", {
  pred <- data.frame(
    start           = c(39,  39),
    end             = c(42,  42),
    scientific_name = c("Turdus merula", "Turdus merula"),  # same species, same window
    common_name     = c("Eurasian Blackbird", "Eurasian Blackbird"),
    confidence      = c(0.9,  0.5),
    stringsAsFactors = FALSE
  )
  out <- run_with(pred)

  expect_equal(nrow(out), 1L)
  expect_equal(out$species_id, 10L)
  expect_equal(out$confidence, 900L)   # round(0.9 * 1000): the higher-confidence copy
})

# --------------------------------------------------------------------------
# 4. Predictions for species outside possible_species are dropped
# --------------------------------------------------------------------------

test_that("species with no LUT match are filtered out, others retained", {
  pred <- data.frame(
    start           = c(39,  39),
    end             = c(42,  42),
    scientific_name = c("Turdus merula", "Unknown species"),
    common_name     = c("Eurasian Blackbird", "Unknown"),
    confidence      = c(0.9,  0.7),
    stringsAsFactors = FALSE
  )
  out <- run_with(pred)

  expect_equal(nrow(out), 1L)
  expect_equal(out$species_id, 10L)
})

# --------------------------------------------------------------------------
# 5. filter_unprocessed_audio_files(): settings-aware resume
# --------------------------------------------------------------------------

make_af <- function(ids = 1:4) {
  ids <- as.integer(ids)
  data.frame(audio_file_id = ids,
             full_path     = if (length(ids)) paste0("/f/", ids, ".wav") else character(0),
             stringsAsFactors = FALSE)
}

test_that("already-analysed files (this settings_id) are dropped", {
  stub(filter_unprocessed_audio_files, "DBI::dbGetQuery",
       data.frame(audio_file_id = c(2L, 3L)))
  out <- filter_unprocessed_audio_files(pool = list(), make_af(1:4), settings_id = 7L)
  expect_equal(sort(out$audio_file_id), c(1L, 4L))
})

test_that("nothing dropped when analysis_log has no rows for this settings", {
  stub(filter_unprocessed_audio_files, "DBI::dbGetQuery",
       data.frame(audio_file_id = integer(0)))
  out <- filter_unprocessed_audio_files(pool = list(), make_af(1:4), settings_id = 7L)
  expect_equal(sort(out$audio_file_id), 1:4)
})

test_that("empty input is returned unchanged without querying", {
  called <- FALSE
  stub(filter_unprocessed_audio_files, "DBI::dbGetQuery",
       function(...) { called <<- TRUE; data.frame(audio_file_id = integer(0)) })
  out <- filter_unprocessed_audio_files(pool = list(), make_af(integer(0)), settings_id = 7L)
  expect_equal(nrow(out), 0L)
  expect_false(called)
})

# --------------------------------------------------------------------------
# 6. process_deployment_birdnet(): all files skipped -> typed empty frame
# --------------------------------------------------------------------------

test_that("when resume skips every file, a typed 0-row result is returned", {
  ran_inference <- FALSE

  stub(process_deployment_birdnet, "pool::poolClose", function(...) invisible(NULL))
  stub(process_deployment_birdnet, "get_deployment_info",
       list(coordinates = c(7.5, 49.5)))
  stub(process_deployment_birdnet, "get_audio_files_for_deployment",
       data.frame(audio_file_id = 1:2, full_path = c("/a.wav", "/b.wav"),
                  week = -999L, stringsAsFactors = FALSE))
  stub(process_deployment_birdnet, "prepare_birdnet_model",
       list(bnm = list(species = NULL, model_name = "birdnet",
                       model_version = "v2.4"),
            birdnet_params = list()))
  stub(process_deployment_birdnet, "get_possible_species",
       data.frame(species_scientific = character(0), species_id = integer(0)))
  stub(process_deployment_birdnet, "upsert_birdnet_settings", 7L)
  # Resume drops both files.
  stub(process_deployment_birdnet, "filter_unprocessed_audio_files",
       function(pool, af, settings_id) af[0, , drop = FALSE])
  stub(process_deployment_birdnet, "run_birdnet_inference",
       function(...) { ran_inference <<- TRUE; data.frame() })

  out <- process_deployment_birdnet(
    deployment_id            = 1L,
    deployments              = NULL,
    audio_files              = NULL,
    species                  = NULL,
    temporal_filtering       = FALSE,
    occurence_min_confidence = 0,
    internal_pool            = list(),
    verbose                  = FALSE
  )

  expect_false(ran_inference)        # inference skipped entirely
  expect_equal(nrow(out), 0L)
  expect_equal(ncol(out), 9L)        # canonical result schema, writable by fst
  expect_true(all(c("audio_file_id", "settings_id", "species_id",
                    "error_type", "analysed_at") %in% names(out)))
})
