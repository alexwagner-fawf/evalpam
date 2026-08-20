# Tests for the explicit species-list filter:
#   resolve_species_filter_labels()  — id -> BirdNET label resolution + abort
#   prepare_birdnet_model()          — settings identity keys in list mode
#
# All pure R: no Python / model backend required.

library(testthat)
library(mockery)

# A tiny stand-in LUT with the columns the resolver relies on.
make_lut <- function() {
  data.frame(
    species_id         = c(10L, 20L, 30L),
    species_scientific = c("Turdus merula",
                           "Cyanistes caeruleus",
                           "Parus major"),
    stringsAsFactors = FALSE
  )
}

# Model labels in BirdNET "Genus species_Common Name" format.
make_labels <- function() {
  c("Turdus merula_Eurasian Blackbird",
    "Cyanistes caeruleus_Eurasian Blue Tit",
    "Parus major_Great Tit",
    "Erithacus rubecula_European Robin")
}

# --------------------------------------------------------------------------
# resolve_species_filter_labels()
# --------------------------------------------------------------------------

test_that("resolves ids to the matching BirdNET labels", {
  labs <- resolve_species_filter_labels(c(10L, 30L), make_lut(), make_labels())
  expect_setequal(labs,
                  c("Turdus merula_Eurasian Blackbird", "Parus major_Great Tit"))
})

test_that("de-duplicates and coerces ids", {
  labs <- resolve_species_filter_labels(c(10, 10, 20), make_lut(), make_labels())
  expect_setequal(labs,
                  c("Turdus merula_Eurasian Blackbird",
                    "Cyanistes caeruleus_Eurasian Blue Tit"))
})

test_that("aborts when an id is absent from the LUT", {
  expect_error(
    resolve_species_filter_labels(c(10L, 999L), make_lut(), make_labels()),
    regexp = "not found in lut_species_code.*999"
  )
})

test_that("aborts when a requested species is not in the model label set", {
  # Add a LUT species the model cannot detect.
  lut <- rbind(make_lut(),
               data.frame(species_id = 40L,
                          species_scientific = "Bubo bubo",
                          stringsAsFactors = FALSE))
  expect_error(
    resolve_species_filter_labels(c(10L, 40L), lut, make_labels()),
    regexp = "not detectable by this BirdNET model.*Bubo bubo"
  )
})

test_that("aborts when no usable ids are supplied", {
  expect_error(
    resolve_species_filter_labels(c(NA, NA), make_lut(), make_labels()),
    regexp = "no usable"
  )
})

# --------------------------------------------------------------------------
# prepare_birdnet_model(): settings identity in list mode vs spatial mode
# --------------------------------------------------------------------------

# Stub setup_birdnet_model so no model backend is needed.
stub_setup <- function() {
  function(...) list(model_name = "birdnet", model_version = "v2.4",
                     species = NULL, model = list(dummy = TRUE))
}

test_that("spatial mode keeps coordinates and marks species_filter none", {
  stub(prepare_birdnet_model, "setup_birdnet_model", stub_setup())
  out <- prepare_birdnet_model(latitude = 52.5, longitude = 13.4, week = 20L,
                               min_confidence = 0.03, tflite_num_threads = 1L)
  p <- out$birdnet_params
  expect_equal(p$latitude, 52.5)
  expect_equal(p$longitude, 13.4)
  expect_equal(p$week, 20L)
  expect_equal(p$ebird_min_confidence, 0.03)
  expect_equal(p$species_filter, "none")
})

test_that("list mode nulls the spatiotemporal keys and records the hash", {
  stub(prepare_birdnet_model, "setup_birdnet_model", stub_setup())
  out <- prepare_birdnet_model(latitude = NULL, longitude = NULL, week = NULL,
                               min_confidence = 0, tflite_num_threads = 1L,
                               species_filter_hash = "abc123")
  p <- out$birdnet_params
  expect_true(is.na(p$latitude))
  expect_true(is.na(p$longitude))
  expect_true(is.na(p$week))
  expect_true(is.na(p$ebird_min_confidence))
  expect_equal(p$species_filter, "abc123")
})

test_that("list-mode NA keys serialise to JSON null, not the string NA", {
  stub(prepare_birdnet_model, "setup_birdnet_model", stub_setup())
  out <- prepare_birdnet_model(latitude = NULL, longitude = NULL, week = NULL,
                               min_confidence = 0, tflite_num_threads = 1L,
                               species_filter_hash = "abc123")
  js <- jsonlite::toJSON(out$birdnet_params, auto_unbox = TRUE, na = "null")
  expect_match(as.character(js), "\"latitude\":null")
  expect_match(as.character(js), "\"species_filter\":\"abc123\"")
  expect_false(grepl("\"NA\"", as.character(js)))
})
