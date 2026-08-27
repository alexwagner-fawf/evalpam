library(testthat)
library(mockery)

# groups_from_spectrograms() — partition logic, DB layer stubbed out.

# A small metadata frame like .spectrogram_group_metadata() returns.
make_meta <- function() {
  data.frame(
    spectrogram_id     = 1:6,
    audio_file_id      = c(10, 10, 11, 11, 12, 12),
    result_id          = 1:6,
    species_id         = c(100, 100, 100, 200, 200, NA),
    species_scientific = c(rep("Turdus merula", 3), rep("Parus major", 2), NA),
    deployment_id      = c(1, 1, 2, 2, 2, 2),
    deployment_name    = c("A", "A", "B", "B", "B", "B"),
    site               = "POINT(7 49)",
    detection_time     = as.POSIXct("2026-03-01", tz = "UTC") +
                           c(0, 0, 0, 40, 0, 0) * 24 * 3600,
    date  = "2026-03-01",
    year  = "2026",
    month = c("2026-03", "2026-03", "2026-03", "2026-04", "2026-03", "2026-03"),
    week  = "2026-W09",
    biweek = "2026-BW05",
    stringsAsFactors = FALSE
  )
}

test_that("column group_by partitions and counts correctly", {
  created <- list()
  stub(groups_from_spectrograms, ".spectrogram_group_metadata", make_meta())
  stub(groups_from_spectrograms, "create_occupancy_group",
       function(pool, project_id, group_name, target_count, spectrogram_ids, description) {
         created[[group_name]] <<- spectrogram_ids
         length(created)  # a fake group_id
       })

  plan <- groups_from_spectrograms(pool = list(), project_id = 1,
                                   group_by = c("species_id", "deployment_id"),
                                   target_count = 2, verbose = FALSE)
  # species 100/dep1 (rows 1,2), species 100/dep2 (row 3), species 200/dep2
  # (rows 4,5); the NA-species row (6) is dropped -> 3 groups, 5 clips.
  expect_setequal(plan$group_key, c("100__1", "100__2", "200__2"))
  expect_setequal(plan$n_spectrograms, c(2, 1, 2))
  expect_equal(sum(plan$n_spectrograms), 5)
  expect_true(all(plan$target_count == 2))
  expect_false(any(is.na(plan$group_id)))
  # the species-100/dep-1 group carries both its spectrogram ids
  expect_equal(sort(created[["auto_100__1"]]), c(1L, 2L))
})

test_that("function group_by is honoured (lumping deployments)", {
  stub(groups_from_spectrograms, ".spectrogram_group_metadata", make_meta())
  stub(groups_from_spectrograms, "create_occupancy_group",
       function(...) 1L)
  # lump every deployment into one plot, split only by species. A user function
  # owns its own NA policy, so an NA species maps to the label it chose here.
  plan <- groups_from_spectrograms(pool = list(), project_id = 1,
                                   group_by = function(df) paste0("plot_", df$species_id),
                                   verbose = FALSE)
  # species 100 (3 clips), species 200 (2 clips), NA -> "plot_NA" (1 clip)
  expect_setequal(plan$group_key, c("plot_100", "plot_200", "plot_NA"))
  expect_equal(plan$n_spectrograms[plan$group_key == "plot_100"], 3)
})

test_that("dry_run returns the plan without creating anything", {
  called <- FALSE
  stub(groups_from_spectrograms, ".spectrogram_group_metadata", make_meta())
  stub(groups_from_spectrograms, "create_occupancy_group",
       function(...) { called <<- TRUE; 1L })
  plan <- groups_from_spectrograms(pool = list(), project_id = 1,
                                   group_by = "deployment_id",
                                   dry_run = TRUE, verbose = FALSE)
  expect_false(called)
  expect_true(all(is.na(plan$group_id)))
  expect_setequal(plan$n_spectrograms, c(2, 4))  # dep1=2, dep2=4
})

test_that("rows with an incomplete grouping key are dropped", {
  stub(groups_from_spectrograms, ".spectrogram_group_metadata", make_meta())
  stub(groups_from_spectrograms, "create_occupancy_group", function(...) 1L)
  plan <- groups_from_spectrograms(pool = list(), project_id = 1,
                                   group_by = "species_id", verbose = FALSE)
  # NA-species row excluded -> only species 100 and 200
  expect_setequal(plan$group_key, c("100", "200"))
  expect_equal(sum(plan$n_spectrograms), 5)  # 6 rows minus the 1 NA-species row
})

test_that("unknown group_by column errors with the available columns listed", {
  stub(groups_from_spectrograms, ".spectrogram_group_metadata", make_meta())
  expect_error(
    groups_from_spectrograms(pool = list(), project_id = 1,
                             group_by = "nope", verbose = FALSE),
    "not available"
  )
})

test_that("group_by function returning wrong length errors", {
  stub(groups_from_spectrograms, ".spectrogram_group_metadata", make_meta())
  expect_error(
    groups_from_spectrograms(pool = list(), project_id = 1,
                             group_by = function(df) "one-label", verbose = FALSE),
    "one label per row"
  )
})
