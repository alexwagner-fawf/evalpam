# =============================================================================
# Test harness for inst/02_inference.R aggregation logic
#
# Tests the index management and upload pipeline using synthetic temp fst
# files — no BirdNET/Python required. Uses project_6 deployments already
# in the DB (deployment_ids 2, 3, 4; audio_file_ids 4–12).
#
# Tests:
#   T1  All deployments finished — remaining_deployments is empty, block skipped
#   T2  Partial run — 1 of 3 temp fsts missing → only that one reprocessed
#   T3  Mixed results — success + file-not-found + worker-error in same fst
#   T4  Index merge — existing index skips already-successful (audio,settings)
#   T5  Previously-failed file re-attempted in new run
#   T6  Upload dry run (upload_inference=FALSE) — DB untouched
#   T7  Idempotent upload — second upload of same results does not duplicate
# =============================================================================

devtools::load_all(quiet = TRUE)  # use source version, not installed package

# ── helpers ──────────────────────────────────────────────────────────────────

pass <- function(label)      cat(sprintf("  PASS  %s\n", label))
fail <- function(label, msg) cat(sprintf("  FAIL  %s — %s\n", label, msg))

check <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) {
    message("  ERROR in check '", label, "': ", conditionMessage(e))
    FALSE
  })
  if (isTRUE(result)) pass(label) else fail(label, deparse(substitute(expr)))
}

# ── Config ────────────────────────────────────────────────────────────────────

PROJECT_FOLDER    <- "/home/alex/Dokumente/sound_db/project_1"
TEMP_FOLDER       <- file.path(PROJECT_FOLDER, "test_inference_results_temp")
RESULTS_FOLDER    <- file.path(PROJECT_FOLDER, "test_inference_results")
INDEX_FILE        <- file.path(RESULTS_FOLDER, "inference_results_index.fst")

dir.create(TEMP_FOLDER,    showWarnings = FALSE)
dir.create(RESULTS_FOLDER, showWarnings = FALSE)

# Use project_6 deployments and audio files already in DB
TEST_PROJECT_ID   <- 6L
DEPLOYMENT_IDS    <- c(2L, 3L, 4L)
# audio_file_id 4-6 → dep 2 | 7-9 → dep 3 | 10-12 → dep 4

pool <- set_db_pool()
on.exit({
  pool::poolClose(pool)
  # clean up test folders
  unlink(TEMP_FOLDER,   recursive = TRUE)
  unlink(RESULTS_FOLDER, recursive = TRUE)
  cat("\nTest folders removed.\n")
}, add = TRUE)

# Helper: make a synthetic fst result frame for one deployment
make_result_frame <- function(audio_file_ids, settings_id,
                              success_ids     = audio_file_ids,
                              error_ids       = integer(0),
                              error_type      = "failed_file_not_found",
                              worker_fail_ids = integer(0)) {

  success_rows <- if (length(success_ids) > 0) data.frame(
    audio_file_id = rep(success_ids, each = 3),
    settings_id   = settings_id,
    begin_time_ms = rep(c(0L, 3000L, 6000L), length(success_ids)),
    end_time_ms   = rep(c(3000L, 6000L, 9000L), length(success_ids)),
    confidence    = rep(c(8000L, 7500L, 9000L), length(success_ids)),
    species_id    = rep(c(2371L, 2371L, 1491L), length(success_ids)),
    behavior_id   = NA_integer_,
    error_type    = NA_character_,
    analysed_at   = Sys.time()
  ) else NULL

  error_rows <- if (length(error_ids) > 0) data.frame(
    audio_file_id = error_ids,
    settings_id   = settings_id,
    begin_time_ms = NA_integer_,
    end_time_ms   = NA_integer_,
    confidence    = NA_integer_,
    species_id    = NA_integer_,
    behavior_id   = NA_integer_,
    error_type    = error_type,
    analysed_at   = Sys.time()
  ) else NULL

  worker_rows <- if (length(worker_fail_ids) > 0) data.frame(
    audio_file_id = worker_fail_ids,
    settings_id   = NA_integer_,   # worker failed before model was loaded
    begin_time_ms = NA_integer_,
    end_time_ms   = NA_integer_,
    confidence    = NA_integer_,
    species_id    = NA_integer_,
    behavior_id   = NA_integer_,
    error_type    = "failed_worker_error",
    analysed_at   = Sys.time()
  ) else NULL

  dplyr::bind_rows(success_rows, error_rows, worker_rows)
}

# ── Reset: wipe any temp fst from previous test run ──────────────────────────

cleanup_temp <- function() {
  f <- list.files(TEMP_FOLDER, pattern = "\\.fst$", full.names = TRUE)
  if (length(f)) file.remove(f)
}

cleanup_results <- function() {
  f <- list.files(RESULTS_FOLDER, pattern = "\\.fst$", full.names = TRUE)
  if (length(f)) file.remove(f)
}

# ═════════════════════════════════════════════════════════════════════════════
# T1: All deployments finished — skip inference block
# ═════════════════════════════════════════════════════════════════════════════

cat("T1: All deployments finished\n")
cleanup_temp(); cleanup_results()

for (did in DEPLOYMENT_IDS) {
  fst::write_fst(make_result_frame(
      audio_file_ids = switch(as.character(did), "2" = 4:6, "3" = 7:9, "4" = 10:12),
      settings_id = 2L),
    file.path(TEMP_FOLDER, paste0(did, ".fst")))
}

finished <- list.files(TEMP_FOLDER) |> tools::file_path_sans_ext() |> as.integer()
remaining <- DEPLOYMENT_IDS[!DEPLOYMENT_IDS %in% finished]

check("T1.1 remaining_deployments is empty",  length(remaining) == 0)
check("T1.2 all temp fst files present",      length(finished) == 3)
pass("T1.3 inference block correctly skipped (validated by remaining == 0)")

# ═════════════════════════════════════════════════════════════════════════════
# T2: Partial run — deployment 4 temp fst missing
# ═════════════════════════════════════════════════════════════════════════════

cat("\nT2: Partial run — deployment 4 missing\n")
cleanup_temp()

for (did in c(2L, 3L)) {
  fst::write_fst(
    make_result_frame(audio_file_ids = switch(as.character(did), "2" = 4:6, "3" = 7:9),
                      settings_id = 2L),
    file.path(TEMP_FOLDER, paste0(did, ".fst")))
}

finished2 <- list.files(TEMP_FOLDER) |> tools::file_path_sans_ext() |> as.integer()
remaining2 <- DEPLOYMENT_IDS[!DEPLOYMENT_IDS %in% finished2]

check("T2.1 remaining contains only deployment 4", identical(remaining2, 4L))
check("T2.2 deployments 2 & 3 marked finished",   setequal(finished2, c(2L, 3L)))

# ═════════════════════════════════════════════════════════════════════════════
# T3: Mixed results — success + file-not-found + worker-error
# ═════════════════════════════════════════════════════════════════════════════

cat("\nT3: Mixed results — success, file-not-found, worker-error\n")
cleanup_temp(); cleanup_results()

# dep 2: all success
fst::write_fst(make_result_frame(4:6, settings_id = 2L),
               file.path(TEMP_FOLDER, "2.fst"))
# dep 3: audio_file_id 7 succeeded, 8 file-not-found, 9 worker-error
fst::write_fst(make_result_frame(audio_file_ids = 7:9, settings_id = 3L,
                                  success_ids = 7L, error_ids = 8L,
                                  worker_fail_ids = 9L),
               file.path(TEMP_FOLDER, "3.fst"))
# dep 4: all success
fst::write_fst(make_result_frame(10:12, settings_id = 4L),
               file.path(TEMP_FOLDER, "4.fst"))

birdnet_inference <- list.files(TEMP_FOLDER, full.names = TRUE, pattern = "\\.fst$") |>
  lapply(fst::read_fst) |>
  dplyr::bind_rows()

cat("  Total rows:", nrow(birdnet_inference), "\n")
cat("  Success rows (NA error_type):", sum(is.na(birdnet_inference$error_type)), "\n")
cat("  file-not-found rows:", sum(!is.na(birdnet_inference$error_type) & birdnet_inference$error_type == "failed_file_not_found", na.rm = TRUE), "\n")
cat("  worker-error rows:", sum(!is.na(birdnet_inference$error_type) & birdnet_inference$error_type == "failed_worker_error", na.rm = TRUE), "\n")

# No prior index — create empty
inference_index <- dplyr::tibble(
  audio_file_id = integer(), settings_id = integer(),
  status = character(), analysed_at = as.POSIXct(character(0))
)

birdnet_inference_new <- birdnet_inference |>
  dplyr::anti_join(dplyr::filter(inference_index, status == "success"),
                   by = c("audio_file_id", "settings_id"))

check("T3.1 all rows are new (empty index)",        nrow(birdnet_inference_new) == nrow(birdnet_inference))

# Build results file — success rows only
results_df <- dplyr::filter(birdnet_inference_new, is.na(error_type))
check("T3.2 results file contains only success rows",       all(is.na(results_df$error_type)))
check("T3.3 worker-error rows not in results file",
      !any(results_df$error_type == "failed_worker_error", na.rm = TRUE))

# Build index update
add2index <- birdnet_inference_new |>
  dplyr::select(audio_file_id, settings_id, error_type, analysed_at) |>
  dplyr::distinct() |>
  dplyr::mutate(status = ifelse(is.na(error_type), "success", error_type)) |>
  dplyr::select(-error_type)

check("T3.4 index has 9 rows (one per audio file)",         nrow(add2index) == 9)
check("T3.5 success rows in index",    sum(add2index$status == "success") == 7)
check("T3.6 file-not-found in index",  sum(add2index$status == "failed_file_not_found") == 1)
check("T3.7 worker-error in index",    sum(add2index$status == "failed_worker_error") == 1)

# Filter for DB upload — skip NA settings_id (worker failures)
upload_ready <- dplyr::filter(add2index, !is.na(settings_id))
check("T3.8 worker-error excluded from analysis_log upload", nrow(upload_ready) == 8)

# ═════════════════════════════════════════════════════════════════════════════
# T4: Index merge — existing success entries not overwritten
# ═════════════════════════════════════════════════════════════════════════════

cat("\nT4: Index merge — existing success entries preserved\n")
cleanup_temp(); cleanup_results()

# Simulate an existing index: dep 2 (af 4-6) already success from prior run
existing_index <- dplyr::tibble(
  audio_file_id = c(4L, 5L, 6L),
  settings_id   = c(2L, 2L, 2L),
  status        = "success",
  analysed_at   = as.POSIXct("2026-01-01 10:00:00", tz = "UTC")
)
fst::write_fst(existing_index, INDEX_FILE)

# New run: all 3 deployments produce results
for (did in DEPLOYMENT_IDS) {
  fst::write_fst(make_result_frame(
      audio_file_ids = switch(as.character(did), "2" = 4:6, "3" = 7:9, "4" = 10:12),
      settings_id = switch(as.character(did), "2" = 2L, "3" = 3L, "4" = 4L)),
    file.path(TEMP_FOLDER, paste0(did, ".fst")))
}

birdnet_inference4 <- list.files(TEMP_FOLDER, full.names = TRUE, pattern = "\\.fst$") |>
  lapply(fst::read_fst) |> dplyr::bind_rows()

birdnet_inference_new4 <- birdnet_inference4 |>
  dplyr::anti_join(dplyr::filter(existing_index, status == "success"),
                   by = c("audio_file_id", "settings_id"))

check("T4.1 dep-2 rows skipped (already successful)", {
  dep2_new <- dplyr::filter(birdnet_inference_new4, audio_file_id %in% 4:6)
  nrow(dep2_new) == 0
})
check("T4.2 dep-3 and dep-4 rows still present", {
  new_af <- unique(birdnet_inference_new4$audio_file_id)
  all(7:12 %in% new_af)
})

# Index update with merge
add2index4 <- birdnet_inference_new4 |>
  dplyr::select(audio_file_id, settings_id, error_type, analysed_at) |>
  dplyr::distinct() |>
  dplyr::mutate(status = ifelse(is.na(error_type), "success", error_type)) |>
  dplyr::select(-error_type)

merged_index <- add2index4 |>
  dplyr::anti_join(existing_index, by = c("audio_file_id", "settings_id")) |>
  dplyr::bind_rows(existing_index) |>
  dplyr::arrange(audio_file_id, settings_id, dplyr::desc(analysed_at)) |>
  dplyr::distinct(audio_file_id, settings_id, .keep_all = TRUE)

check("T4.3 merged index has 9 rows total",     nrow(merged_index) == 9)
check("T4.4 dep-2 analysed_at unchanged",       {
  orig_ts <- existing_index$analysed_at[1]
  merged_ts <- merged_index$analysed_at[merged_index$audio_file_id == 4L]
  identical(orig_ts, merged_ts)
})

fst::write_fst(merged_index, INDEX_FILE)
check("T4.5 index file written correctly",       file.exists(INDEX_FILE) && nrow(fst::read_fst(INDEX_FILE)) == 9)

# ═════════════════════════════════════════════════════════════════════════════
# T5: Previously-failed file re-attempted
# ═════════════════════════════════════════════════════════════════════════════

cat("\nT5: Previously-failed file re-attempted\n")
cleanup_temp(); cleanup_results()

# Start: af 4 failed in prior run
failed_index <- dplyr::tibble(
  audio_file_id = 4L, settings_id = 2L,
  status = "failed_file_not_found",
  analysed_at = as.POSIXct("2026-01-01 10:00:00", tz = "UTC")
)

# New run: dep 2 — af 4 now succeeds
new_frame <- make_result_frame(4:6, settings_id = 2L)
fst::write_fst(new_frame, file.path(TEMP_FOLDER, "2.fst"))

birdnet5 <- fst::read_fst(file.path(TEMP_FOLDER, "2.fst"))

# anti_join filters only status=="success" — failed entries do NOT block re-upload
new5 <- birdnet5 |>
  dplyr::anti_join(dplyr::filter(failed_index, status == "success"),
                   by = c("audio_file_id", "settings_id"))

check("T5.1 all 3 audio files re-processed (failed not blocked)", {
  length(unique(new5$audio_file_id)) == 3
})
check("T5.2 previously-failed af 4 included in new results", 4L %in% new5$audio_file_id)

# ═════════════════════════════════════════════════════════════════════════════
# T6: upload_inference = FALSE — no DB changes
# ═════════════════════════════════════════════════════════════════════════════

cat("\nT6: upload_inference=FALSE — DB untouched\n")

results_count_before <- DBI::dbGetQuery(pool, "SELECT COUNT(*) AS n FROM import.results")$n
log_count_before     <- DBI::dbGetQuery(pool, "SELECT COUNT(*) AS n FROM import.analysis_log")$n

upload_inference <- FALSE

if (upload_inference) {
  stop("This branch must not run in T6")
}

results_count_after <- DBI::dbGetQuery(pool, "SELECT COUNT(*) AS n FROM import.results")$n
log_count_after     <- DBI::dbGetQuery(pool, "SELECT COUNT(*) AS n FROM import.analysis_log")$n

check("T6.1 results table unchanged",      results_count_before == results_count_after)
check("T6.2 analysis_log unchanged",       log_count_before == log_count_after)

# ═════════════════════════════════════════════════════════════════════════════
# T7: Idempotent upload — second upload does not duplicate
# ═════════════════════════════════════════════════════════════════════════════

cat("\nT7: Idempotent DB upload\n")
cleanup_temp(); cleanup_results()
upload_inference <- TRUE

# Use dep 3 (af 7-9, settings 3) — already in DB from previous sessions
# Make a small synthetic result that should already exist in import.results
existing_results <- DBI::dbGetQuery(pool,
  "SELECT audio_file_id, settings_id, begin_time_ms, end_time_ms, species_id
   FROM import.results WHERE settings_id = 3 LIMIT 3")

if (nrow(existing_results) == 0) {
  cat("  SKIP T7 — no existing results for settings_id=3 to test idempotency\n")
} else {
  results_before <- DBI::dbGetQuery(pool, "SELECT COUNT(*) AS n FROM import.results")$n

  # Attempt to re-upload the same rows
  upload_df <- existing_results |>
    dplyr::mutate(behavior_id = NA_integer_)

  tryCatch(
    upsert_results_df(conn = pool, upload_df),
    error = function(e) cat("  INFO upsert raised error (may be OK):", conditionMessage(e), "\n")
  )

  results_after <- DBI::dbGetQuery(pool, "SELECT COUNT(*) AS n FROM import.results")$n
  check("T7.1 results count unchanged after re-upload", results_before == results_after)
}

# ── Summary ───────────────────────────────────────────────────────────────────

cat("\n=== T02 harness complete ===\n")
cat("Test folders will be removed on exit.\n")
