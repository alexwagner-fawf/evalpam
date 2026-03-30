# =============================================================================
# Test harness for inst/01_get_audio_files.R logic
#
# Tests retrieve_local_file_info() and the audio-file upload pipeline against
# the sound_db test data. Covers:
#   T1  Fresh scan — depth=1, flat structure (project_1)
#   T2  Re-scan skip — fst files already exist → new_audio_file_indices empty
#   T3  force_exif = TRUE — fst files ignored, everything re-read
#   T4a Edge case: empty WAV  → NA sample_rate → dropped before upload
#   T4b Edge case: corrupt WAV → exiftool handles gracefully (NA or error)
#   T4c Edge case: duplicate timestamp → dedup logic resolves to longer file
#   T4d Edge case: dead symlink → file unreachable by exiftool
#   T5  depth=2 nested structure (project_2)
#   T6  DB round-trip — upsert_deployments_sf + upsert_audio_files_df
#   T7  Re-upload idempotency — second upload produces same audio_file_ids
#
# Edge case files must be present (run setup below once):
#   location1_timeinterval1/EMPTY_20240526_060000.wav       (0 bytes)
#   location1_timeinterval1/CORRUPT_20240526_060500.wav     (random 512 bytes)
#   location1_timeinterval1/KW2100ALA1B_BACKUP_20240526_053500.wav (copy, same ts)
#   location1_timeinterval1/KW2100ALA1B_20240526_061000.wav (dead symlink)
# =============================================================================

devtools::load_all(quiet = TRUE)  # use source version, not installed package

# ── helpers ──────────────────────────────────────────────────────────────────

pass <- function(label) cat(sprintf("  PASS  %s\n", label))
fail <- function(label, msg) cat(sprintf("  FAIL  %s — %s\n", label, msg))

check <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) FALSE)
  if (isTRUE(result)) pass(label) else fail(label, deparse(substitute(expr)))
}

PROJECT_1 <- "/home/alex/Dokumente/sound_db/project_1"
PROJECT_2 <- "/home/alex/Dokumente/sound_db/project_2"
AFI_DIR_1 <- file.path(PROJECT_1, "audio_file_indices")
AFI_DIR_2 <- file.path(PROJECT_2, "audio_file_indices")

# ── DB connection ─────────────────────────────────────────────────────────────

pool <- set_db_pool()
on.exit(pool::poolClose(pool), add = TRUE)

# Create a dedicated test project so we don't touch existing data
TEST_PROJECT_SHORT <- paste0("test_01_", format(Sys.time(), "%H%M%S"))
test_project_id <- evalpam::upsert_project(
  conn               = pool,
  project_name_short = TEST_PROJECT_SHORT,
  project_name_long  = "Automated test — 01_get_audio_files harness",
  description        = "Created by test_01_harness.R — safe to delete",
  contact            = "test",
  organisation       = "test"
)
cat("Test project_id:", test_project_id, "\n\n")

# ── T1: Fresh scan — depth=1 ──────────────────────────────────────────────────

cat("T1: Fresh scan (depth=1, project_1)\n")

# Remove any existing afi fst files so the scan starts fresh
existing_afi <- list.files(AFI_DIR_1, pattern = "\\.fst$", full.names = TRUE)
if (length(existing_afi) > 0) {
  file.remove(existing_afi)
  cat("  Removed", length(existing_afi), "existing afi fst(s)\n")
}

t1 <- retrieve_local_file_info(
  project_id                          = test_project_id,
  project_folder                      = PROJECT_1,
  folder_depth                        = 1L,
  force_tz                            = "UTC",
  force_exif                          = FALSE,
  list_files_retries                  = 3L,
  list_files_verify                   = FALSE,   # local disk, no verification needed
  default_required_annotation_type_id = 3L
)

check("T1.1 returns named list",          is.list(t1) && all(c("new_audio_file_indices","all_audio_file_indices","deployment_index") %in% names(t1)))
check("T1.2 deployment_index CSV exists", file.exists(t1$deployment_index))
check("T1.3 3 deployments found",         {
  di <- readr::read_csv(t1$deployment_index, show_col_types = FALSE)
  nrow(di) == 3
})
check("T1.4 3 new afi fst files created", length(t1$new_audio_file_indices) == 3)
check("T1.5 all == new on fresh scan",    length(t1$all_audio_file_indices) == length(t1$new_audio_file_indices))

# Check that edge-case files are present in the raw scan but handled
afi_loc1 <- t1$all_audio_file_indices[grepl("location1", t1$all_audio_file_indices)]
if (length(afi_loc1) > 0) {
  raw <- fst::read_fst(afi_loc1)
  cat("  location1 afi rows (including edge cases):", nrow(raw), "\n")
  cat("  NA sample_rate rows:", sum(is.na(raw$sample_rate)), "\n")
  cat("  Files found:", basename(raw$relative_path), "\n")
}

# ── T2: Re-scan skip ──────────────────────────────────────────────────────────

cat("\nT2: Re-scan — fst files exist, force_exif=FALSE\n")

t2 <- retrieve_local_file_info(
  project_id     = test_project_id,
  project_folder = PROJECT_1,
  folder_depth   = 1L,
  force_tz       = "UTC",
  force_exif     = FALSE,
  list_files_verify = FALSE
)

check("T2.1 new_audio_file_indices is empty (skipped)", length(t2$new_audio_file_indices) == 0)
check("T2.2 all_audio_file_indices still 3",            length(t2$all_audio_file_indices) == 3)

# ── T3: force_exif = TRUE ─────────────────────────────────────────────────────

cat("\nT3: force_exif=TRUE — re-reads even if fst exists\n")

t3 <- retrieve_local_file_info(
  project_id     = test_project_id,
  project_folder = PROJECT_1,
  folder_depth   = 1L,
  force_tz       = "UTC",
  force_exif     = TRUE,
  list_files_verify = FALSE
)

check("T3.1 all 3 fst files regenerated", length(t3$new_audio_file_indices) == 3)

# ── T4: Edge cases ────────────────────────────────────────────────────────────

cat("\nT4: Edge cases in location1_timeinterval1\n")

afi_loc1_path <- t3$all_audio_file_indices[grepl("location1_timeinterval1", t3$all_audio_file_indices)]

if (length(afi_loc1_path) == 0) {
  cat("  SKIP  T4 — could not locate location1 afi fst\n")
} else {
  afi1 <- fst::read_fst(afi_loc1_path)
  cat("  Raw rows in location1 afi:", nrow(afi1), "\n")

  # T4a: empty file — exiftool returns no duration/sample_rate → NA
  empty_rows <- afi1[grepl("EMPTY", afi1$relative_path, ignore.case = TRUE), ]
  if (nrow(empty_rows) > 0) {
    check("T4a empty WAV → NA sample_rate", is.na(empty_rows$sample_rate[1]))
  } else {
    cat("  INFO  T4a — empty WAV not found in afi (may have been skipped by exiftool)\n")
  }

  # T4b: corrupt file — exiftool may return NA sample_rate
  corrupt_rows <- afi1[grepl("CORRUPT", afi1$relative_path, ignore.case = TRUE), ]
  if (nrow(corrupt_rows) > 0) {
    cat("  INFO  T4b corrupt WAV sample_rate:", corrupt_rows$sample_rate[1], "\n")
    pass("T4b corrupt WAV handled (no crash)")
  } else {
    cat("  INFO  T4b — corrupt WAV not in afi (exiftool may have skipped it)\n")
    pass("T4b corrupt WAV handled (not indexed)")
  }

  # T4c: duplicate timestamp — BACKUP and original share 2024-05-26 05:35:00
  ts_check <- as.POSIXct(afi1$timestamp_start, origin = "1970-01-01", tz = "UTC")
  dup_ts <- ts_check[duplicated(ts_check) | duplicated(ts_check, fromLast = TRUE)]
  check("T4c duplicate timestamp detected in raw afi", length(dup_ts) >= 2)

  # T4d: dead symlink — should not appear in scan (non-resolvable)
  dead_rows <- afi1[grepl("061000", afi1$relative_path), ]
  cat("  INFO  T4d dead symlink rows in afi:", nrow(dead_rows),
      "(0 = skipped by exiftool, >0 = included with NA fields)\n")
  pass("T4d dead symlink handled (no crash)")
}

# Simulate the full 01_get_audio_files.R upload pipeline with edge-case data
cat("\n  Simulating upload pipeline with edge-case data:\n")

afi_all <- t3$all_audio_file_indices |> lapply(fst::read_fst) |> dplyr::bind_rows()
cat("  Total raw afi rows across all deployments:", nrow(afi_all), "\n")

# Build pam_locs equivalent: use project_6 coordinates as stand-ins
pam_locs <- data.frame(
  deployment_name  = c("project_1_location1_timeinterval1",
                       "project_1_location2_timeinterval2",
                       "project_1_location3_timeinterval3"),
  notes            = NA_character_,      # required by upsert_deployments_sf
  geometry_x_4326  = c(7.955363, 7.919819, 7.081097),
  geometry_y_4326  = c(49.08278, 49.18532, 49.24050),
  stringsAsFactors = FALSE
)

di <- readr::read_csv(t3$deployment_index, show_col_types = FALSE)

deployments_sf <- di |>
  # deployment_index.csv already has geometry_x/y_4326 columns (written as NA);
  # drop them before joining so pam_locs values take precedence without .x/.y suffixes
  dplyr::select(-dplyr::any_of(c("geometry_x_4326", "geometry_y_4326", "notes"))) |>
  dplyr::left_join(pam_locs, by = "deployment_name") |>
  dplyr::filter(!is.na(geometry_y_4326)) |>
  dplyr::mutate(
    project_id          = test_project_id,
    valid               = TRUE,
    device_manufacturer = "Wildlife Acoustics",
    device_modelname    = "SongMeter Mini 2 Li"
  ) |>
  sf::st_as_sf(coords = c("geometry_x_4326", "geometry_y_4326")) |>
  sf::st_set_crs(4326)

check("T4e all 3 deployments have coords after join", nrow(deployments_sf) == 3)

upserted_dep_ids <- upsert_deployments_sf(deployments_sf, conn = pool)
check("T4f 3 deployment IDs returned",               length(upserted_dep_ids) == 3)

# Build audio file data frame — mirrors the 01_get_audio_files.R pipeline
db_deps <- DBI::dbGetQuery(pool, sprintf(
  "SELECT deployment_id, deployment_name FROM import.deployments WHERE deployment_id IN (%s)",
  paste(upserted_dep_ids, collapse = ",")
))

df <- afi_all |>
  dplyr::rename(deployment_name = deployment_id) |>
  dplyr::left_join(db_deps, by = "deployment_name") |>
  dplyr::filter(deployment_id %in% upserted_dep_ids) |>
  dplyr::mutate(timestamp_start = as.POSIXct(timestamp_start, origin = "1970-01-01", tz = "UTC")) |>
  dplyr::filter(!is.na(sample_rate)) |>   # T4a/T4b: drops empty + corrupt rows
  dplyr::select(-deployment_name)

cat("  Rows after NA sample_rate filter:", nrow(df), "\n")
check("T4g NA sample_rate rows removed",  {
  n_na <- sum(is.na(df$sample_rate))
  n_na == 0
})

# Duplicate handling (T4c)
df_grouped <- df |>
  dplyr::group_by(deployment_id) |>
  dplyr::mutate(dupls = duplicated(timestamp_start) | duplicated(timestamp_start, fromLast = TRUE))

n_dups <- sum(df_grouped$dupls)
cat("  Duplicate timestamp rows:", n_dups, "\n")
check("T4h duplicates flagged correctly", n_dups >= 2)

# Resolve duplicates: keep longest duration per (deployment_id, timestamp_start)
df_deduped <- df_grouped |>
  dplyr::filter(dupls) |>
  dplyr::arrange(dplyr::desc(duration_s)) |>
  dplyr::group_by(deployment_id, timestamp_start) |>
  dplyr::slice_head(n = 1) |>
  dplyr::bind_rows(dplyr::filter(df_grouped, !dupls)) |>
  dplyr::ungroup() |>
  dplyr::select(-dupls)

check("T4i no duplicate timestamps after dedup", {
  !any(duplicated(paste(df_deduped$deployment_id, df_deduped$timestamp_start)))
})

na_ts_rows <- sum(is.na(df_deduped$timestamp_start))
cat("  Rows with NA timestamp_start:", na_ts_rows, "\n")
check("T4i2 no NA timestamp_start rows", na_ts_rows == 0)

df_deduped_clean <- dplyr::filter(df_deduped, !is.na(timestamp_start))
af_ids <- upsert_audio_files_df(conn = pool, df_audio = df_deduped_clean, update_if_exists = TRUE)
cat("  Uploaded audio_file_ids:", length(af_ids), "\n")
check("T4j audio files uploaded",           length(af_ids) > 0)

# ── T5: depth=2 nested structure (project_2) ─────────────────────────────────

cat("\nT5: depth=2 scan (project_2 nested structure)\n")

existing_afi2 <- list.files(AFI_DIR_2, pattern = "\\.fst$", full.names = TRUE)
if (length(existing_afi2) > 0) file.remove(existing_afi2)

t5 <- retrieve_local_file_info(
  project_id     = test_project_id,
  project_folder = PROJECT_2,
  folder_depth   = 2L,
  force_tz       = "UTC",
  force_exif     = TRUE,
  list_files_verify = FALSE
)

check("T5.1 2 deployments found at depth=2", {
  di5 <- readr::read_csv(t5$deployment_index, show_col_types = FALSE)
  nrow(di5) == 2
})
check("T5.2 2 new afi fst files created",    length(t5$new_audio_file_indices) == 2)
check("T5.3 correct audio file counts", {
  rows <- t5$all_audio_file_indices |> lapply(fst::read_fst) |> dplyr::bind_rows()
  nrow(rows) == 6   # 3 files × 2 deployments
})

# ── T6: DB round-trip integrity ───────────────────────────────────────────────

cat("\nT6: DB round-trip — verify uploaded data is readable\n")

af_check <- DBI::dbGetQuery(pool, sprintf(
  "SELECT af.audio_file_id, af.deployment_id, af.timestamp_start, af.sample_rate
   FROM import.audio_files af
   JOIN import.deployments d USING(deployment_id)
   WHERE d.project_id = %d
   ORDER BY af.audio_file_id",
  test_project_id
))
cat("  Audio files in DB for test project:", nrow(af_check), "\n")
check("T6.1 sample_rates are all 48000",      all(af_check$sample_rate == 48000))
check("T6.2 no duplicate timestamps per dep", {
  !any(duplicated(paste(af_check$deployment_id, af_check$timestamp_start)))
})

# ── T7: Re-upload idempotency ─────────────────────────────────────────────────

cat("\nT7: Re-upload idempotency\n")

af_ids2 <- upsert_audio_files_df(conn = pool, df_audio = df_deduped, update_if_exists = TRUE)
check("T7.1 same audio_file_ids on second upload", setequal(af_ids, af_ids2))

# ── Summary ──────────────────────────────────────────────────────────────────

cat("\n=== Test project DB IDs (clean up with SQL below if desired) ===\n")
cat(sprintf(
  "DELETE FROM import.deployments WHERE project_id = %d;\nDELETE FROM import.projects WHERE project_id = %d;\n",
  test_project_id, test_project_id
))

cat("\n=== Edge case files still present in location1_timeinterval1 ===\n")
cat("Remove with inst/test_01_cleanup.R or leave for repeated runs.\n")
