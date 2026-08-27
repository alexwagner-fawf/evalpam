# =============================================================================
# Two-phase occupancy + calibration workflow
# =============================================================================
#
# This script walks through a two-phase spectrogram workflow:
#
#   PHASE 1  "Screening"  – TOP samples WITH a stop criterion.
#             Confirm each species x location with a few high-confidence clips,
#             then stop (occupancy auto-stop). Answers "is the species here?".
#
#   PHASE 2  "Calibration" – STRATIFIED samples WITHOUT a stop criterion.
#             For the species confirmed in phase 1, sample detections spread
#             across the whole confidence range and verify ALL of them, to fit
#             e.g. a logistic regression of manual presence on BirdNET score.
#
# Prerequisites (run once, in order):
#   * inst/01_get_audio_files.R   – ingest audio files
#   * inst/02_inference.R         – run BirdNET, populate import.results
#   * Database migrations applied by a privileged (owner) role:
#       inst/sql/42_add_spectrogram_blobs.sql
#       inst/sql/43_update_ground_truth_annotations.sql
#       inst/sql/44_add_occupancy_groups.sql
#       inst/sql/45_add_performance_indices.sql
#       inst/sql/46_add_spectrogram_selection_mode.sql
#       inst/sql/47_spectrogram_unique_by_selection_mode.sql
#     (44 creates the occupancy tables; 46 adds spectrograms.selection_mode,
#      which every clip is tagged with – this is what separates the two phases;
#      47 makes (audio_file_id, begin_time_ms, selection_mode) unique so a window
#      holds at most one clip per sampling batch.)
# =============================================================================

library(evalpam)

project_id <- 1

pool <- set_db_pool()
tryCatch({

# =============================================================================
# PHASE 1 — screening with a stop criterion
# =============================================================================
# 1a. Generate TOP (highest-confidence) clips, grouped by species x location.
#     Every clip is tagged selection_mode = 'top' automatically.
generate_spectrograms(
  pool                      = pool,
  project_id                = project_id,
  confidence_selection_mode = "top",
  grouping_by               = c("species_id", "deployment_id"),  # species x location
  n_per_species             = 10,     # a small screening budget per group
  export_to_db              = TRUE
)

# 1b. Create the STOP CRITERION: one occupancy group per species x location,
#     confirmed after target_count = 2 certain-present verifications.
#     `groups_from_spectrograms()` partitions the project's spectrograms by any
#     key you like; use a function for anything the columns don't express
#     (e.g. lumping deployments into a site, or bi-weekly bins).
groups_from_spectrograms(
  pool         = pool,
  project_id   = project_id,
  group_by     = c("species_id", "deployment_id"),  # or add "month" / "biweek"
  target_count = 2L
)
# Tip: preview the partition first with dry_run = TRUE.

# 1c. Verify in the app WITH the occupancy filter ON.
#     -> After 2 certain-present hits, a group is confirmed: its remaining clips
#        drop out of the queue and a pop-up offers to skip to the next group.
#     Run:  evalpam::run_app()   (enable "Occupancy filter", pick a target species)


# =============================================================================
# BETWEEN PHASES — which species x locations got confirmed?
# =============================================================================
# Feeds phase 2: the species that reached their target_count. (A species x
# deployment group is single-species, so the group's species is the confirmed
# species.)
confirmed <- DBI::dbGetQuery(pool, "
  SELECT og.group_name,
         r.species_id,
         ls.species_scientific,
         COUNT(DISTINCT (gt.audio_file_id, gt.begin_time_ms)) AS n_hits,
         og.target_count
  FROM import.occupancy_groups og
  JOIN import.spectrogram_groups sg USING (group_id)
  JOIN import.spectrograms s        ON s.spectrogram_id = sg.spectrogram_id
  JOIN import.results r             ON r.result_id      = s.result_id
  JOIN public.lut_species_code ls   ON ls.species_id    = r.species_id
  JOIN import.annotation_status ast ON ast.audio_file_id = s.audio_file_id
                                   AND ast.begin_time_ms = s.begin_time_ms
                                   AND ast.target_species_id = r.species_id
  JOIN import.ground_truth_annotations gt
       ON gt.audio_file_id = ast.audio_file_id
      AND gt.begin_time_ms = ast.begin_time_ms
      AND gt.user_id       = ast.user_id
      AND gt.species_id    = ast.target_species_id
      AND gt.certainty_id  = 1
      AND gt.is_present    = TRUE
  WHERE og.project_id = $1
  GROUP BY og.group_name, r.species_id, ls.species_scientific, og.target_count
  HAVING COUNT(DISTINCT (gt.audio_file_id, gt.begin_time_ms)) >= og.target_count
", params = list(project_id))

print(confirmed)
confirmed_species <- sort(unique(confirmed$species_id))


# =============================================================================
# PHASE 2 — calibration sampling with NO stop criterion
# =============================================================================
# 2a. Generate STRATIFIED clips (spread across the confidence range) for the
#     confirmed species only. Tagged selection_mode = 'stratified'.
#
#     IMPORTANT — no stop criterion:
#       * Do NOT call groups_from_spectrograms() again afterwards. If you did,
#         these new clips would join the (still-confirmed) species x location
#         groups and be suppressed by the auto-stop immediately.
#       * Verify these clips in the app with the occupancy filter OFF, so every
#         clip stays in the queue and gets a manual label.
generate_spectrograms(
  pool                      = pool,
  project_id                = project_id,
  species_ids               = confirmed_species,
  confidence_selection_mode = "stratified",
  grouping_by               = c("species_id", "deployment_id"),
  n_per_species             = 30,     # ~one clip per confidence bin per group
  export_to_db              = TRUE
)

# 2b. Verify in the app WITH the occupancy filter OFF (no auto-stop):
#     evalpam::run_app()

# 2c. Build the modelling table: BirdNET confidence + manual label, restricted
#     to the phase-2 stratified clips (selection_mode tag keeps them separate
#     from the phase-1 screening clips, whose high-confidence bias would distort
#     the fit). Since migration 47, import.spectrograms is UNIQUE on
#     (audio_file_id, begin_time_ms, selection_mode), so a window has at most one
#     'stratified' clip and re-running phase 2 skips existing ones. The distinct()
#     below is belt-and-suspenders (e.g. multiple annotators per window).
model_raw <- DBI::dbGetQuery(pool, "
  SELECT s.spectrogram_id,
         s.audio_file_id, s.begin_time_ms,
         r.species_id, r.confidence / 1000.0 AS confidence,
         af.deployment_id,
         gt.is_present, gt.certainty_id
  FROM import.spectrograms s
  JOIN import.results r        ON r.result_id      = s.result_id
  JOIN import.audio_files af   ON af.audio_file_id = s.audio_file_id
  LEFT JOIN import.ground_truth_annotations gt
       ON gt.audio_file_id = s.audio_file_id
      AND gt.begin_time_ms = s.begin_time_ms
      AND gt.species_id    = r.species_id
  WHERE s.selection_mode = 'stratified'
    AND r.species_id = ANY($1::int[])
", params = list(paste0("{", paste(confirmed_species, collapse = ","), "}")))

model_df <- model_raw |>
  dplyr::filter(!is.na(.data$is_present)) |>                 # verified clips only
  dplyr::distinct(.data$audio_file_id, .data$begin_time_ms,
                  .data$species_id, .keep_all = TRUE)        # one row per window+species

# Example: detection reliability as a function of BirdNET score
# (per-species intercepts; add deployment as a covariate/random effect if sites
#  differ in SNR). Note this models P(true positive | a detection at score x) —
#  precision vs. score, truncated at BirdNET's min_confidence — not recall.
fit <- stats::glm(is_present ~ confidence + factor(species_id),
                  family = stats::binomial(), data = model_df)
print(summary(fit))

}, finally = {
  pool::poolClose(pool)
})
