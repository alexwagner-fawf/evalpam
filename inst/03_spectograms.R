library(evalpam)

# Generic one-shot spectrogram generation. Each clip is tagged in
# import.spectrograms.selection_mode with the confidence_selection_mode used
# ("top"/"random"/"stratified"), so different sampling batches stay separable.
# For the guided screening -> calibration pipeline (phase 1 "top" WITH a stop
# criterion, then phase 2 "stratified" WITHOUT one) see
#   inst/05_two_phase_occupancy_workflow.R

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
project_id    <- 1          # filter deployments to this project (NULL = all)
n_per_species <- 30         # detections to sample per species × deployment
padding_s     <- 2          # seconds of context before/after detection
export_to_db  <- TRUE       # upload MP3 blobs to import.spectrograms

# Output directory for MP3 cache files.
# Reads spectogram_folder from .Renviron; falls back to ./spectograms if unset.
output_dir <- NULL  # NULL = use Sys.getenv("spectogram_folder")

# Optional: supply explicit deployment IDs to skip the automatic
# "one per location" selection. NULL uses the automatic selection.
deployment_ids <- NULL

# Optional: override the sampling strategy or grouping.
confidence_selection_mode <- "top"               # "top", "random", or "stratified"
grouping_by               <- c("species_id", "deployment_id")
# ─────────────────────────────────────────────────────────────────────────────

pool <- set_db_pool()
on.exit(pool::poolClose(pool), add = TRUE)

generate_spectrograms(
  pool                      = pool,
  project_id                = project_id,
  deployment_ids            = deployment_ids,
  n_per_species             = n_per_species,
  confidence_selection_mode = confidence_selection_mode,
  grouping_by               = grouping_by,
  padding_s                 = padding_s,
  output_dir                = output_dir,
  export_to_db              = export_to_db
)
