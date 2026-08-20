library(evalpam)

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
# Adjust all values in this block before running.

project_id               <- 1
N_WORKERS                <- 4L   # number of parallel future workers

CONDA_ENV_NAME           <- NULL  # e.g. "birdnet-r" — NULL uses birdnetR's managed env
                                   # Set to a conda env name when the managed env fails.
                                   # Create the env first with: setup_birdnet_conda()

# ── Species selection: choose ONE of the two approaches (A) or (B) ────────────
# (A) Explicit species list — restrict inference to exactly these species.
#     Integer vector of species_id values from lut_species_code, or NULL.
#     When set, spatiotemporal filtering (B) is DISABLED (mutually exclusive):
#     location/time play no role, unlikely species are still searched, and any
#     id not in lut_species_code / not detectable by the model aborts the run.
species_ids              <- NULL  # e.g. c(10L, 42L, 118L); NULL uses (B) below

# (B) Spatiotemporal filtering via eBird — used only when species_ids is NULL.
occurence_min_confidence <- 0.03  # eBird occurrence confidence threshold for species filtering
spatial_filtering        <- TRUE  # spatially filter species list via eBird; set FALSE to use all species
temporal_filtering       <- TRUE  # group audio files by week for time-specific species list

birdnet_params_list      <- list() # override BirdNET defaults, e.g. list(min_confidence = 0.1)
upload_inference         <- TRUE  # set FALSE to do a dry run without touching the database

# Windows: redirect temp I/O to a fast local disk (comment out on Linux/Mac)
# Sys.setenv(TMPDIR = "C:/temp", TMP = "C:/temp", TEMP = "C:/temp")
# ─────────────────────────────────────────────────────────────────────────────

pool <- set_db_pool()
on.exit(pool::poolClose(pool), add = TRUE)

# The two species-selection modes are mutually exclusive, so only pass the
# spatiotemporal arguments when no explicit species_ids list is given. Passing
# both would abort in run_birdnet_project().
args <- list(
  pool                = pool,
  project_id          = project_id,
  n_workers           = N_WORKERS,
  conda_env_name      = CONDA_ENV_NAME,
  birdnet_params_list = birdnet_params_list,
  upload_inference    = upload_inference
)

if (is.null(species_ids)) {
  args$occurence_min_confidence <- occurence_min_confidence
  args$spatial_filtering        <- spatial_filtering
  args$temporal_filtering       <- temporal_filtering
} else {
  args$species_ids <- species_ids
}

do.call(run_birdnet_project, args)
