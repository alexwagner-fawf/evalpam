library(evalpam)

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
# Adjust all values in this block before running.

project_id               <- 1
N_WORKERS                <- 4L   # number of parallel future workers

CONDA_ENV_NAME           <- NULL  # e.g. "birdnet-r" — NULL uses birdnetR's managed env
                                   # Set to a conda env name when the managed env fails.
                                   # Create the env first with: setup_birdnet_conda()

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

run_birdnet_project(
  pool                     = pool,
  project_id               = project_id,
  n_workers                = N_WORKERS,
  conda_env_name           = CONDA_ENV_NAME,
  occurence_min_confidence = occurence_min_confidence,
  spatial_filtering        = spatial_filtering,
  temporal_filtering       = temporal_filtering,
  birdnet_params_list      = birdnet_params_list,
  upload_inference         = upload_inference
)
