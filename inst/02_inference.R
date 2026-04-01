library(evalpam)

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
# Adjust all values in this block before running.
CONDA_ENV_NAME <- NULL  # e.g. "birdnet-r" — NULL uses birdnetR's managed env
                        # Set to a conda env name when the managed env fails
                        # (wrong Python version, missing birdnet package, etc.)
                        # Create the env first with: setup_birdnet_conda()

N_WORKERS      <- 4L   # number of parallel future workers

project_id               <- 1
occurence_min_confidence <- 0.03  # eBird occurrence confidence threshold for species filtering
spatial_filtering        <- TRUE  # spatially filter species list via eBird; set FALSE to use all species
temporal_filtering       <- TRUE  # group audio files by week for time-specific species list
birdnet_params_list      <- list() # override BirdNET defaults, e.g. list(min_confidence = 0.1)
upload_inference         <- TRUE  # set FALSE to do a dry run without touching the database

# Windows: redirect temp I/O to a fast local disk (comment out on Linux/Mac)
# Sys.setenv(TMPDIR = "C:/temp", TMP = "C:/temp", TEMP = "C:/temp")

#Sys.setenv("https_proxy" = "http://proxy.rlp:8080")
# ─────────────────────────────────────────────────────────────────────────────

library(future)
library(future.apply)
options(future.workdir = normalizePath(getwd(), winslash = "/", mustWork = TRUE))

if (!spatial_filtering) {
  occurence_min_confidence <- 0
  temporal_filtering <- FALSE
}

# ── Python environment selection ──────────────────────────────────────────────
# Priority: (1) birdnetR managed venv, (2) conda env named by CONDA_ENV_NAME.
# The managed venv is tried first so that conda is only used when truly needed.
# If CONDA_ENV_NAME is set it skips the managed-env check entirely.
#
# To diagnose managed env problems interactively:
#   check_birdnet_managed_env()            # test + diagnose
#   check_birdnet_managed_env(force_reinstall = TRUE)  # wipe + rebuild
# To create a conda fallback env:
#   setup_birdnet_conda()                  # creates env "birdnet-r"

if (is.null(CONDA_ENV_NAME)) {
  # Try birdnetR's managed virtual environment first.
  managed_ok <- tryCatch({
    Sys.setenv(RETICULATE_PYTHON = "managed")
    reticulate::py_require(
      packages       = c("numpy>=1.23.5,<2.0.0", "birdnet==0.1.7"),
      python_version = ">=3.9,<3.12",
      action         = "add"
    )
    reticulate::py_config()  # force initialisation — surfaces version errors now
    TRUE
  }, error = function(e) {
    message("birdnetR managed env unavailable: ", conditionMessage(e))
    FALSE
  })

  if (!managed_ok) {
    stop(
      "The birdnetR managed virtual env could not be initialised and ",
      "CONDA_ENV_NAME is not set.\n",
      "Options:\n",
      "  1. Diagnose with: check_birdnet_managed_env()\n",
      "  2. Create a conda fallback and set CONDA_ENV_NAME:\n",
      "       setup_birdnet_conda()  # creates env 'birdnet-r'\n",
      "       CONDA_ENV_NAME <- \"birdnet-r\"  # then re-run"
    )
  }

  message("Using birdnetR managed virtual environment.")
  conda_env_python <- NULL

} else {
  # Resolve conda Python path — fail fast if the env does not exist.
  conda_env_python <- tryCatch(
    reticulate::conda_python(envname = CONDA_ENV_NAME),
    error = function(e) {
      stop("Could not find conda env '", CONDA_ENV_NAME, "'. ",
           "Run setup_birdnet_conda(\"", CONDA_ENV_NAME, "\") first.\n  ",
           e$message)
    }
  )
  message("Using conda env '", CONDA_ENV_NAME, "': ", conda_env_python)
}

pool <- set_db_pool()
on.exit(pool::poolClose(pool), add = TRUE)

# ── Load data from database ───────────────────────────────────────────────────

deployments <- sf::st_read(pool, DBI::Id("import", "deployments")) |>
  dplyr::filter(.data$project_id == !!project_id)

audio_files <- dplyr::tbl(pool, DBI::Id("import", "audio_files")) |>
  dplyr::filter(deployment_id %in% deployments$deployment_id) |>
  dplyr::select(deployment_id, audio_file_id, relative_path, timestamp_start) |>
  dplyr::collect()

species <- DBI::dbReadTable(pool, DBI::Id("lut_species_code"))

# ── Dev-mode coordinate check ─────────────────────────────────────────────────
# Coordinates are required for spatial/temporal species filtering.  In dev mode
# deployments may have empty geometry (no real recorder locations set up yet).
# Auto-disable spatial filtering so the script can run end-to-end for testing.
is_dev     <- tryCatch(golem::app_dev(), error = function(e) FALSE)
pkg_dev_path <- if (is_dev) tryCatch(golem::pkg_path(), error = function(e) NULL) else NULL
if (!is.null(pkg_dev_path))
  message("Dev mode: workers will load evalpam source from ", pkg_dev_path)

if (is_dev && any(sf::st_is_empty(deployments))) {
  message(
    "Dev mode: ", sum(sf::st_is_empty(deployments)),
    " deployment(s) have no geometry — spatial_filtering and temporal_filtering disabled."
  )
  spatial_filtering  <- FALSE
  temporal_filtering <- FALSE
  occurence_min_confidence <- 0
}

# Build full paths to audio files
audio_files <- audio_files |>
  dplyr::left_join(
    deployments |>
      dplyr::select(deployment_id, deployment_path) |>
      sf::st_drop_geometry()
  ) |>
  dplyr::mutate(
    relative_path = sub("^[\\\\/]+", "", relative_path),
    full_path     = file.path(deployment_path, relative_path)
  )

# Derive shared project folder from common path prefix of all deployment paths.
# Assumes all deployments share a common parent directory.
project_folder <- deployments |>
  dplyr::pull("deployment_path") |>
  normalizePath(winslash = "/", mustWork = FALSE) |>
  stringr::str_split(pattern = "/", simplify = TRUE) |>
  apply(2, function(x) if (length(unique(x)) == 1) x[1] else "") |>
  paste0(collapse = "/") |>
  stringr::str_remove("/$")

dir.create(file.path(project_folder, "inference_results"), showWarnings = FALSE)
temp_results_folder <- file.path(project_folder, "inference_results_temp")
dir.create(temp_results_folder, showWarnings = FALSE)

# ── Determine remaining deployments ──────────────────────────────────────────
# A deployment is considered finished when its temp .fst file exists.
# Re-run the script to retry any deployment whose temp file is missing.

finished_deployments <- list.files(temp_results_folder) |>
  tools::file_path_sans_ext() |>
  as.integer()

remaining_deployments <- as.integer(deployments$deployment_id)[
  !as.integer(deployments$deployment_id) %in% finished_deployments
]

if (length(remaining_deployments) == 0) {
  message("All deployments already processed. Skipping inference, proceeding to aggregation.")
} else {

  # ── Worker function ───────────────────────────────────────────────────────
  # Each future worker runs this function in its own R session.
  # On error the worker writes a per-audio-file error frame so the deployment
  # is not silently skipped on the next run, and the main session is not aborted.

  process_deployment_worker <- function(deployment_id,
                                        deployments,
                                        audio_files,
                                        temporal_filtering,
                                        occurence_min_confidence,
                                        species,
                                        birdnet_params_list,
                                        conda_env_python,
                                        temp_results_folder,
                                        pkg_dev_path) {

    # Thread-count pins apply in both managed and conda paths.
    Sys.setenv(
      OMP_NUM_THREADS      = 1,
      MKL_NUM_THREADS      = 1,
      OPENBLAS_NUM_THREADS = 1,
      TF_CPP_MIN_LOG_LEVEL = 2   # suppress TF info logs
    )

    # ── Python / reticulate setup ──────────────────────────────────────────
    # Both branches call py_config() before library(evalpam) so that Python is
    # fully initialised before any birdnetR:: namespace call can trigger
    # birdnetR's .onLoad (which would otherwise race to set RETICULATE_PYTHON).
    #
    # Managed env path (conda_env_python is NULL):
    #   Replicate birdnetR's .onLoad explicitly.  Errors surface here as clear
    #   worker failures rather than opaque "birdnetR failed to load" messages.
    #
    # Conda path (conda_env_python is a file path):
    #   Bind Python before birdnetR loads so .onLoad's
    #   Sys.setenv(RETICULATE_PYTHON="managed") is a no-op.
    library(reticulate)

    if (!is.null(conda_env_python)) {
      Sys.setenv(RETICULATE_PYTHON = conda_env_python)
      reticulate::use_python(conda_env_python, required = TRUE)
      reticulate::py_config()
      library(birdnetR)  # .onLoad fires; Python already bound — no-op
    } else {
      Sys.setenv(RETICULATE_PYTHON = "managed")
      reticulate::py_require(
        packages       = c("numpy>=1.23.5,<2.0.0", "birdnet==0.1.7"),
        python_version = ">=3.9,<3.12",
        action         = "add"
      )
      reticulate::py_config()
      # birdnetR will load lazily on first birdnetR:: call; Python already bound
    }

    # In dev mode load from source so workers see the same function signatures
    # as the main session (avoids "unused argument" errors when the installed
    # package is older than the source tree).
    if (!is.null(pkg_dev_path)) {
      pkgload::load_all(pkg_dev_path, quiet = TRUE)
    } else {
      library(evalpam)
    }
    library(sf)
    library(dplyr)

    # Reconstruct sf object from WKT — sf objects cannot cross session boundaries
    deployments <- deployments |>
      dplyr::mutate(geometry = sf::st_as_sfc(geometry_wkt, crs = 4326)) |>
      sf::st_as_sf()

    temp_file_name <- file.path(temp_results_folder, paste0(deployment_id, ".fst"))

    result_df <- tryCatch(
      process_deployment_birdnet(
        deployment_id            = deployment_id,
        deployments              = deployments,
        audio_files              = audio_files,
        temporal_filtering       = temporal_filtering,
        occurence_min_confidence = occurence_min_confidence,
        birdnet_params_list      = birdnet_params_list,
        species                  = species,
        verbose                  = FALSE,
        tflite_num_threads       = 1
      ),
      error = function(e) {
        warning("Worker failed for deployment ", deployment_id, ": ", conditionMessage(e))
        # Return one error row per audio file so the deployment is not re-queued
        # endlessly. settings_id = NA signals a worker-level failure to the
        # aggregation step (these rows are excluded from DB upload).
        af_ids <- audio_files$audio_file_id[audio_files$deployment_id == deployment_id]
        n      <- length(af_ids)
        data.frame(
          audio_file_id = af_ids,
          settings_id   = rep(NA_integer_,   n),
          begin_time_ms = rep(NA_integer_,   n),
          end_time_ms   = rep(NA_integer_,   n),
          confidence    = rep(NA_integer_,   n),
          species_id    = rep(NA_integer_,   n),
          behavior_id   = rep(NA_integer_,   n),
          error_type    = rep("failed_worker_error", n),
          analysed_at   = rep(Sys.time(),    n)
        )
      }
    )

    if (file.exists(temp_file_name) && is.data.frame(result_df)) file.remove(temp_file_name)
    fst::write_fst(result_df, temp_file_name)
    return(temp_file_name)
  }

  # Serialize deployments as a plain data frame — sf objects cannot be passed to
  # future workers. Workers rebuild the sf object from the geometry_wkt column.
  deployments_export <- deployments |>
    dplyr::mutate(geometry_wkt = sf::st_as_text(geometry)) |>
    sf::st_drop_geometry()

  # Pre-split audio_files by deployment so each worker only receives the rows
  # it needs. With 1M+ total audio files, passing the full data frame to every
  # worker wastes memory and serialisation time. future_mapply varies the
  # audio_files argument per call while keeping all other args constant.
  audio_files_by_dep <- split(audio_files, audio_files$deployment_id)
  # Guard: ensure every remaining deployment has an entry (may be empty)
  missing_dep_keys <- setdiff(as.character(remaining_deployments),
                              names(audio_files_by_dep))
  for (d in missing_dep_keys) audio_files_by_dep[[d]] <- audio_files[0L, ]

  plan(multisession, workers = N_WORKERS)

  temp_files <- future.apply::future_mapply(
    FUN           = process_deployment_worker,
    deployment_id = as.integer(remaining_deployments),
    audio_files   = audio_files_by_dep[as.character(remaining_deployments)],
    MoreArgs = list(
      deployments              = deployments_export,
      species                  = species,
      temporal_filtering       = temporal_filtering,
      occurence_min_confidence = occurence_min_confidence,
      birdnet_params_list      = birdnet_params_list,
      conda_env_python         = conda_env_python,
      temp_results_folder      = temp_results_folder,
      pkg_dev_path             = pkg_dev_path
    ),
    SIMPLIFY    = FALSE,
    future.seed = TRUE
  )
}

# ── Aggregation ───────────────────────────────────────────────────────────────

# Local index: tracks (audio_file_id, settings_id) pairs already processed
# to skip re-uploads and allow incremental runs.
index_file <- file.path(project_folder,
                        "inference_results",
                        "inference_results_index.fst") |>
  normalizePath(winslash = "/", mustWork = FALSE)

# One results file per session to avoid reading and rewriting large files.
results_file <- file.path(
  project_folder,
  "inference_results",
  paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_inference_results.fst")
) |>
  normalizePath(winslash = "/", mustWork = FALSE)

if (file.exists(index_file)) {
  inference_index <- fst::read_fst(index_file)
} else {
  inference_index <- dplyr::tibble(
    audio_file_id = integer(),
    settings_id   = integer(),
    status        = character(),
    analysed_at   = as.POSIXct(character(0))
  )
}

# Pre-filter temp fst files to skip deployments already fully indexed.
# For projects with > 100 deployments, this avoids loading GBs of previously
# processed results into RAM just to discard them via anti_join.
#
# A deployment is "fully indexed" when every audio_file_id belonging to it
# has at least one successful index entry. We derive this from `audio_files`
# (already in memory from the DB query above) and the in-memory index.
all_temp_fst_files <- list.files(temp_results_folder,
                                 full.names = TRUE,
                                 pattern    = "\\.fst$")

if (nrow(inference_index) > 0 && length(all_temp_fst_files) > 0) {
  indexed_af_ids <- inference_index |>
    dplyr::filter(status == "success") |>
    dplyr::pull(audio_file_id)

  # Deployments where all audio files have a success entry
  fully_indexed_dep_ids <- audio_files |>
    dplyr::group_by(deployment_id) |>
    dplyr::filter(all(audio_file_id %in% indexed_af_ids)) |>
    dplyr::pull(deployment_id) |>
    unique()

  temp_fst_to_load <- all_temp_fst_files[
    !(tools::file_path_sans_ext(basename(all_temp_fst_files)) %in%
        as.character(fully_indexed_dep_ids))
  ]

  n_skipped <- length(all_temp_fst_files) - length(temp_fst_to_load)
  if (n_skipped > 0)
    message(sprintf("Skipping %d fully-indexed deployment temp file(s); loading %d.",
                    n_skipped, length(temp_fst_to_load)))
} else {
  temp_fst_to_load <- all_temp_fst_files
}

birdnet_inference <- temp_fst_to_load |>
  lapply(fst::read_fst) |>
  dplyr::bind_rows()

# Skip files already successfully indexed. Re-process previously failed files
# (allows recovery after transient errors on a re-run).
birdnet_inference_new <- birdnet_inference |>
  dplyr::anti_join(
    dplyr::filter(inference_index, status == "success"),
    by = c("audio_file_id", "settings_id")
  )

if (nrow(birdnet_inference_new) == 0) {
  message("No new inference results.")
} else {

  # Write local results file (successful detections only, no error rows)
  birdnet_inference_new |>
    dplyr::filter(is.na(error_type)) |>
    dplyr::select(-error_type, -analysed_at) |>
    fst::write_fst(results_file)

  # Build index update — one row per (audio_file_id, settings_id)
  add2index_file <- birdnet_inference_new |>
    dplyr::select(audio_file_id, settings_id, error_type, analysed_at) |>
    dplyr::distinct() |>
    dplyr::mutate(status = ifelse(is.na(error_type), "success", error_type)) |>
    dplyr::select(-error_type)

  if (file.exists(index_file)) {
    # Merge: new result wins over old for the same (audio_file_id, settings_id)
    add2index_file |>
      dplyr::anti_join(inference_index, by = c("audio_file_id", "settings_id")) |>
      dplyr::bind_rows(inference_index) |>
      dplyr::arrange(audio_file_id, settings_id, dplyr::desc(analysed_at)) |>
      dplyr::distinct(audio_file_id, settings_id, .keep_all = TRUE) |>
      fst::write_fst(index_file)
  } else {
    fst::write_fst(add2index_file, index_file)
  }

  if (upload_inference) {
    result_ids <- birdnet_inference_new |>
      dplyr::filter(is.na(error_type)) |>
      dplyr::filter(!is.na(species_id)) |>
      upsert_results_df(conn = pool)

    # Worker-level failures have settings_id = NA and cannot be written to
    # analysis_log (FK + NOT NULL constraint). They are tracked locally only.
    add2index_file |>
      dplyr::filter(!is.na(settings_id)) |>
      upsert_analysis_log_df(conn = pool)
  }
}

# ── Notes ─────────────────────────────────────────────────────────────────────
# To upload results from previous sessions that were not yet uploaded:
# read all .fst files from inference_results/ individually and call
# upsert_results_df() on each. The DB enforces uniqueness on
# (audio_file_id, settings_id, begin_time_ms, end_time_ms, species_id),
# so re-uploading is safe.
#
# Conda fallback usage:
#   1. Run setup_birdnet_conda() once to create the env (Python 3.11 + birdnet==0.1.7)
#   2. Set CONDA_ENV_NAME <- "birdnet-r" in the config block above
#   3. Re-run this script; workers will use the conda env instead of the managed venv
