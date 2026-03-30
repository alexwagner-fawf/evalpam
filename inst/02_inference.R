library(evalpam)

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
# Adjust all values in this block before running.

PYTHON_PATH    <- "C:/Users/awagner/.conda/envs/pam_py310/python.exe"
N_WORKERS      <- 4L   # number of parallel future workers

project_id               <- 1
occurence_min_confidence <- 0.03  # eBird occurrence confidence threshold for species filtering
spatial_filtering        <- TRUE  # spatially filter species list via eBird; set FALSE to use all species
temporal_filtering       <- TRUE  # group audio files by week for time-specific species list
birdnet_params_list      <- list() # override BirdNET defaults, e.g. list(min_confidence = 0.1)
upload_inference         <- TRUE  # set FALSE to do a dry run without touching the database

# Windows: redirect temp I/O to a fast local disk (comment out on Linux/Mac)
# Sys.setenv(TMPDIR = "C:/temp", TMP = "C:/temp", TEMP = "C:/temp")

Sys.setenv("https_proxy" = "http://proxy.rlp:8080")
# ─────────────────────────────────────────────────────────────────────────────

library(future)
library(future.apply)
options(future.workdir = normalizePath(getwd(), winslash = "/", mustWork = TRUE))

if (!spatial_filtering) {
  occurence_min_confidence <- 0
  temporal_filtering <- FALSE
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
                                        python_path,
                                        temp_results_folder) {

    Sys.setenv(
      RETICULATE_PYTHON    = python_path,
      OMP_NUM_THREADS      = 1,
      MKL_NUM_THREADS      = 1,
      OPENBLAS_NUM_THREADS = 1,
      TF_CPP_MIN_LOG_LEVEL = 2   # suppress TF info logs
    )

    library(reticulate)
    use_python(python_path, required = TRUE)
    py_require(c("numpy>=1.23.5", "birdnet==0.1.7"), action = "add")
    library(evalpam)
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
        data.frame(
          audio_file_id = af_ids,
          settings_id   = NA_integer_,
          begin_time_ms = NA_integer_,
          end_time_ms   = NA_integer_,
          confidence    = NA_integer_,
          species_id    = NA_integer_,
          behavior_id   = NA_integer_,
          error_type    = "failed_worker_error",
          analysed_at   = Sys.time()
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

  plan(multisession, workers = N_WORKERS)

  temp_files <- future_lapply(
    as.integer(remaining_deployments),
    FUN                      = process_deployment_worker,
    deployments              = deployments_export,
    audio_files              = audio_files,
    species                  = species,
    temporal_filtering       = temporal_filtering,
    occurence_min_confidence = occurence_min_confidence,
    birdnet_params_list      = birdnet_params_list,
    python_path              = PYTHON_PATH,
    temp_results_folder      = temp_results_folder,
    future.seed              = TRUE
  )
}

# ── Aggregation ───────────────────────────────────────────────────────────────

birdnet_inference <- list.files(temp_results_folder,
                                full.names = TRUE,
                                pattern    = "\\.fst$") |>
  lapply(fst::read_fst) |>
  dplyr::bind_rows()

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
