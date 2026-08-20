#' Run BirdNET inference for an entire project
#'
#' Loads all deployments and audio files for a project, runs BirdNET inference
#' in parallel (one future worker per deployment), aggregates the per-deployment
#' result files, and optionally uploads results and analysis logs to the database.
#'
#' Intermediate per-deployment results are written to
#' \code{<results_folder>/inference_results_temp/<deployment_id>.fst} so that
#' interrupted runs can be resumed without reprocessing finished deployments.
#' A persistent index at \code{<results_folder>/inference_results/inference_results_index.fst}
#' tracks which (audio_file_id, settings_id) pairs have already been uploaded.
#'
#' @param pool A database connection pool (from \code{\link{set_db_pool}}).
#' @param project_id Integer. ID of the project to process.
#' @param species_ids Integer vector or NULL. Explicit list of species (ids
#'   from \code{lut_species_code}) to restrict inference to. This is mutually
#'   exclusive with spatiotemporal filtering: supplying \code{species_ids}
#'   \emph{and} explicitly requesting spatial/temporal filtering (a truthy
#'   \code{spatial_filtering}/\code{temporal_filtering} or
#'   \code{occurence_min_confidence > 0}) is an error. When \code{species_ids}
#'   is given, location/time filtering is disabled, the exact list is searched
#'   (unlikely species included), and latitude/longitude/week/eBird-confidence
#'   are stored as null in the settings row. Ids absent from
#'   \code{lut_species_code}, or species the BirdNET model cannot detect, abort
#'   the run. \code{NULL} (default) uses spatiotemporal filtering.
#' @param n_workers Integer. Number of parallel future workers. Default 4.
#' @param conda_env_name Character or NULL. Name of a conda environment to use
#'   for Python/BirdNET. \code{NULL} (default) uses birdnetR's managed
#'   virtual environment.
#' @param occurence_min_confidence Numeric. Minimum eBird occurrence probability
#'   used to filter the location/time species list. Default 0.03. Set to 0 to
#'   disable spatial filtering (all BirdNET species are considered).
#' @param spatial_filtering Logical. If \code{FALSE}, species filtering by
#'   location/time is disabled (\code{occurence_min_confidence} is forced to 0
#'   and \code{temporal_filtering} to \code{FALSE}). Default \code{TRUE}.
#' @param temporal_filtering Logical. Group audio files by calendar week to
#'   obtain week-specific species lists. Default \code{TRUE}.
#' @param birdnet_params_list Named list. Override BirdNET inference defaults
#'   (e.g. \code{list(min_confidence = 0.1)}). Default \code{list()}.
#' @param upload_inference Logical. Upload results and analysis log to the
#'   database. Set \code{FALSE} for a dry run. Default \code{TRUE}.
#' @param coordinates_decimal_places Integer. Decimal places for rounding
#'   deployment coordinates when building the species list (1 or 2). Default 1.
#' @param results_folder Character or NULL. Root folder for storing temp and
#'   aggregated result files. If \code{NULL} (default), derived automatically
#'   from the common ancestor path of all deployment paths.
#' @param verbose Logical. Print progress messages. Default \code{TRUE}.
#'
#' @return Invisibly returns the aggregated inference data frame (rows for new
#'   detections only; error rows included).
#'
#' @export
run_birdnet_project <- function(pool,
                                project_id,
                                species_ids                = NULL,
                                n_workers                  = 4L,
                                conda_env_name             = NULL,
                                occurence_min_confidence   = 0.03,
                                spatial_filtering          = TRUE,
                                temporal_filtering         = TRUE,
                                birdnet_params_list        = list(),
                                upload_inference           = TRUE,
                                coordinates_decimal_places = 1L,
                                results_folder             = NULL,
                                verbose                    = TRUE) {

  # ── Species-selection mode: explicit list XOR spatiotemporal filtering ──────
  # These two ways of restricting the searched species set are mutually
  # exclusive. `missing()` distinguishes "user asked for filtering" from the
  # defaults, so a bare `species_ids = ...` call transparently switches to
  # list mode, while explicitly combining the two is an error.
  use_species_list <- !is.null(species_ids)
  if (use_species_list) {
    conflict <-
      (!missing(spatial_filtering)        && isTRUE(spatial_filtering))  ||
      (!missing(temporal_filtering)       && isTRUE(temporal_filtering)) ||
      (!missing(occurence_min_confidence) && occurence_min_confidence > 0)
    if (conflict) {
      stop("Provide either `species_ids` (explicit species list) OR ",
           "spatiotemporal filtering (`spatial_filtering` / `temporal_filtering` / ",
           "`occurence_min_confidence` > 0), not both. When `species_ids` is set, ",
           "location/time filtering is disabled.")
    }
    spatial_filtering        <- FALSE
    temporal_filtering       <- FALSE
    occurence_min_confidence <- 0
    if (verbose) message(sprintf(
      "Species-list mode: %d species requested; spatial/temporal filtering disabled.",
      length(unique(species_ids))
    ))
  }

  if (!spatial_filtering) {
    occurence_min_confidence <- 0
    temporal_filtering       <- FALSE
  }

  # ── Python environment selection ─────────────────────────────────────────────
  if (is.null(conda_env_name)) {
    managed_ok <- tryCatch({
      Sys.setenv(RETICULATE_PYTHON = "managed")
      reticulate::py_require(
        packages       = c("numpy>=1.23.5,<2.0.0", "birdnet==0.1.7"),
        python_version = ">=3.9,<3.12",
        action         = "add"
      )
      reticulate::py_config()
      TRUE
    }, error = function(e) {
      message("birdnetR managed env unavailable: ", conditionMessage(e))
      FALSE
    })

    if (!managed_ok) {
      stop(
        "The birdnetR managed virtual env could not be initialised and ",
        "conda_env_name is not set.\n",
        "Options:\n",
        "  1. Diagnose with: check_birdnet_managed_env()\n",
        "  2. Create a conda fallback and set conda_env_name:\n",
        "       setup_birdnet_conda()  # creates env 'birdnet-r'\n",
        "       conda_env_name <- \"birdnet-r\""
      )
    }

    if (verbose) message("Using birdnetR managed virtual environment.")
    conda_env_python <- NULL

  } else {
    conda_env_python <- tryCatch(
      reticulate::conda_python(envname = conda_env_name),
      error = function(e) {
        stop("Could not find conda env '", conda_env_name, "'. ",
             "Run setup_birdnet_conda(\"", conda_env_name, "\") first.\n  ",
             e$message)
      }
    )
    if (verbose) message("Using conda env '", conda_env_name, "': ", conda_env_python)
  }

  # ── Load data from database ──────────────────────────────────────────────────
  deployments <- sf::st_read(pool, DBI::Id("import", "deployments"), quiet = TRUE) |>
    dplyr::filter(.data$project_id == !!project_id)

  audio_files <- dplyr::tbl(pool, DBI::Id("import", "audio_files")) |>
    dplyr::filter(.data$deployment_id %in% deployments$deployment_id) |>
    dplyr::select(.data$deployment_id, .data$audio_file_id,
                  .data$relative_path, .data$timestamp_start) |>
    dplyr::collect()

  species <- DBI::dbReadTable(pool, DBI::Id("lut_species_code"))

  # ── Resolve explicit species filter (validate once, before spawning workers) ─
  species_filter_labels <- NULL
  species_filter_hash   <- "none"
  if (use_species_list) {
    model_labels          <- .birdnet_model_labels(version = "v2.4", language = "en_us")
    species_filter_labels <- resolve_species_filter_labels(species_ids, species, model_labels)
    species_filter_hash   <- digest::digest(sort(species_filter_labels), algo = "xxhash64")
    if (verbose) message(sprintf(
      "Resolved %d species id(s) to %d BirdNET label(s); settings species_filter = %s.",
      length(unique(species_ids)), length(species_filter_labels), species_filter_hash
    ))
  }

  # ── Dev-mode coordinate check ─────────────────────────────────────────────────
  is_dev       <- tryCatch(golem::app_dev(), error = function(e) FALSE)
  pkg_dev_path <- if (is_dev) tryCatch(golem::pkg_path(), error = function(e) NULL) else NULL

  if (!is.null(pkg_dev_path) && verbose)
    message("Dev mode: workers will load evalpam source from ", pkg_dev_path)

  if (is_dev && any(sf::st_is_empty(deployments))) {
    if (verbose) message(
      "Dev mode: ", sum(sf::st_is_empty(deployments)),
      " deployment(s) have no geometry — spatial_filtering and temporal_filtering disabled."
    )
    spatial_filtering        <- FALSE
    temporal_filtering       <- FALSE
    occurence_min_confidence <- 0
  }

  # Build full paths to audio files
  audio_files <- audio_files |>
    dplyr::left_join(
      deployments |>
        dplyr::select(.data$deployment_id, .data$deployment_path) |>
        sf::st_drop_geometry(),
      by = "deployment_id"
    ) |>
    dplyr::mutate(
      relative_path = sub("^[\\\\/]+", "", .data$relative_path),
      full_path     = file.path(.data$deployment_path, .data$relative_path)
    )

  # ── Result folder ────────────────────────────────────────────────────────────
  if (is.null(results_folder)) {
    results_folder <- deployments |>
      dplyr::pull("deployment_path") |>
      normalizePath(winslash = "/", mustWork = FALSE) |>
      stringr::str_split("/", simplify = TRUE) |>
      apply(2, function(x) if (length(unique(x)) == 1L) x[1L] else "") |>
      paste0(collapse = "/") |>
      stringr::str_remove("/$")
  }

  dir.create(file.path(results_folder, "inference_results"), showWarnings = FALSE)
  temp_results_folder <- file.path(results_folder, "inference_results_temp")
  dir.create(temp_results_folder, showWarnings = FALSE)

  # ── Determine remaining deployments ─────────────────────────────────────────
  finished_deployments <- list.files(temp_results_folder) |>
    tools::file_path_sans_ext() |>
    as.integer()

  remaining_deployments <- as.integer(deployments$deployment_id)[
    !as.integer(deployments$deployment_id) %in% finished_deployments
  ]

  if (length(remaining_deployments) == 0) {
    if (verbose) message("All deployments already processed. Proceeding to aggregation.")
  } else {

    # ── Worker function ───────────────────────────────────────────────────────
    process_deployment_worker <- function(deployment_id,
                                          audio_files,
                                          deployments,
                                          species,
                                          temporal_filtering,
                                          occurence_min_confidence,
                                          birdnet_params_list,
                                          coordinates_decimal_places,
                                          conda_env_python,
                                          temp_results_folder,
                                          pkg_dev_path,
                                          species_filter_labels,
                                          species_filter_hash) {

      Sys.setenv(
        OMP_NUM_THREADS      = 1,
        MKL_NUM_THREADS      = 1,
        OPENBLAS_NUM_THREADS = 1,
        TF_CPP_MIN_LOG_LEVEL = 2
      )

      library(reticulate)

      if (!is.null(conda_env_python)) {
        Sys.setenv(RETICULATE_PYTHON = conda_env_python)
        reticulate::use_python(conda_env_python, required = TRUE)
        reticulate::py_config()
        library(birdnetR)
      } else {
        Sys.setenv(RETICULATE_PYTHON = "managed")
        reticulate::py_require(
          packages       = c("numpy>=1.23.5,<2.0.0", "birdnet==0.1.7"),
          python_version = ">=3.9,<3.12",
          action         = "add"
        )
        reticulate::py_config()
      }

      if (!is.null(pkg_dev_path)) {
        pkgload::load_all(pkg_dev_path, quiet = TRUE)
      } else {
        library(evalpam)
      }
      library(sf)
      library(dplyr)

      deployments <- deployments |>
        dplyr::mutate(geometry = sf::st_as_sfc(.data$geometry_wkt, crs = 4326)) |>
        sf::st_as_sf()

      temp_file_name <- file.path(temp_results_folder, paste0(deployment_id, ".fst"))

      result_df <- tryCatch(
        process_deployment_birdnet(
          deployment_id              = deployment_id,
          deployments                = deployments,
          audio_files                = audio_files,
          temporal_filtering         = temporal_filtering,
          occurence_min_confidence   = occurence_min_confidence,
          birdnet_params_list        = birdnet_params_list,
          species                    = species,
          verbose                    = FALSE,
          coordinates_decimal_places = coordinates_decimal_places,
          tflite_num_threads         = 1L,
          species_filter_labels      = species_filter_labels,
          species_filter_hash        = species_filter_hash
        ),
        error = function(e) {
          warning("Worker failed for deployment ", deployment_id, ": ", conditionMessage(e))
          af_ids <- audio_files$audio_file_id[audio_files$deployment_id == deployment_id]
          n      <- length(af_ids)
          data.frame(
            audio_file_id = af_ids,
            settings_id   = rep(NA_integer_,          n),
            begin_time_ms = rep(NA_integer_,          n),
            end_time_ms   = rep(NA_integer_,          n),
            confidence    = rep(NA_integer_,          n),
            species_id    = rep(NA_integer_,          n),
            behavior_id   = rep(NA_integer_,          n),
            error_type    = rep("failed_worker_error", n),
            analysed_at   = rep(Sys.time(),            n)
          )
        }
      )

      if (file.exists(temp_file_name) && is.data.frame(result_df))
        file.remove(temp_file_name)
      fst::write_fst(result_df, temp_file_name)
      temp_file_name
    }

    # Serialize deployments (sf objects cannot cross future session boundaries)
    deployments_export <- deployments |>
      dplyr::mutate(geometry_wkt = sf::st_as_text(.data$geometry)) |>
      sf::st_drop_geometry()

    # Pre-split audio_files by deployment to minimise serialisation overhead
    audio_files_by_dep <- split(audio_files, audio_files$deployment_id)
    missing_keys <- setdiff(as.character(remaining_deployments),
                            names(audio_files_by_dep))
    for (d in missing_keys) audio_files_by_dep[[d]] <- audio_files[0L, ]

    future::plan(future::multisession, workers = n_workers)

    future.apply::future_mapply(
      FUN           = process_deployment_worker,
      deployment_id = as.integer(remaining_deployments),
      audio_files   = audio_files_by_dep[as.character(remaining_deployments)],
      MoreArgs = list(
        deployments                = deployments_export,
        species                    = species,
        temporal_filtering         = temporal_filtering,
        occurence_min_confidence   = occurence_min_confidence,
        birdnet_params_list        = birdnet_params_list,
        coordinates_decimal_places = coordinates_decimal_places,
        conda_env_python           = conda_env_python,
        temp_results_folder        = temp_results_folder,
        pkg_dev_path               = pkg_dev_path,
        species_filter_labels      = species_filter_labels,
        species_filter_hash        = species_filter_hash
      ),
      SIMPLIFY    = FALSE,
      future.seed = TRUE
    )
  }

  # ── Aggregation ──────────────────────────────────────────────────────────────
  index_file <- file.path(results_folder,
                          "inference_results",
                          "inference_results_index.fst") |>
    normalizePath(winslash = "/", mustWork = FALSE)

  results_file <- file.path(
    results_folder,
    "inference_results",
    paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_inference_results.fst")
  ) |>
    normalizePath(winslash = "/", mustWork = FALSE)

  inference_index <- if (file.exists(index_file)) {
    fst::read_fst(index_file)
  } else {
    dplyr::tibble(
      audio_file_id = integer(),
      settings_id   = integer(),
      status        = character(),
      analysed_at   = as.POSIXct(character(0))
    )
  }

  all_temp_fst <- list.files(temp_results_folder, full.names = TRUE, pattern = "\\.fst$")

  # Skip fully-indexed deployments to avoid loading large files unnecessarily
  temp_fst_to_load <- if (nrow(inference_index) > 0 && length(all_temp_fst) > 0) {
    indexed_af_ids <- inference_index |>
      dplyr::filter(.data$status == "success") |>
      dplyr::pull(.data$audio_file_id)

    fully_indexed_dep_ids <- audio_files |>
      dplyr::group_by(.data$deployment_id) |>
      dplyr::filter(all(.data$audio_file_id %in% indexed_af_ids)) |>
      dplyr::pull(.data$deployment_id) |>
      unique()

    kept <- all_temp_fst[
      !(tools::file_path_sans_ext(basename(all_temp_fst)) %in%
          as.character(fully_indexed_dep_ids))
    ]
    n_skipped <- length(all_temp_fst) - length(kept)
    if (n_skipped > 0 && verbose)
      message(sprintf("Skipping %d fully-indexed deployment temp file(s); loading %d.",
                      n_skipped, length(kept)))
    kept
  } else {
    all_temp_fst
  }

  birdnet_inference <- temp_fst_to_load |>
    lapply(fst::read_fst) |>
    dplyr::bind_rows()

  birdnet_inference_new <- birdnet_inference |>
    dplyr::anti_join(
      dplyr::filter(inference_index, .data$status == "success"),
      by = c("audio_file_id", "settings_id")
    )

  if (nrow(birdnet_inference_new) == 0) {
    if (verbose) message("No new inference results.")
    return(invisible(birdnet_inference_new))
  }

  birdnet_inference_new |>
    dplyr::filter(is.na(.data$error_type)) |>
    dplyr::select(-.data$error_type, -.data$analysed_at) |>
    fst::write_fst(results_file)

  add2index <- birdnet_inference_new |>
    dplyr::select(.data$audio_file_id, .data$settings_id,
                  .data$error_type, .data$analysed_at) |>
    dplyr::distinct() |>
    dplyr::mutate(status = ifelse(is.na(.data$error_type), "success", .data$error_type)) |>
    dplyr::select(-.data$error_type)

  updated_index <- if (file.exists(index_file)) {
    add2index |>
      dplyr::anti_join(inference_index, by = c("audio_file_id", "settings_id")) |>
      dplyr::bind_rows(inference_index) |>
      dplyr::arrange(.data$audio_file_id, .data$settings_id,
                     dplyr::desc(.data$analysed_at)) |>
      dplyr::distinct(.data$audio_file_id, .data$settings_id, .keep_all = TRUE)
  } else {
    add2index
  }
  fst::write_fst(updated_index, index_file)

  if (upload_inference) {
    birdnet_inference_new |>
      dplyr::filter(is.na(.data$error_type), !is.na(.data$species_id)) |>
      upsert_results_df(conn = pool)

    add2index |>
      dplyr::filter(!is.na(.data$settings_id)) |>
      upsert_analysis_log_df(conn = pool)

    if (verbose) message("Inference results uploaded.")
  }

  invisible(birdnet_inference_new)
}
