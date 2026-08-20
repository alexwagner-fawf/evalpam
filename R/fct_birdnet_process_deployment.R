#' Process a single deployment with BirdNET inference
#'
#' Main entry point for processing a deployment:
#' - filters audio files
#' - sets up BirdNET model
#' - generates settings
#' - applies inference
#' - aggregates results
#'
#' @param deployment_id Integer. ID of the deployment to process.
#' @param deployments Data frame / sf object. Deployment metadata.
#' @param audio_files Data frame. Audio files metadata.
#' @param temporal_filtering Logical. Whether to group audio files by week to facilitate temporal species filtering.
#' @param occurence_min_confidence Numeric. Minimum eBird occurrence confidence. If 0, all available species in Birdnet will be used
#' @param birdnet_params_list List. Optional parameters to override BirdNET defaults.
#' @param verbose Logical. Print progress messages.
#' @param internal_pool DBI pool object. If NULL, a new pool will be created.
#' @param coordinates_decimal_places integer, number of decimal places for epsg4326 coordinates (only 1 or 2 decimal places are meaningful, 2 may lead to more fine grained species filters but 1 should be enough and reduce the number of settings in the database)
#' @param species_filter_labels Character vector or NULL. Explicit BirdNET
#'   labels (from \code{\link{resolve_species_filter_labels}}) to restrict
#'   inference to. When supplied, this overrides the spatiotemporal (eBird)
#'   species list entirely: location/time play no role, unlikely species are
#'   still included, and \code{latitude}/\code{longitude}/\code{week}/
#'   \code{ebird_min_confidence} are stored as null in the settings row.
#'   \code{NULL} (default) uses the spatiotemporal list.
#' @param species_filter_hash Character. Stable hash of the species-list filter
#'   used as part of settings identity, or \code{"none"} (default) in
#'   spatiotemporal mode.
#'
#' @return Data frame of BirdNET inference results.
#' @export
process_deployment_birdnet <- function(deployment_id,
                               deployments,
                               audio_files,
                               species,
                               temporal_filtering,
                               occurence_min_confidence,
                               birdnet_params_list = list(),
                               verbose = TRUE,
                               internal_pool = NULL,
                               coordinates_decimal_places = 1L,
                               tflite_num_threads = 1,
                               species_filter_labels = NULL,
                               species_filter_hash = "none") {
  if(is.null(internal_pool)) internal_pool <- set_db_pool()
  on.exit(pool::poolClose(internal_pool), add = TRUE)

  if(verbose) message("Processing deployment: ", deployment_id)

  dep_info <- get_deployment_info(deployments, deployment_id)
  audio_files_subset <- get_audio_files_for_deployment(audio_files, deployment_id, temporal_filtering)

  list_mode <- !is.null(species_filter_labels)

  birdnet_inference_list_weekly <- vector("list", length = length(unique(audio_files_subset$week)))

  for(week_i in seq_along(unique(audio_files_subset$week))) {
    week <- unique(audio_files_subset$week)[week_i]
    if(week == -999) week <- NULL

    # Species-list mode: location/time are inert. Pass NULL coordinates/week so
    # no eBird prediction is attempted, and override the model's species set
    # with the explicit list below.
    if (list_mode) {
      latitude <- NULL
      longitude <- NULL
      week <- NULL
    } else {
      latitude  <- round(dep_info$coordinates[2], coordinates_decimal_places)
      longitude <- round(dep_info$coordinates[1], coordinates_decimal_places)
    }

    model_info <- prepare_birdnet_model(latitude = latitude,
                                        longitude = longitude,
                                        week =  week,
                                        min_confidence = occurence_min_confidence,
                                        birdnet_params_list = birdnet_params_list,
                                        tflite_num_threads = tflite_num_threads,
                                        species_filter_hash = species_filter_hash)
    bnm <- model_info$bnm
    birdnet_params <- model_info$birdnet_params

    # Explicit list overrides the spatiotemporal species set entirely.
    if (list_mode) {
      bnm$species <- data.frame(label = species_filter_labels,
                                stringsAsFactors = FALSE)
    }

    # get_possible_species must run before upsert_birdnet_settings so species_ids
    # can be written to settings_species in the same transaction
    possible_species <- get_possible_species(bnm$species, species)
    settings_id <- upsert_birdnet_settings(internal_pool, bnm, birdnet_params,
                                           species_ids = possible_species$species_id)

    birdnet_inference_list_weekly[[week_i]] <- run_birdnet_inference(audio_files_subset,
                                                                     bnm,
                                                                     birdnet_params,
                                                                     settings_id,
                                                                     possible_species)
  }

  return(
    dplyr::bind_rows(birdnet_inference_list_weekly)
  )

}

# ------------------ Internal helper functions ------------------

#' @keywords internal
get_deployment_info <- function(deployments, deployment_id) {
  deployment <- deployments |>
    dplyr::filter(.data$deployment_id == !!deployment_id)

  xy <- deployment |>
    sf::st_coordinates() |>
    apply(2, mean)

  return(list(deployment = deployment, coordinates = xy))
}

#' @keywords internal
get_audio_files_for_deployment <- function(audio_files, deployment_id, temporal_filtering) {
  audio_files_subset <- audio_files |>
    dplyr::filter(.data$deployment_id == !!deployment_id)

  if(temporal_filtering){
    audio_files_subset <- audio_files_subset |>
      dplyr::mutate(week = round(mean(lubridate::week(timestamp_start))*52/50))
  } else {
    audio_files_subset$week <- -999
  }

  return(audio_files_subset)
}

#' @keywords internal
prepare_birdnet_model <- function(latitude, longitude, week, min_confidence,
                                  birdnet_params_list = list(), tflite_num_threads,
                                  species_filter_hash = "none") {
  bnm <- setup_birdnet_model(version = "v2.4",
                             latitude = latitude,
                             longitude = longitude,
                             week = week,
                             min_confidence = min_confidence,
                             tflite_num_threads = tflite_num_threads)

  # In species-list mode the spatiotemporal keys play no role in inference and
  # are stored as JSON null (NA -> null via na="null" at serialisation). The
  # species_filter hash is what makes the settings row identity distinct, so
  # different species lists never collide on the model_params `@>` dedup.
  list_mode <- !identical(species_filter_hash, "none")

  defaults <- list(
    model_name = bnm$model_name,
    model_version = bnm$model_version,
    min_confidence = 0.2,
    chunk_overlap_s = 0,
    use_bandpass = TRUE,
    bandpass_fmin = 150L,
    bandpass_fmax = 15000L,
    apply_sigmoid = TRUE,
    sigmoid_sensitivity = 1,
    keep_empty = FALSE,
    locale = "de",
    latitude             = if (list_mode) NA_real_    else latitude,
    longitude            = if (list_mode) NA_real_    else longitude,
    week                 = if (list_mode) NA_integer_ else week,
    ebird_min_confidence = if (list_mode) NA_real_    else min_confidence,
    species_filter       = species_filter_hash
  )

  birdnet_params <- modifyList(defaults, birdnet_params_list)
  list(bnm = bnm, birdnet_params = birdnet_params)
}

#' @keywords internal
upsert_birdnet_settings <- function(pool, bnm, birdnet_params, species_ids) {
  settings <- data.frame(
    model_name = bnm$model_name,
    model_version = bnm$model_version,
    min_conf = birdnet_params$min_confidence,
    overlap = birdnet_params$chunk_overlap_s,
    locale = birdnet_params$locale,
    model_params = I(list(birdnet_params))
  )

  # na = "null" so that inert spatiotemporal keys (latitude/longitude/week/
  # ebird_min_confidence in species-list mode) serialise to JSON null rather
  # than the string "NA". This single serialisation is reused for both the
  # `@>` existence check and the INSERT, keeping settings identity consistent.
  settings$model_params <- lapply(settings$model_params, jsonlite::toJSON,
                                  auto_unbox = TRUE, na = "null")

  query <- "SELECT * FROM import.settings WHERE model_params @> $1::jsonb;"
  existing <- dbGetQuery(pool, query, params = settings$model_params)

  if(nrow(existing) == 0){
    new_settings_id <- upsert_settings_df(pool, settings)
    replace_settings_species(conn = pool,
                             settings_id = as.integer(new_settings_id),
                             species_id = as.integer(na.omit(species_ids)))
  } else {
    new_settings_id <- existing$settings_id[1]
  }
  new_settings_id
}

#' Read the BirdNET label set for a model version
#'
#' Thin wrapper around birdnetR so tests can stub the label lookup without a
#' working Python/model backend.
#'
#' @param version Character. BirdNET model version. Default "v2.4".
#' @param language Character. Label language. Default "en_us".
#' @return Character vector of BirdNET labels ("Genus species_Common Name").
#' @noRd
.birdnet_model_labels <- function(version = "v2.4", language = "en_us") {
  model <- birdnetR::birdnet_model_tflite(version = version, language = language)
  birdnetR::read_labels(birdnetR::labels_path(model, language = language))
}

#' Resolve an explicit species filter to BirdNET labels
#'
#' Maps a vector of \code{species_id}s (from \code{lut_species_code}) to the
#' BirdNET label strings the model uses (\code{"Genus species_Common Name"}),
#' validating against both the lookup table and the model's own label set.
#'
#' Aborts if any id is absent from \code{species_lut}, or if a requested
#' species has no matching label in the model (i.e. the model cannot detect
#' it). Matching is by exact scientific name; the evalpam LUT is generated from
#' the BirdNET label set, so exact matching is reliable and this abort also
#' catches LUT/model version drift.
#'
#' @param species_ids Integer vector of species ids to restrict inference to.
#' @param species_lut Data frame with \code{species_id} and
#'   \code{species_scientific} columns.
#' @param model_labels Character vector of BirdNET labels for the model
#'   (e.g. from \code{.birdnet_model_labels()}).
#' @return Character vector of BirdNET labels to pass as \code{filter_species}.
#' @noRd
resolve_species_filter_labels <- function(species_ids, species_lut, model_labels) {
  ids <- unique(as.integer(species_ids))
  ids <- ids[!is.na(ids)]
  if (length(ids) == 0) {
    stop("`species_ids` contains no usable (non-NA) ids.")
  }

  hit <- species_lut[match(ids, species_lut$species_id), , drop = FALSE]
  missing_ids <- ids[is.na(hit$species_id)]
  if (length(missing_ids) > 0) {
    stop("species_ids not found in lut_species_code: ",
         paste(missing_ids, collapse = ", "))
  }

  requested_sci <- hit$species_scientific
  # BirdNET label format: "Genus species_Common Name" — scientific part is
  # everything before the first underscore.
  sci_of_label <- sub("_.*$", "", model_labels)

  not_in_model <- setdiff(requested_sci, sci_of_label)
  if (length(not_in_model) > 0) {
    stop("species not detectable by this BirdNET model (no matching label): ",
         paste(not_in_model, collapse = ", "))
  }

  model_labels[sci_of_label %in% requested_sci]
}

#' @keywords internal
get_possible_species <- function(bnm_species, species_lut) {
  # bnm$species is NULL when occurence_min_confidence == 0 (no location filter)
  if (is.null(bnm_species)) {
    species_list <- species_lut |>
      dplyr::select(species_scientific, species_id)
  } else {
    # BirdNET label format: "Genus species_Common Name" — split on first underscore
    species_list <- data.frame(
      species_scientific = (bnm_species$label |> stringr::str_split("_", simplify = TRUE, n = 2))[, 1]
    ) |>
      dplyr::left_join(species_lut, by = "species_scientific") |>
      dplyr::select(species_scientific, species_id)
  }
  # Drop entries with no match in the LUT — they cannot be stored in the DB
  species_list |> dplyr::filter(!is.na(species_id))
}

#' @keywords internal
run_birdnet_inference <- function(audio_files_subset, bnm, birdnet_params, settings_id, possible_species_df) {
  inference_list <- vector("list", length = nrow(audio_files_subset))

  for(i in seq_len(nrow(audio_files_subset))){
    res <- apply_birdnet_model(
      audio_file = audio_files_subset$full_path[i],
      birdnet_setup = bnm,
      birdnet_params = birdnet_params
    )

    if(!is.null(res$error)){
      res <- data.frame(audio_file_id = audio_files_subset$audio_file_id[i],
                        settings_id = settings_id,
                        begin_time_ms = NA_integer_,
                        end_time_ms = NA_integer_,
                        confidence = NA_integer_,
                        species_id = NA_integer_,
                        behavior_id = NA_integer_,
                        error_type= paste0("failed_", res$error$type),
                        analysed_at = res$prediction_time
                        )
    }else{
      prediction_time <- res$prediction_time
      res <- res$prediction_raw |>
        dplyr::select(-common_name) |>
        dplyr::mutate(
          audio_file_id = audio_files_subset$audio_file_id[i],
          begin_time_ms = as.integer(round(start * 1000)),
          end_time_ms   = as.integer(round(end * 1000)),
          settings_id   = settings_id
        ) |>
        dplyr::rename(species_scientific = scientific_name) |>
        dplyr::left_join(possible_species_df, by = "species_scientific") |>
        dplyr::filter(!is.na(species_id)) |>
        dplyr::mutate(
          behavior_id = NA_integer_,
          confidence = as.integer(round(confidence * 1000)),
          error_type = NA_character_
        ) |>
        dplyr::mutate(analysed_at = prediction_time) |>
        dplyr::select(audio_file_id, settings_id, begin_time_ms, end_time_ms, confidence, species_id, behavior_id, error_type, analysed_at) |>
        # Deduplicate: keep highest-confidence row per (file, window, species)
        dplyr::arrange(dplyr::desc(confidence)) |>
        dplyr::distinct(audio_file_id, settings_id, begin_time_ms, end_time_ms, species_id, .keep_all = TRUE)
    }

    inference_list[[i]] <- res


  }

  dplyr::bind_rows(inference_list)
}
