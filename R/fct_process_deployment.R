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
#'
#' @return Data frame of BirdNET inference results.
#' @export
process_deployment_birdnet <- function(deployment_id,
                               deployments,
                               audio_files,
                               temporal_filtering,
                               occurence_min_confidence,
                               birdnet_params_list = list(),
                               verbose = TRUE,
                               internal_pool = NULL,
                               coordinates_decimal_places = 1L) {
  if(is.null(internal_pool)) internal_pool <- set_db_pool()
  on.exit(pool::poolClose(internal_pool), add = TRUE)

  if(verbose) message("Processing deployment: ", deployment_id)

  dep_info <- get_deployment_info(deployments, deployment_id)
  audio_files_subset <- get_audio_files_for_deployment(audio_files, deployment_id, temporal_filtering)

  birdnet_inference_list_weekly <- vector("list", length = length(unique(audio_files_subset$week)))

  for(week_i in seq_along(unique(audio_files_subset$week))) {
    week <- unique(audio_files_subset$week)[week_i]
    if(week == -999) week <- NULL

    model_info <- prepare_birdnet_model(latitude = round(dep_info$coordinates[2], coordinates_decimal_places),
                                        longitude =  round(dep_info$coordinates[1], coordinates_decimal_places),
                                        week =  week,
                                        min_confidence = occurence_min_confidence,
                                        birdnet_params_list = birdnet_params_list)
    bnm <- model_info$bnm
    birdnet_params <- model_info$birdnet_params

    settings_id <- upsert_birdnet_settings(internal_pool, bnm, birdnet_params)
    possible_species <- get_possible_species(bnm$species, species)

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
prepare_birdnet_model <- function(latitude, longitude, week, min_confidence, birdnet_params_list = list()) {
  bnm <- setup_birdnet_model(version = "v2.4",
                             latitude = latitude,
                             longitude = longitude,
                             week = week,
                             min_confidence = min_confidence)

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
    latitude = latitude,
    longitude = longitude,
    week = week,
    ebird_min_confidence = min_confidence
  )

  birdnet_params <- modifyList(defaults, birdnet_params_list)
  list(bnm = bnm, birdnet_params = birdnet_params)
}

#' @keywords internal
upsert_birdnet_settings <- function(pool, bnm, birdnet_params) {
  settings <- data.frame(
    model_name = bnm$model_name,
    model_version = bnm$model_version,
    min_conf = birdnet_params$min_confidence,
    overlap = birdnet_params$chunk_overlap_s,
    locale = birdnet_params$locale,
    model_params = I(list(birdnet_params))
  )

  settings$model_params <- lapply(settings$model_params, jsonlite::toJSON, auto_unbox = TRUE)

  query <- "SELECT * FROM import.settings WHERE model_params @> $1::jsonb;"
  existing <- dbGetQuery(pool, query, params = settings$model_params)

  if(nrow(existing) == 0){
    new_settings_id <- upsert_settings_df(pool, settings)
    replace_settings_species(conn = pool,
                             settings_id = as.integer(new_settings_id),
                             species_id = as.integer(birdnet_params$species_ids))
  } else {
    new_settings_id <- existing$settings_id[1]
  }
  new_settings_id
}

#' @keywords internal
get_possible_species <- function(bnm_species, species_lut) {
  species_list <- data.frame(
    species_scientific = (bnm_species$label |> stringr::str_split("_", simplify = TRUE, n = 2))[,1]
  )
  species_list |>
    dplyr::left_join(species_lut, by = "species_scientific") |>
    dplyr::select(species_scientific, species_id)
}

#' @keywords internal
run_birdnet_inference <- function(audio_files_subset, bnm, birdnet_params, settings_id, possible_species_df) {
  inference_list <- vector("list", length = nrow(audio_files_subset))

  for(i in seq_len(nrow(audio_files_subset))){
    res <- apply_birdnet_model(
      audio_file = audio_files_subset$full_path[i],
      birdnet_setup = bnm,
      birdnet_params = birdnet_params
    )$prediction_raw

    res <- res |>
      dplyr::select(-common_name) |>
      dplyr::mutate(
        audio_file_id = audio_files_subset$audio_file_id[i],
        begin_time_ms = as.integer(round(start * 1000)),
        end_time_ms   = as.integer(round(end * 1000)),
        settings_id   = settings_id
      ) |>
      dplyr::rename(species_scientific = scientific_name) |>
      dplyr::left_join(possible_species_df, by = "species_scientific") |>
      dplyr::mutate(
        behavior_id = NA_integer_,
        confidence = as.integer(round(confidence * 10000))
      ) |>
      dplyr::select(audio_file_id, settings_id, begin_time_ms, end_time_ms, confidence, species_id, behavior_id)

    inference_list[[i]] <- res
  }

  dplyr::bind_rows(inference_list)
}
