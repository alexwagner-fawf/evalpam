pool <- set_db_pool()
deployments <- sf::st_read(pool, DBI::Id("import", "deployments"))
audio_files <- DBI::dbReadTable(pool, DBI::Id("import", "audio_files"))
species <- DBI::dbReadTable(pool, DBI::Id("lut_species_code"))


audio_files <- audio_files |>
  dplyr::select(deployment_id, audio_file_id, relative_path) |>
  dplyr::left_join(deployments |>
                     dplyr::select(deployment_id, deployment_path) |>
                     sf::st_drop_geometry()) |>
  dplyr::mutate(full_path = paste0(deployment_path, relative_path))

xy <- deployments |>
  sf::st_coordinates() |>
  apply(2, mean)

week = round(mean(lubridate::week(deployments$start_datetime))*52/50)

bnm <- setup_birdnet_model(version = "v2.4", latitude = xy[2], longitude = xy[1], week = week, min_confidence = 0.03)

birdnet_params <- list(
  min_confidence = 0.2,
  chunk_overlap_s = 0,
  use_bandpass = TRUE,
  bandpass_fmin = 150L,
  bandpass_fmax = 15000L,
  apply_sigmoid = TRUE,
  sigmoid_sensitivity = 1,
  keep_empty = FALSE,
  locale = "de"
)

settings <- data.frame(
  model_name = bnm$model_name,
  model_version = bnm$model_version,
  min_conf = birdnet_params$min_confidence,
  overlap = birdnet_params$chunk_overlap_s,
  locale = birdnet_params$locale,
  model_params = I(list(birdnet_params))
)

settings$model_params <- lapply(settings$model_params, jsonlite::toJSON, auto_unbox = TRUE)

new_settings_id <- upsert_settings_df(pool, settings)

species_list <- data.frame(species_scientific = (bnm$species$label |>
  stringr::str_split("_", simplify = TRUE, n = 2))[,1])

possible_species_df <- species_list |>
  dplyr::left_join(species) |>
  dplyr::select(species_scientific, species_id)

replace_settings_species(conn = pool, settings_id = new_settings_id, possible_species_df$species_id)


for(i in seq(nrow(audio_files))){
  apply_birdnet_model(audio_files$full_path[i],
                      birdnet_setup = bnm,
                      use_arrow = FALSE,
                      batch_size = 1L,
                      birdnet_params = birdnet_params)$prediction_raw |>
    dplyr::select(-common_name) |>
    dplyr::mutate(audio_file_id = audio_files$audio_file_id[i]) |>
    dplyr::rename(begin_time_s = start) |>
    dplyr::rename(end_time_s = end) |>
    dplyr::rename(species_scientific = scientific_name) |>
    dplyr::mutate(settings_id = new_settings_id) |>
    dplyr::left_join(possible_species_df) |>
    dplyr::mutate(behavior_id = NA) |>
    dplyr::select(audio_file_id, settings_id, begin_time_s, end_time_s, confidence, species_id, behavior_id)
}

