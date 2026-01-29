#establish connection

pool <- set_db_pool()

# get deployments, audio_files and species lut
deployments <- sf::st_read(pool, DBI::Id("import", "deployments"))
audio_files <- DBI::dbReadTable(pool, DBI::Id("import", "audio_files"))
species <- DBI::dbReadTable(pool, DBI::Id("lut_species_code"))

# join audio_files and deployments to generate full path to audio_file
audio_files <- audio_files |>
  dplyr::select(deployment_id, audio_file_id, relative_path) |>
  dplyr::left_join(deployments |>
                     dplyr::select(deployment_id, deployment_path) |>
                     sf::st_drop_geometry()) |>
  dplyr::mutate(full_path = paste0(deployment_path, relative_path))


#parameterize birdnet

# set long  / lat based on deployments (should be more fine-grained)
xy <- deployments |>
  sf::st_coordinates() |>
  apply(2, mean)

# set week based on deployments (should be more fine-grained)
week = round(mean(lubridate::week(deployments$start_datetime))*52/50)

# set occurence threshold (this will ultimately define the potential species)
occurence_min_confidence = 0.03

# initialize birdnet model
bnm <- setup_birdnet_model(version = "v2.4", latitude = xy[2], longitude = xy[1], week = week, min_confidence = occurence_min_confidence)

# full parameter list (will be stored as json blob in database)
birdnet_params <- list(
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
  latitude = xy[2],
  longitude = xy[1],
  week = week,
  ebird_min_confidence = occurence_min_confidence
)

# generate settings data frame compatible with database structure
settings <- data.frame(
  model_name = bnm$model_name,
  model_version = bnm$model_version,
  min_conf = birdnet_params$min_confidence,
  overlap = birdnet_params$chunk_overlap_s,
  locale = birdnet_params$locale,
  model_params = I(list(birdnet_params))
)

# generate json object from parameter list
settings$model_params <- lapply(settings$model_params, jsonlite::toJSON, auto_unbox = TRUE)

# check if these settings are already in the database (otherwise there will be - harmless - redudancy) and either upsert or get existing settings id
query <- "
SELECT *
FROM import.settings
WHERE model_params @> $1::jsonb
"

setting_available <- dbGetQuery(pool, query, params = settings$model_params)

if(nrow(setting_available) == 0){
  new_settings_id <- upsert_settings_df(pool, settings)

  # if it is a new setting, also update species-settings junction table (this will delete previous entries for this settings id)
  species_list <- data.frame(species_scientific = (bnm$species$label |>
                                                     stringr::str_split("_", simplify = TRUE, n = 2))[,1])

  possible_species_df <- species_list |>
    dplyr::left_join(species) |>
    dplyr::select(species_scientific, species_id)

  replace_settings_species(conn = pool, settings_id = new_settings_id, possible_species_df$species_id)

}else{
  new_settings_id <- setting_available$settings_id[i]
}

# generate birdnet inference
birdnet_inference_list <- vector("list", length = nrow(audio_files))

for(i in seq(nrow(audio_files))){
  birdnet_inference_list[[i]] <- apply_birdnet_model(audio_files$full_path[i],
                      birdnet_setup = bnm,
                      use_arrow = FALSE,
                      batch_size = 1L,
                      birdnet_params = birdnet_params)$prediction_raw |>
    dplyr::select(-common_name) |>
    dplyr::mutate(audio_file_id = audio_files$audio_file_id[i]) |>
    dplyr::mutate(begin_time_ms = as.integer(round(start * 1000))) |>
    dplyr::mutate(end_time_ms = as.integer(round(end * 1000))) |>
    dplyr::rename(species_scientific = scientific_name) |>
    dplyr::mutate(settings_id = new_settings_id) |>
    dplyr::left_join(possible_species_df) |>
    dplyr::mutate(behavior_id = NA_integer_) |>
    dplyr::mutate(confidence = as.integer(round(confidence * 1000))) |>
    dplyr::select(audio_file_id, settings_id, begin_time_ms, end_time_ms, confidence, species_id, behavior_id)
}

# upload inference
birdnet_inference <- birdnet_inference_list |>
  dplyr::bind_rows() |>
  upsert_results_df(conn = pool)

pool::poolClose(pool)


