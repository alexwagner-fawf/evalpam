

###### define processing params
project_id = 2
occurence_min_confidence = 0.03 #e bird occurence confidence threshold for species filtering
spatial_filtering = TRUE #will filter species list spatially (e bird occurence data) - if FALSE, occurence min confidence will be set to 0
temporal_filtering = TRUE # will group audio_files by week (1 - 50) to generate time specific species list (will be reset to FALSE if spatial_filtering is FALSE)
birdnet_params_list = list() #here, parameters can be specified (for instance changing default minimum confidence of inference results from 0.2 to 0.1), an empty list will result in default settings

#####
if(!spatial_filtering){
  occurence_min_confidence <- 0
  temporal_filtering <- FALSE
}


#establish connection
pool <- set_db_pool()

# get deployments, audio_files and species lut
deployments <- sf::st_read(pool, DBI::Id("import", "deployments")) |>
  dplyr::filter(.data$project_id == !!project_id)

audio_files <- dplyr::tbl(pool, DBI::Id("import", "audio_files")) |>
  dplyr::filter(deployment_id %in% deployments$deployment_id) |>
  dplyr::collect()

species <- DBI::dbReadTable(pool, DBI::Id("lut_species_code"))

# join audio_files and deployments to generate full path to audio_file
audio_files <- audio_files |>
  dplyr::select(deployment_id, audio_file_id, relative_path,  timestamp_start) |>
  dplyr::left_join(deployments |>
                     dplyr::select(deployment_id, deployment_path) |>
                     sf::st_drop_geometry()) |>
  dplyr::mutate(full_path = paste0(deployment_path, relative_path))

# process deployments (use spatial and temporal information as species list filters as indicated before)
results_list <- lapply(as.list(deployments$deployment_id),
       process_deployment_birdnet,
       deployments = deployments,
       audio_files = audio_files,
       temporal_filtering = temporal_filtering,
       occurence_min_confidence = occurence_min_confidence,
       birdnet_params_list = list()
)


# upload inference
birdnet_inference <- results_list |>
  dplyr::bind_rows() |>
  upsert_results_df(conn = pool)

pool::poolClose(pool)


