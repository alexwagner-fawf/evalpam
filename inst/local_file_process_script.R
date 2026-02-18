

###### define processing params
project_id = 2
occurence_min_confidence = 0.03 #e bird occurence confidence threshold for species filtering
spatial_filtering = TRUE #will filter species list spatially (e bird occurence data) - if FALSE, occurence min confidence will be set to 0
temporal_filtering = TRUE # will group audio_files by week (1 - 50) to generate time specific species list (will be reset to FALSE if spatial_filtering is FALSE)
birdnet_params_list = list() #here, parameters can be specified (for instance changing default minimum confidence of inference results from 0.2 to 0.1), an empty list will result in default settings
upload_inference = TRUE #this will upload inference results

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
  dplyr::select(deployment_id, audio_file_id, relative_path,  timestamp_start) |>
  dplyr::collect()

species <- DBI::dbReadTable(pool, DBI::Id("lut_species_code"))

# join audio_files and deployments to generate full path to audio_file
audio_files <- audio_files |>
  dplyr::left_join(deployments |>
                     dplyr::select(deployment_id, deployment_path) |>
                     sf::st_drop_geometry()) |>
  dplyr::mutate(
    relative_path = sub("^[\\\\/]+", "", relative_path),  # trim leading slashes
    full_path = file.path(deployment_path, relative_path)
  )

audio_files$full_path[1] <- paste0(audio_files$full_path[1], ".123")
# rm(results_list)
# process deployments (use spatial and temporal information as species list filters as indicated before)
results_list <- lapply(as.list(deployments$deployment_id),
       process_deployment_birdnet,
       deployments = deployments,
       audio_files = audio_files,
       temporal_filtering = temporal_filtering,
       occurence_min_confidence = occurence_min_confidence,
       birdnet_params_list = list()
)


# store inference

birdnet_inference <- results_list |>
  dplyr::bind_rows()



# get project_folder based on common path segment (this assumes equal parent directory for deployments!)
project_folder <- deployments |>
  dplyr::pull("deployment_path") |>
  normalizePath(winslash = "/", mustWork = FALSE) |> #should be forward slash already
  stringr::str_split(pattern = "/", simplify = TRUE) |>
  apply(2, function(x){
    if(length(unique(x)) == 1){
      x[1]
    }else{
      ""
    }
  }) |>
  paste0(collapse = "/") |>
  stringr::str_remove("/$")


# create inference results directory
dir.create(file.path(project_folder, "inference_results"),
           showWarnings = FALSE)


# define file names for index and results

# index file only stores unique combinations of settings_id and audio_file_id
# to check if a file is locally stored already and filter new inference results
# this assumes that audio files are never partially inferenced
# TODO: use index in process_deployment to skip inference
index_file <- file.path(project_folder,
                        "inference_results",
                        "inference_results_index.fst")|>
  normalizePath(winslash = "/", mustWork = FALSE)

# there will always be unique results file for each inference session to avoid
# reading and appending large files in R
results_file <- file.path(project_folder,
                          "inference_results",
                          paste0(stringr::str_remove_all(Sys.time(),
                                                         "\\-|\\:|\\.| "),
                                 "_inference_results.fst")) |>
  normalizePath(winslash = "/", mustWork = FALSE)

# read index file or create a dummy
if(file.exists(index_file)){
  inference_index <- fst::read_fst(index_file)
}else{
  inference_index <- dplyr::tibble(
    audio_file_id = integer(),
    settings_id = integer(),
    status = character(),
    analysed_at = NA_integer_,
  )
}


# filter birdnet inference based on index
# the process will also update results if they now succeeded and previously failed
birdnet_inference_new <- birdnet_inference |>
  dplyr::anti_join(inference_index |>
                     dplyr::filter(status == "success"),
                   by = c("audio_file_id", "settings_id"))

# store new birdnet inference (if any)
if(nrow(birdnet_inference_new) == 0){
  message("no new inferences")
}else{

  # export new inference results
  birdnet_inference_new |>
    dplyr::filter(is.na(error_type)) |>
    dplyr::select(-error_type, -analysed_at) |>
  fst::write_fst(results_file)

  # update local index file
  add2index_file <- birdnet_inference_new |>
    dplyr::select(audio_file_id, settings_id, error_type, analysed_at) |>
    dplyr::distinct() |>
    dplyr::mutate(error_type = ifelse(is.na(error_type), "success", error_type)) |>
    dplyr::rename(status = error_type)

  if(file.exists(index_file)){
    # append to index and update if status changed - unlikely unsolved edge case: status changes from success to failure
    add2index_file |>
      dplyr::anti_join(inference_index) |>
      dplyr::bind_rows(inference_index) |>
      dplyr::arrange(audio_file_id, settings_id, dplyr::desc(analysed_at)) |> #sort by time
      dplyr::distinct(audio_file_id, settings_id, .keep_all = TRUE) |> #keep new result
      fst::write_fst(index_file)

  }else{
    add2index_file |>
      fst::write_fst(index_file)
  }


  if(upload_inference){
    result_ids <- birdnet_inference_new |>
      dplyr::filter(is.na(error_type)) |>
      upsert_results_df(conn = pool)

    add2index_file |>
      upsert_analysis_log_df(conn = pool)
  }
}

# this will upload new inference results
# if there are previous inference runs that have not been uploaded
# one has to read all inference files and upload them individually
# there will be no redundancy as the database/upload function also checks for uniqueness of
# settings_id, audio_file_id and time


pool::poolClose(pool)


