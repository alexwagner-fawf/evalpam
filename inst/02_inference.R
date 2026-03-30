library(evalpam)

Sys.setenv("https_proxy"  = "http://proxy.rlp:8080")
library(future)
library(future.apply)
options(future.workdir = normalizePath(getwd(), winslash = "/", mustWork = TRUE))
# prerequirement is to setup a conda environment with python 3.10
# Sys.setenv("RETICULATE_PYTHON" = "C:/Users/awagner/.conda/envs/birdnet_py309/python.exe")
# reticulate::use_python("C:/Users/awagner/.conda/envs/birdnet_py309/python.exe")
# reticulate::use_condaenv("birdnet_py309", required = TRUE)
#
# reticulate::py_require(c("numpy>=1.23.5", "protobuf==3.19.6", "tensorflow-gpu==2.10.1", "birdnet==0.1.7"), action = "add")
# reticulate::py_install(
#   c(
#     "numpy>=1.23.5", "protobuf==3.19.6", "tensorflow-gpu==2.10.1", "birdnet==0.1.7"
#   ),
#   pip = TRUE,
#   pip_ignore_installed = TRUE, pip_options = "--no-deps"
# )
#
# reticulate::py_run_string("
# import tensorflow as tf
# print('TF version:', tf.__version__)
# print('GPU devices:', tf.config.list_physical_devices('GPU'))
# ")
# environment works but not gpu support
PYTHON_PATH <- "C:/Users/awagner/.conda/envs/pam_py310/python.exe"
# Sys.setenv("RETICULATE_PYTHON" = "C:/Users/awagner/.conda/envs/pam_py310/python.exe")
# reticulate::use_python("C:/Users/awagner/.conda/envs/pam_py310/python.exe")
# reticulate::use_condaenv("pam_py310", required = TRUE)
# reticulate::py_require(c("numpy>=1.23.5", "birdnet==0.1.7"), action = "add")
#reticulate::py_install(c("numpy==1.23.5", "birdnet==0.1.7"), pip = TRUE, pip_ignore_installed = TRUE)
#Optional: check which Python is being used
#
# reticulate::py_config()
#
pool <- set_db_pool()

# reticulate::py_config()

###### define processing params
project_id = 1
occurence_min_confidence = 0.03 #e bird occurrence confidence threshold for species filtering
spatial_filtering = TRUE #will filter species list spatially (e bird occurrence data) - if FALSE, occurrence min confidence will be set to 0
temporal_filtering = TRUE # will group audio_files by week (1 - 50) to generate time specific species list (will be reset to FALSE if spatial_filtering is FALSE)
birdnet_params_list = list() #here, parameters can be specified (for instance changing default minimum confidence of inference results from 0.2 to 0.1), an empty list will result in default settings
upload_inference = TRUE #this will upload inference results

#####
if(!spatial_filtering){
  occurence_min_confidence <- 0
  temporal_filtering <- FALSE
}

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

temp_results_folder <- file.path(project_folder, "inference_results_temp")
dir.create(temp_results_folder,
           showWarnings = FALSE)


###
#
# audio_files_f <- audio_files |>
#   dplyr::filter(audio_file_id == 1)
# deployment_id = 2
# process deployments (use spatial and temporal information as species list filters as indicated before)

finished_deployments <- list.files(temp_results_folder) |>
  tools::file_path_sans_ext() |>
  as.integer()

remaining_deployments <- as.integer(deployments$deployment_id)[!as.integer(deployments$deployment_id) %in% finished_deployments]

# for(deployment_id in remaining_deployments){
#
#   temp_file_name <- file.path(temp_results_folder, paste0(deployment_id, ".fst"))
#
#   result <- process_deployment_birdnet(
#     deployment_id = deployment_id,
#     deployments = deployments,
#     audio_files = audio_files,
#     temporal_filtering = temporal_filtering,
#     occurence_min_confidence = occurence_min_confidence,
#     birdnet_params_list = list(keep_empty = TRUE),
#     tflite_num_threads = 1
#   )
#
#   if(file.exists(temp_file_name) & is.data.frame(result)) file.remove(temp_file_name)
#
#   fst::write_fst(result, temp_file_name)
#   rm(result)
#   }

# Set up future

plan(multisession, workers = 4)

Sys.setenv(TMPDIR = "C:/temp")
Sys.setenv(TMP = "C:/temp")
Sys.setenv(TEMP = "C:/temp")

deployment_ids <- remaining_deployments

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
    RETICULATE_PYTHON = python_path,
    OMP_NUM_THREADS = 1,
    MKL_NUM_THREADS = 1,
    OPENBLAS_NUM_THREADS = 1,
    TF_CPP_MIN_LOG_LEVEL = 2  # suppress TF info logs
  )

  library(reticulate)

  Sys.setenv("RETICULATE_PYTHON" = "C:/Users/awagner/.conda/envs/pam_py310/python.exe")
  use_python(python_path, required = TRUE)
  py_require(c("numpy>=1.23.5", "birdnet==0.1.7"), action = "add")
  py_config()
  library(evalpam)
  library(sf)
  library(dplyr)

  # rebuild sf object INSIDE worker
  deployments <- deployments_export |>
    dplyr::mutate(geometry = sf::st_as_sfc(geometry_wkt, crs = 4326)) |>
    sf::st_as_sf()

  temp_file_name <- file.path(temp_results_folder, paste0(deployment_id, ".fst"))

  result_df <- process_deployment_birdnet(
    deployment_id = deployment_id,
    deployments = deployments,
    audio_files = audio_files,
    temporal_filtering = temporal_filtering,
    occurence_min_confidence = occurence_min_confidence,
    birdnet_params_list = birdnet_params_list,
    species = species,
    verbose = FALSE,
    tflite_num_threads = 1
  )

  if (file.exists(temp_file_name) && is.data.frame(result_df)) {
    file.remove(temp_file_name)
  }

  fst::write_fst(result_df, temp_file_name)
  return(temp_file_name)
}
# do not pass sf object but dataframe
deployments_export <- deployments |>
  dplyr::mutate(geometry_wkt = sf::st_as_text(geometry)) |>
  sf::st_drop_geometry()

temp_files <- future_lapply(
  as.integer(deployment_ids),
  FUN = process_deployment_worker,
  deployments = deployments_export,
  audio_files = audio_files,
  species = species,
  temporal_filtering = temporal_filtering,
  occurence_min_confidence = occurence_min_confidence,
  birdnet_params_list = birdnet_params_list,
  python_path = PYTHON_PATH,
  temp_results_folder = temp_results_folder,
  future.seed = TRUE
)


# get inference
birdnet_inference <- list.files(temp_results_folder,
                                full.names = TRUE,
                                pattern = ".fst") |>
  lapply(fst::read_fst) |>
  dplyr::bind_rows()

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
      dplyr::filter(!is.na(species_id)) |>
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

