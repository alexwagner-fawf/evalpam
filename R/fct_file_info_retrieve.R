retrieve_local_file_info <- function(conn, project_id, project_folder, folder_depth = 2, force_tz = NULL, parse_datetime = TRUE, custom_parse_fun = NULL){


  project_folder ="/home/alex/Dokumente/sound_db/project_2"


  deployment_index_file <- paste0(project_folder, "/deployment_index.csv") |>
    normalizePath(winslash = "/", mustWork = FALSE)


  start_folders <- project_folder

  for(i in seq_len(folder_depth)){
    start_folders <- unlist(
      lapply(start_folders,
             list.dirs,
             full.names = TRUE,
             recursive = FALSE),
      use.names = FALSE
    )
  }

  deployment_paths <- start_folders |>
    normalizePath(winslash = "/")

  # create names based on folder depth
  deployment_names <- fs::path_split(deployment_paths) |>
    lapply(function(x) tail(x, folder_depth+1)) |>
    sapply(paste, collapse = "_")


  deployments_table <- dplyr::tibble(
    deployment_name = deployment_names,
    deployment_path = deployment_paths
  )


  deployment_timestamps <- data.frame(deployment_id = NA,
                                      start_datetime = NA,
                                      end_datetime = NA)

  for(dir in deployment_paths){
    audio_files_table<- exifr::read_exif(
      dir,
      recursive = TRUE,
      tags = c("SourceFile", "SampleRate", "FileModifyDate", "Duration", "CreateDate", "MediaCreateDate"),
      quiet = TRUE
    ) |>
      dplyr::mutate(SourceFile = stringr::str_remove_all(SourceFile, project_folder)) |>
      dplyr::rename(relative_path = SourceFile) |>
      dplyr::rename(sample_rate = SampleRate) |>
      dplyr::rename(duration_s = Duration) |>
      dplyr::mutate(sample_rate = as.integer(sample_rate)) |>
      dplyr::mutate(duration_s = as.integer(duration_s))

    if(parse_datetime){
      default_parse_fun <- function(relative_path){
        relative_path |>
          basename() |>
        tools::file_path_sans_ext() |>
          stringr::str_split(pattern = "_", simplify = TRUE) |>
          as.data.frame() %>%
          dplyr::select(ncol(.)-1, ncol(.)) |>
          apply(1, paste, collapse = " ") |>
          strptime(format = "%Y%m%d %H%M%S")
      }

      if(is.null(custom_parse_fun)){
        parsed_timestamps <- audio_files_table$relative_path |>
          default_parse_fun()
      }else{
        parsed_timestamps <- audio_files_table$relative_path |>
          custom_parse_fun()
      }

    }else{
      if("MediaCreateDate" %in% names(audio_files_table)){
        exif_timestamps <- audio_files_table$MediaCreateDate
      }else{
        if("CreateDate" %in% names(audio_files_table)){
          exif_timestamps <- audio_files_table$CreateDate
        }else{
          exif_timestamps <- audio_files_table$FileModifyDate
        }
      }
    }

    if(parse_datetime){
      audio_files_table$timestamp_start <- parsed_timestamps
    }else{
      audio_files_table$timestamp_start <- exif_timestamps
    }

    if(!is.null(force_tz)){
      audio_files_table <- audio_files_table |>
        dplyr::mutate(timestamp_start = lubridate::force_tz(timestamp_start, force_tz))
    }

    audio_files_table <- audio_files_table |>
      dplyr::mutate(deleted = FALSE) |>
      dplyr::mutate(deployment_id = dir) |> #placeholder for ID insert later on
      dplyr::mutate(required_annotation_type_id = 3)|>  # assuming WEAK_PRES as default annotation mode
      dplyr::select(deployment_id, sample_rate, relative_path, timestamp_start, duration_s, deleted, required_annotation_type_id)

    deployment_timestamps <- deployment_timestamps |>
      dplyr::bind_rows(
        audio_files_table |>
          dplyr::group_by(deployment_id) |>
          dplyr::summarise(start_datetime = min(timestamp_start),
                           end_datetime = max(timestamp_start))

      )


    current_deployment_name <- dir |>
      stringr::str_remove(project_folder) |>
      normalizePath(winslash = "/", mustWork = FALSE) |>
      stringr::str_replace_all("/", "_")

    audio_file_index_file <- paste0(project_folder, paste0("/", current_deployment_name, "_audio_file_index.csv")) |>
      normalizePath(winslash = "/", mustWork = FALSE)

    readr::write_csv(audio_files_table, audio_file_index_file)
  }


  # add timestamps information to deployment
  deployments_table <- deployments_table |>
    dplyr::left_join(deployment_timestamps |> dplyr::filter(!is.na(deployment_id)), by = c("deployment_path" = "deployment_id"))


  if(file.exists(deployment_index_file)){
    deployment_index <- readr::read_csv(deployment_index_file)

    deployment_index <- deployment_index |>
      dplyr::filter(!deployment_names %in% deployments_table$deployment_name) |>
      dplyr::bind_rows(deployments_table) |>
      dplyr::bind_rows(deployment_index_file)
  }else{
    deployments_table |>
      dplyr::mutate(project_id = project_id, .before = deployment_name) |>
      dplyr::mutate(device_manufacturer = NA,
                    device_modelname = NA,
                    valid = NA,
                    notes = NA,
                    geometry_x_4326 = NA,
                    geometry_y_4326 = NA) |>
      readr::write_csv(deployment_index_file)
  }



}
