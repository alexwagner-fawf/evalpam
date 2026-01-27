#' Retrieve Local File Information for Audio Deployments
#'
#' Scans a project folder structure to index audio files and extract metadata
#' from EXIF data. Creates deployment and audio file index CSV files with
#' timestamps, file paths, and technical specifications.
#'
#' @param project_id Integer. Unique identifier for the project as provided in the database.
#' @param project_folder Character. Path to the root project folder containing
#'   deployment subdirectories.
#' @param folder_depth Integer. Number of folder levels to traverse from
#'   `project_folder` to reach deployment directories. Default is 2.
#' @param force_tz Character or NULL. Timezone to force for timestamp data
#'   (e.g., "UTC", "America/New_York"). If NULL, no timezone conversion is applied.
#'   Default is NULL.
#' @param parse_datetime Logical. If TRUE, parses timestamps from filenames using
#'   a custom or default parsing function. If FALSE, uses EXIF metadata timestamps.
#'   Default is TRUE.
#' @param custom_parse_fun Function or NULL. Custom function to parse timestamps
#'   from relative file paths. Should take a character vector of paths and return
#'   POSIXct timestamps. If NULL, uses default parsing (expects format
#'   "YYYYMMDD_HHMMSS" in filename). Default is NULL.
#' @param force_exif Logical. If TRUE, forces re-reading of EXIF data even if
#'   audio file index already exists. Default is FALSE.
#' @param default_required_annotation_type_id Integer. Default annotation type ID
#'   to assign to all audio files. Default is 3 (which .
#'
#' @return List of created audio_file_indices + list of deployment_index. Function is also called for its side effects:
#'   \itemize{
#'     \item Creates or updates `deployment_index.csv` in project folder
#'     \item Creates `<deployment_name>_audio_file_index.csv` for each deployment
#'   }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Traverses the folder structure to the specified depth to identify deployments
#'   \item Creates deployment names by concatenating folder path components
#'   \item For each deployment, extracts EXIF metadata from audio files (or reads
#'     from existing index if available)
#'   \item Parses or extracts timestamps from files
#'   \item Calculates start and end datetimes for each deployment
#'   \item Creates/updates deployment index with metadata placeholders
#' }
#'
#' Timestamp extraction follows this priority (when `parse_datetime = FALSE`):
#' \enumerate{
#'   \item MediaCreateDate (from EXIF)
#'   \item CreateDate (from EXIF)
#'   \item FileModifyDate (from EXIF)
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage with default settings
#' retrieve_local_file_info(
#'   project_id = 123,
#'   project_folder = "/path/to/project"
#' )
#'
#' # Parse timestamps from filenames with custom timezone
#' retrieve_local_file_info(
#'   project_id = 123,
#'   project_folder = "/path/to/project",
#'   folder_depth = 3,
#'   force_tz = "America/Chicago"
#' )
#'
#' # Use EXIF timestamps instead of filename parsing
#' retrieve_local_file_info(
#'   project_id = 123,
#'   project_folder = "/path/to/project",
#'   parse_datetime = FALSE
#' )
#'
#' # Custom timestamp parsing function
#' my_parser <- function(paths) {
#'   # Extract timestamps from custom filename format
#'   basename(paths) |>
#'     stringr::str_extract("\\d{8}-\\d{6}") |>
#'     strptime(format = "%Y%m%d-%H%M%S")
#' }
#'
#' retrieve_local_file_info(
#'   project_id = 123,
#'   project_folder = "/path/to/project",
#'   custom_parse_fun = my_parser
#' )
#' }
#'
#' @export

retrieve_local_file_info <- function(project_id,
                                     project_folder,
                                     folder_depth = 2,
                                     force_tz = NULL,
                                     parse_datetime = TRUE,
                                     custom_parse_fun = NULL,
                                     force_exif = FALSE,
                                     default_required_annotation_type_id = 3){

  project_folder <- normalizePath(project_folder, winslash = "/", mustWork = FALSE)

  stopifnot(dir.exists(project_folder))
  stopifnot(is.numeric(folder_depth) && folder_depth > 0)
  stopifnot(is.numeric(project_id))

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

  if(length(deployment_paths) == 0) stop("no deployments found at given folder depth. Reduce the folder depth instead")

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

  created_audio_file_indices <- character()

  for(deployment_i in seq(nrow(deployments_table))){

    dir = deployments_table$deployment_path[deployment_i]
    dpl_name = deployments_table$deployment_name[deployment_i]

    audio_file_index_file <- paste0(project_folder, paste0("/", dpl_name, "_audio_file_index.csv")) |>
      normalizePath(winslash = "/", mustWork = FALSE)

    if(!file.exists(audio_file_index_file) | force_exif){

      message(paste0("reading audio files of ", dpl_name, " (", dir, ")"))

      audio_files_table<- exifr::read_exif(
        dir,
        recursive = TRUE,
        tags = c("SourceFile", "SampleRate", "FileModifyDate", "Duration", "CreateDate", "MediaCreateDate"),
        quiet = TRUE
      ) |>
        dplyr::mutate(SourceFile = normalizePath(SourceFile, winslash = "/", mustWork = FALSE)) |>
        dplyr::mutate(SourceFile = stringr::str_remove_all(SourceFile, dir)) |>
        dplyr::rename(relative_path = SourceFile) |>
        dplyr::rename(sample_rate = SampleRate) |>
        dplyr::rename(duration_s = Duration) |>
        dplyr::mutate(sample_rate = as.integer(sample_rate)) |>
        dplyr::mutate(duration_s = as.integer(duration_s))

      audio_files_table <- audio_files_table |>
        extract_audio_timestamps(parse_datetime = parse_datetime,
                                 custom_parse_fun = custom_parse_fun,
                                 force_tz = force_tz)


      audio_files_table <- audio_files_table |>
        dplyr::mutate(deleted = FALSE) |>
        dplyr::mutate(deployment_id = dpl_name) |> #placeholder for ID insert later on
        dplyr::mutate(required_annotation_type_id = default_required_annotation_type_id)|>  # assuming one annotation type for all audio files in a project
        dplyr::select(deployment_id, sample_rate, relative_path, timestamp_start, duration_s, deleted, required_annotation_type_id)

      current_deployment_timestamps <-  audio_files_table |>
        dplyr::group_by(deployment_id) |>
        dplyr::summarise(start_datetime = min(timestamp_start, na.rm = TRUE),
                         end_datetime = max(timestamp_start, na.rm = TRUE))



      if(nrow(current_deployment_timestamps) > 0){
        deployment_timestamps <- deployment_timestamps |>
          dplyr::bind_rows(current_deployment_timestamps)
      }

      readr::write_csv(audio_files_table, audio_file_index_file)

      created_audio_file_indices <- append(created_audio_file_indices, audio_file_index_file)

    }else{
      message(paste0("skipping ", dpl_name, " (", dir, ")"))
    }


  }


  # add timestamps information to deployment
  deployments_table <- deployments_table |>
    dplyr::left_join(deployment_timestamps |>
                       dplyr::filter(!is.na(deployment_id)) |>
                       dplyr::mutate(deployment_id = as.character(deployment_id)),
                     by = c("deployment_name" = "deployment_id")) |>
    dplyr::filter(!is.na(deployment_name))


  if(file.exists(deployment_index_file)){
    deployment_index <- readr::read_csv(deployment_index_file)

    new_deployments <- deployments_table |>
                         dplyr::filter(!deployment_names %in% deployment_index$deployment_name)

    if(nrow(new_deployments) > 0){
      deployment_index <- deployment_index |>
        dplyr::bind_rows(new_deployments)

      readr::write_csv(deployment_index, deployment_index_file)
    }else{
      message("no new deployments")
    }

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

  return(
    list(
      audio_file_indices = created_audio_file_indices,
      deployment_index = deployment_index_file
    )
  )
}


#' Extract Timestamps from Audio Files
#'
#' Extracts timestamps either by parsing filenames or reading EXIF metadata,
#' with optional timezone conversion.
#'
#' @param audio_files_table Data frame containing audio file metadata with
#'   columns including `relative_path` and optionally EXIF timestamp columns
#'   (`MediaCreateDate`, `CreateDate`, `FileModifyDate`).
#' @param parse_datetime Logical. If TRUE, parses timestamps from filenames.
#'   If FALSE, uses EXIF metadata timestamps. Default is TRUE.
#' @param custom_parse_fun Function or NULL. Custom function to parse timestamps
#'   from relative file paths. Should take a character vector of paths and return
#'   POSIXct timestamps. If NULL, uses default parsing. Default is NULL.
#' @param force_tz Character or NULL. Timezone to force for timestamp data
#'   (e.g., "UTC", "America/New_York"). If NULL, no timezone conversion is applied.
#'
#' @return Data frame with added `timestamp_start` column containing parsed
#'   or extracted timestamps.
#'
#' @details
#' Default filename parsing expects format: `*_YYYYMMDD_HHMMSS.*`
#' (extracts last two underscore-separated components before file extension).
#'
#' EXIF timestamp priority (when `parse_datetime = FALSE`):
#' \enumerate{
#'   \item MediaCreateDate
#'   \item CreateDate
#'   \item FileModifyDate
#' }
#'
#' @keywords internal
extract_audio_timestamps <- function(audio_files_table,
                                     parse_datetime = TRUE,
                                     custom_parse_fun = NULL,
                                     force_tz = NULL) {

  if (parse_datetime) {
    # Default parsing function for filenames
    default_parse_fun <- function(relative_path) {
      relative_path |>
        basename() |>
        tools::file_path_sans_ext() |>
        stringr::str_split(pattern = "_", simplify = TRUE) |>
        as.data.frame() %>%
        dplyr::select(ncol(.) - 1, ncol(.)) |>
        apply(1, paste, collapse = " ") |>
        strptime(format = "%Y%m%d %H%M%S")
    }

    # Use custom or default parsing function
    parse_fun <- if (!is.null(custom_parse_fun)) custom_parse_fun else default_parse_fun
    timestamps <- parse_fun(audio_files_table$relative_path)

  } else {
    # Extract from EXIF metadata with fallback priority
    timestamps <- dplyr::case_when(
      "MediaCreateDate" %in% names(audio_files_table) ~ audio_files_table$MediaCreateDate,
      "CreateDate" %in% names(audio_files_table) ~ audio_files_table$CreateDate,
      TRUE ~ audio_files_table$FileModifyDate
    )
  }

  # Add timestamps to table
  audio_files_table$timestamp_start <- timestamps

  # Apply timezone conversion if specified
  if (!is.null(force_tz)) {
    audio_files_table <- audio_files_table |>
      dplyr::mutate(timestamp_start = lubridate::force_tz(timestamp_start, force_tz))
  }

  return(audio_files_table)
}
