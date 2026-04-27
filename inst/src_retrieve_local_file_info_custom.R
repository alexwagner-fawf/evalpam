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
#'
#' @export

retrieve_local_file_info_custom <- function(project_id,
                                     project_folder,
                                     folder_depth = 2,
                                     force_tz = NULL,
                                     parse_datetime = TRUE,
                                     custom_parse_fun = NULL,
                                     force_exif = FALSE,
                                     list_files_retries = 3,
                                     list_files_verify = TRUE,
                                     list_files_min_stable_scans = 2,
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

  audio_file_indices_folder <- file.path(project_folder, "audio_file_indices") |>
    normalizePath(winslash = "/", mustWork = FALSE)

  dir.create(audio_file_indices_folder, showWarnings = FALSE)

  for(deployment_i in seq(nrow(deployments_table))){

    dir = deployments_table$deployment_path[deployment_i]
    dpl_name = deployments_table$deployment_name[deployment_i]

    audio_file_index_file <- file.path(audio_file_indices_folder,
                                       paste0(dpl_name, "_afi.fst")) |>
      normalizePath(winslash = "/", mustWork = FALSE)

    if(!file.exists(audio_file_index_file) | force_exif){

      message(paste0(Sys.time(), ": reading audio files of ", dpl_name, " (", dir, ")"))

      # function to safely get all audio files even if list.files fails or returns partial results
      audio_files <- find_audio_files(folder = dir,
                                      retry_delay = 0.5,
                                      max_retries = list_files_retries,
                                      verify_complete = list_files_verify,
                                      min_stable_scans = list_files_min_stable_scans)



      audio_files_norm_unproc <- audio_files_normalized <- normalizePath(audio_files, winslash = "/", mustWork = FALSE)


      if(length(audio_files_normalized) > 0){

        audio_files_table_raw <- dplyr::tibble(
          SourceFile = character(),
          Duration = double()
        )

        n_retries = 0
        n_missing = length(audio_files_norm_unproc)

        while(n_missing > 0| n_retries >= 5){
          n_retries <- n_retries + 1

          message(paste0("try #",  n_retries, " of reading exif data"))

          try({
            exif_try <-  exiftoolr::exif_read(
            audio_files_norm_unproc,
            recursive = FALSE,
            tags = c("SourceFile", "SampleRate", "FileModifyDate", "Duration", "CreateDate", "MediaCreateDate"),
            quiet = TRUE
          ) |>
            dplyr::mutate(SourceFile = normalizePath(SourceFile, winslash = "/", mustWork = FALSE))

          audio_files_table_raw <- dplyr::bind_rows(audio_files_table_raw, exif_try)

          audio_files_norm_unproc <- audio_files_norm_unproc[!audio_files_norm_unproc %in% audio_files_table_raw$SourceFile]
          })

          n_missing <- length(audio_files_norm_unproc)

          if(n_missing > 0) message(paste0(n_missing, " files missing - will retry"))

        }

        if(n_missing > 0){
          message(paste0(n_missing, " files missing - will no longer retry"))
          csvtemp <- tempfile(fileext = ".csv")

          readr::write_csv(data.frame(missing_files = audio_files_norm_unproc), csvtemp)
          message(paste0("missing files listed in ", csvtemp))
        }


#
#       #readr::write_csv(audio_files_table_raw, "test.csv")
#       audio_files_table_raw <- readr::read_csv("test.csv")

      if("SampleRate" %in% names(audio_files_table_raw)){
        audio_files_table <- audio_files_table_raw |>
          dplyr::rename(sample_rate = SampleRate)  |>
          dplyr::mutate(sample_rate = as.integer(sample_rate))
      }else{
        audio_files_table <- audio_files_table_raw |>
          dplyr::mutate(sample_rate = as.integer(NA))
      }

      audio_files_table <- audio_files_table |>
        dplyr::mutate(SourceFile = stringr::str_remove_all(SourceFile, dir)) |>
        dplyr::rename(relative_path = SourceFile) |>
        dplyr::rename(duration_s = Duration) |>
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

      audio_files_table |>
        dplyr::mutate(timestamp_start = as.numeric(timestamp_start)) |> #dttm not supported by fst
        fst::write_fst(path = audio_file_index_file)

      created_audio_file_indices <- append(created_audio_file_indices, audio_file_index_file)
      }else{
        message("no audio files found")
      }


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

  all_audio_file_indices = list.files(audio_file_indices_folder,
                                  recursive = FALSE,
                                  full.names = TRUE,
                                  pattern = "_afi.fst")


  return(
    list(
      new_audio_file_indices = created_audio_file_indices,
      all_audio_file_indices = all_audio_file_indices,
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
        as.data.frame() |>
        (\(df) df[, (ncol(df)-1):ncol(df)])() |>
        (\(df) apply(df, 1, paste, collapse = " "))() |>
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

#' Find Audio Files Recursively with Network Resilience
#'
#' Recursively searches a directory for audio files with built-in retry logic
#' and verification scanning to handle unstable network connections. This function
#' is designed to ensure all files are captured even when list.files() silently
#' returns incomplete results due to network microdisconnects.
#'
#' @param folder Character string. Path to the directory to search.
#' @param max_retries Integer. Maximum number of retry attempts for each
#'   directory scan. Default is 3.
#' @param retry_delay Numeric. Delay in seconds between retry attempts.
#'   Default is 0.5 seconds.
#' @param pattern Character string. Regular expression to retrieve certain file types.
#'   Default retrieves audio files
#' @param verify_complete Logical. If TRUE, performs multiple scans of each
#'   directory and only accepts results when consecutive scans return identical
#'   file lists. This catches cases where list.files() silently returns incomplete
#'   results. Default is TRUE (recommended for network drives).
#' @param min_stable_scans Integer. Number of consecutive identical scans required
#'   to consider results stable and complete. Only used when verify_complete = TRUE.
#'   Default is 2. Increase for very unstable connections.
#'
#' @return Character vector of full file paths to all audio files found.
#'   Supported formats: mp3, wav, flac, m4a, aac, ogg, wma.
#'   Returns unique paths (duplicates removed).
#'
#' @details
#' The function works in two phases:
#' \enumerate{
#'   \item Retrieves the complete directory structure with verification
#'   \item Scans each directory for audio files with verification
#' }
#'
#' When \code{verify_complete = TRUE}, the function performs multiple scans
#' and compares results. This is crucial for network drives where list.files()
#' may silently skip files during temporary connection issues without throwing
#' an error.
#'
#' Progress messages are displayed every 10 directories to help monitor
#' long-running scans. Warnings are issued for directories that could not
#' be fully accessed or showed unstable results.
#'
#' @section Network Drive Recommendations:
#' For flaky network drives:
#' \itemize{
#'   \item Keep \code{verify_complete = TRUE}
#'   \item Increase \code{max_retries} to 5 or more
#'   \item Increase \code{min_stable_scans} to 3 for very unstable connections
#'   \item Run the function multiple times and compare results to verify completeness
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' files <- find_audio_files("Z:/Music")
#' cat(sprintf("Found %d audio files\n", length(files)))
#'
#' # For problematic network drives
#' files <- find_audio_files("Z:/Music",
#'                          max_retries = 5,
#'                          retry_delay = 1,
#'                          min_stable_scans = 3)
#'
#' # Faster but risky (no verification)
#' files <- find_audio_files("C:/LocalMusic", verify_complete = FALSE)
#'
#' # Verify consistency across runs
#' files1 <- find_audio_files("Z:/Music")
#' Sys.sleep(30)
#' files2 <- find_audio_files("Z:/Music")
#' identical(sort(files1), sort(files2))
#' }
#'
#' @seealso \code{\link{count_audio_files}} for getting just the file count
#'
#' @keywords internal
find_audio_files <- function(folder,
                             max_retries = 3,
                             retry_delay = 0.5,
                             pattern = "\\.(mp3|wav|flac|m4a|aac|ogg|wma)$",
                             verify_complete = TRUE,
                             min_stable_scans = 2) {

  all_files <- character(0)

  # Get all directories with retry and verification
  dirs <- NULL
  for (attempt in 1:max_retries) {
    tryCatch({
      dirs_candidate <- list.dirs(folder, full.names = TRUE, recursive = TRUE)
      if (!folder %in% dirs_candidate) dirs_candidate <- c(folder, dirs_candidate)

      # Verify we got a stable result by re-scanning
      if (verify_complete && attempt < max_retries) {
        Sys.sleep(retry_delay)
        dirs_verify <- list.dirs(folder, full.names = TRUE, recursive = TRUE)
        if (!folder %in% dirs_verify) dirs_verify <- c(folder, dirs_verify)

        if (length(dirs_candidate) != length(dirs_verify) ||
            !setequal(dirs_candidate, dirs_verify)) {
          message(sprintf("Directory list unstable on attempt %d (got %d vs %d dirs), retrying...",
                          attempt, length(dirs_candidate), length(dirs_verify)))
          next
        }
      }

      dirs <- dirs_candidate
      break
    }, error = function(e) {
      if (attempt < max_retries) {
        message(sprintf("Attempt %d failed to list directories, retrying in %.1f seconds...",
                        attempt, retry_delay))
        Sys.sleep(retry_delay)
      } else {
        stop(sprintf("Failed to access directory structure after %d attempts: %s",
                     max_retries, e$message))
      }
    })
  }

  if (is.null(dirs)) {
    stop("Could not retrieve directory list")
  }

  message(sprintf("Found %d directories to scan", length(dirs)))

  # Process each directory with verification
  for (i in seq_along(dirs)) {
    d <- dirs[i]

    # Progress reporting every 10 directories
    if (i %% 10 == 0 || i == length(dirs)) {
      message(sprintf("Progress: %d/%d directories scanned", i, length(dirs)))
    }

    dir_files <- NULL
    stable_count <- 0
    last_result <- NULL

    for (attempt in 1:max_retries) {
      tryCatch({
        files <- list.files(d,
                            pattern = pattern,
                            ignore.case = TRUE,
                            full.names = TRUE)

        # Verify completeness by comparing multiple scans
        if (verify_complete) {
          if (!is.null(last_result)) {
            if (setequal(files, last_result)) {
              stable_count <- stable_count + 1
              if (stable_count >= min_stable_scans) {
                dir_files <- files
                break
              }
            } else {
              # Results changed, reset counter
              stable_count <- 1
              message(sprintf("Unstable results in '%s' (got %d vs %d files), continuing scan...",
                              basename(d), length(files), length(last_result)))
            }
          }
          last_result <- files
          Sys.sleep(retry_delay * 0.5)  # Short delay between verification scans
        } else {
          dir_files <- files
          break
        }

      }, error = function(e) {
        if (attempt < max_retries) {
          message(sprintf("Attempt %d failed for directory '%s', retrying in %.1f seconds...",
                          attempt, basename(d), retry_delay))
          Sys.sleep(retry_delay)
        } else {
          warning(sprintf("Failed to access directory '%s' after %d attempts: %s",
                          d, max_retries, e$message))
        }
      })
    }

    # If we exhausted retries but have a last_result, use it with warning
    if (is.null(dir_files) && !is.null(last_result)) {
      warning(sprintf("Using potentially incomplete results for '%s' (%d files)",
                      basename(d), length(last_result)))
      dir_files <- last_result
    }

    if (!is.null(dir_files)) {
      all_files <- c(all_files, dir_files)
    }
  }

  unique(all_files)
}

