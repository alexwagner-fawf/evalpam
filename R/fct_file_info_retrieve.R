#' Retrieve Local File Information for Audio Deployments
#'
#' Scans a project folder structure to index audio files and extract metadata
#' from EXIF data. Creates deployment and audio file index files (.fst) with
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
#' @param list_files_retries Integer. Maximum retry attempts for each directory
#'   scan in \code{find_audio_files}. Default is 3.
#' @param list_files_verify Logical. If TRUE, performs multiple scans per
#'   directory and only accepts results when consecutive scans are identical.
#'   Recommended for network drives. Default is TRUE.
#' @param list_files_min_stable_scans Integer. Consecutive identical scans
#'   required to accept a directory listing. Only used when
#'   \code{list_files_verify = TRUE}. Default is 2.
#' @param default_required_annotation_type_id Integer. Default annotation type
#'   ID to assign to all audio files. Default is 3.
#'
#' @return Named list with:
#'   \itemize{
#'     \item \code{new_audio_file_indices} – paths to newly created .fst index files
#'     \item \code{all_audio_file_indices} – paths to all .fst index files found
#'     \item \code{deployment_index} – path to deployment_index.csv
#'   }
#'   Side effects: creates \code{<project_folder>/audio_file_indices/<name>_afi.fst}
#'   per deployment and \code{<project_folder>/deployment_index.csv}.
#'
#' @details
#' File discovery uses \code{\link{find_audio_files}} which scans each directory
#' with retry and optional verification scanning to handle network micro-disconnects.
#' EXIF reading retries up to 5 times for any files not returned in a previous attempt.
#'
#' Timestamp extraction priority when \code{parse_datetime = FALSE}:
#' \enumerate{
#'   \item MediaCreateDate
#'   \item CreateDate
#'   \item FileModifyDate
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
                                     list_files_retries = 3,
                                     list_files_verify = TRUE,
                                     list_files_min_stable_scans = 2,
                                     default_required_annotation_type_id = 3) {

  project_folder <- normalizePath(project_folder, winslash = "/", mustWork = FALSE)

  stopifnot(dir.exists(project_folder))
  stopifnot(is.numeric(folder_depth) && folder_depth > 0)
  stopifnot(is.numeric(project_id))

  # Check ExifTool is accessible before doing any work
  tryCatch(
    exiftoolr::exif_version(),
    error = function(e) stop(
      "ExifTool not found or not configured.\n",
      "  Windows: call exiftoolr::configure_exiftoolr(command = 'path/to/exiftool.exe')\n",
      "  Linux:   sudo apt install libimage-exiftool-perl\n",
      "  Original error: ", e$message,
      call. = FALSE
    )
  )

  deployment_index_file <- file.path(project_folder, "deployment_index.csv") |>
    normalizePath(winslash = "/", mustWork = FALSE)

  audio_file_indices_folder <- file.path(project_folder, "audio_file_indices") |>
    normalizePath(winslash = "/", mustWork = FALSE)

  dir.create(audio_file_indices_folder, showWarnings = FALSE)

  # Traverse folder structure to deployment level
  start_folders <- project_folder
  for (i in seq_len(folder_depth)) {
    start_folders <- unlist(
      lapply(start_folders, list.dirs, full.names = TRUE, recursive = FALSE),
      use.names = FALSE
    )
  }

  deployment_paths <- normalizePath(start_folders, winslash = "/") |>
    # exclude the indices subfolder this function manages itself
    (\(p) p[p != audio_file_indices_folder])()

  if (length(deployment_paths) == 0)
    stop("No deployments found at given folder depth. Reduce folder_depth.")

  deployment_names <- fs::path_split(deployment_paths) |>
    lapply(function(x) tail(x, folder_depth + 1)) |>
    sapply(paste, collapse = "_")

  deployments_table <- dplyr::tibble(
    deployment_name = deployment_names,
    deployment_path = deployment_paths
  )

  deployment_timestamps <- data.frame(deployment_id = NA_character_,
                                      start_datetime = NA,
                                      end_datetime   = NA)

  created_audio_file_indices <- character()

  for (deployment_i in seq_len(nrow(deployments_table))) {

    dir      <- deployments_table$deployment_path[deployment_i]
    dpl_name <- deployments_table$deployment_name[deployment_i]

    audio_file_index_file <- file.path(audio_file_indices_folder,
                                       paste0(dpl_name, "_afi.fst")) |>
      normalizePath(winslash = "/", mustWork = FALSE)

    if (!file.exists(audio_file_index_file) || force_exif) {

      message(Sys.time(), ": reading audio files of ", dpl_name, " (", dir, ")")

      # Phase 1: resilient file discovery
      audio_files <- find_audio_files(
        folder           = dir,
        retry_delay      = 0.5,
        max_retries      = list_files_retries,
        verify_complete  = list_files_verify,
        min_stable_scans = list_files_min_stable_scans
      )

      if (length(audio_files) == 0) {
        message("  no audio files found in ", dir)
        next
      }

      audio_files_norm <- normalizePath(audio_files, winslash = "/", mustWork = FALSE)

      # Phase 2: EXIF reading with per-file retry
      audio_files_remaining <- audio_files_norm
      audio_files_table_raw <- dplyr::tibble(SourceFile = character(),
                                             Duration   = double())
      n_retries <- 0

      while (length(audio_files_remaining) > 0 && n_retries < 5) {
        n_retries <- n_retries + 1
        message("  EXIF attempt #", n_retries,
                " (", length(audio_files_remaining), " files remaining)")

        try({
          exif_try <- exiftoolr::exif_read(
            audio_files_remaining,
            recursive = FALSE,
            tags      = c("SourceFile", "SampleRate", "FileModifyDate",
                          "Duration", "CreateDate", "MediaCreateDate"),
            quiet     = TRUE
          ) |>
            dplyr::mutate(
              SourceFile = normalizePath(SourceFile, winslash = "/", mustWork = FALSE)
            )

          audio_files_table_raw   <- dplyr::bind_rows(audio_files_table_raw, exif_try)
          audio_files_remaining   <- setdiff(audio_files_remaining,
                                             audio_files_table_raw$SourceFile)
        })
      }

      if (length(audio_files_remaining) > 0) {
        missing_csv <- tempfile(fileext = ".csv")
        readr::write_csv(data.frame(missing_files = audio_files_remaining), missing_csv)
        warning(length(audio_files_remaining),
                " file(s) could not be read after 5 EXIF attempts. ",
                "List written to: ", missing_csv, call. = FALSE)
      }

      # Rename columns, handle missing SampleRate gracefully
      if ("SampleRate" %in% names(audio_files_table_raw)) {
        audio_files_table <- dplyr::rename(audio_files_table_raw, sample_rate = SampleRate)
      } else {
        audio_files_table <- dplyr::mutate(audio_files_table_raw,
                                           sample_rate = NA_integer_)
      }

      audio_files_table <- audio_files_table |>
        dplyr::mutate(SourceFile = stringr::str_remove_all(SourceFile,
                                                           stringr::fixed(dir))) |>
        dplyr::rename(relative_path = SourceFile,
                      duration_s    = Duration) |>
        dplyr::mutate(sample_rate = as.integer(sample_rate),
                      duration_s  = as.integer(duration_s))

      audio_files_table <- extract_audio_timestamps(
        audio_files_table,
        parse_datetime   = parse_datetime,
        custom_parse_fun = custom_parse_fun,
        force_tz         = force_tz
      )

      audio_files_table <- audio_files_table |>
        dplyr::mutate(
          deleted                      = FALSE,
          deployment_id                = dpl_name,
          required_annotation_type_id  = default_required_annotation_type_id
        ) |>
        dplyr::select(deployment_id, sample_rate, relative_path,
                      timestamp_start, duration_s, deleted,
                      required_annotation_type_id)

      current_timestamps <- audio_files_table |>
        dplyr::group_by(deployment_id) |>
        dplyr::summarise(start_datetime = min(timestamp_start, na.rm = TRUE),
                         end_datetime   = max(timestamp_start, na.rm = TRUE),
                         .groups = "drop")

      if (nrow(current_timestamps) > 0) {
        deployment_timestamps <- dplyr::bind_rows(deployment_timestamps,
                                                  current_timestamps)
      }

      # Store timestamp as numeric — fst does not support POSIXct
      audio_files_table |>
        dplyr::mutate(timestamp_start = as.numeric(timestamp_start)) |>
        fst::write_fst(path = audio_file_index_file)

      created_audio_file_indices <- append(created_audio_file_indices,
                                           audio_file_index_file)

    } else {
      message(Sys.time(), ": skipping ", dpl_name, " (index already exists)")
    }
  }

  # Only keep deployments that produced a .fst index (skip empty/non-audio dirs)
  deployments_table <- deployments_table |>
    dplyr::filter(file.exists(
      file.path(audio_file_indices_folder, paste0(deployment_name, "_afi.fst"))
    ))

  # Join timestamps back onto deployments
  deployments_table <- deployments_table |>
    dplyr::left_join(
      deployment_timestamps |>
        dplyr::filter(!is.na(deployment_id)) |>
        dplyr::mutate(deployment_id = as.character(deployment_id)),
      by = c("deployment_name" = "deployment_id")
    ) |>
    dplyr::filter(!is.na(deployment_name))

  # Update or create deployment index
  if (file.exists(deployment_index_file)) {
    deployment_index <- readr::read_csv(deployment_index_file,
                                        show_col_types = FALSE)

    new_deployments <- deployments_table |>
      dplyr::filter(!deployment_name %in% deployment_index$deployment_name)

    if (nrow(new_deployments) > 0) {
      deployment_index <- dplyr::bind_rows(deployment_index, new_deployments)
      readr::write_csv(deployment_index, deployment_index_file)
    } else {
      message("No new deployments found.")
    }

  } else {
    deployments_table |>
      dplyr::mutate(project_id = project_id, .before = deployment_name) |>
      dplyr::mutate(device_manufacturer          = NA,
                    device_modelname             = NA,
                    valid                        = NA,
                    notes                        = NA,
                    geometry_x_4326              = NA,
                    geometry_y_4326              = NA) |>
      readr::write_csv(deployment_index_file)
  }

  all_audio_file_indices <- list.files(audio_file_indices_folder,
                                       recursive  = FALSE,
                                       full.names = TRUE,
                                       pattern    = "_afi\\.fst$")

  return(list(
    new_audio_file_indices = created_audio_file_indices,
    all_audio_file_indices = all_audio_file_indices,
    deployment_index       = deployment_index_file
  ))
}


#' Extract Timestamps from Audio Files
#'
#' Extracts timestamps either by parsing filenames or reading EXIF metadata,
#' with optional timezone conversion.
#'
#' @param audio_files_table Data frame with columns `relative_path` and
#'   optionally `MediaCreateDate`, `CreateDate`, `FileModifyDate`.
#' @param parse_datetime Logical. If TRUE, parses timestamps from filenames.
#' @param custom_parse_fun Function or NULL. Custom filename parser returning
#'   POSIXct. If NULL, uses default parsing for format `*_YYYYMMDD_HHMMSS.*`.
#' @param force_tz Character or NULL. Timezone to apply via
#'   \code{lubridate::force_tz}.
#'
#' @return Data frame with `timestamp_start` column added.
#' @keywords internal
extract_audio_timestamps <- function(audio_files_table,
                                     parse_datetime   = TRUE,
                                     custom_parse_fun = NULL,
                                     force_tz         = NULL) {

  if (parse_datetime) {
    default_parse_fun <- function(relative_path) {
      relative_path |>
        basename() |>
        tools::file_path_sans_ext() |>
        stringr::str_split(pattern = "_", simplify = TRUE) |>
        as.data.frame() |>
        (\(df) df[, (ncol(df) - 1):ncol(df)])() |>
        (\(df) apply(df, 1, paste, collapse = " "))() |>
        strptime(format = "%Y%m%d %H%M%S")
    }

    parse_fun  <- if (!is.null(custom_parse_fun)) custom_parse_fun else default_parse_fun
    timestamps <- parse_fun(audio_files_table$relative_path)

  } else {
    timestamps <- dplyr::case_when(
      "MediaCreateDate" %in% names(audio_files_table) ~ audio_files_table$MediaCreateDate,
      "CreateDate"      %in% names(audio_files_table) ~ audio_files_table$CreateDate,
      TRUE                                             ~ audio_files_table$FileModifyDate
    )
  }

  audio_files_table$timestamp_start <- timestamps

  if (!is.null(force_tz)) {
    audio_files_table <- audio_files_table |>
      dplyr::mutate(timestamp_start = lubridate::force_tz(timestamp_start, force_tz))
  }

  return(audio_files_table)
}


#' Find Audio Files Recursively with Network Resilience
#'
#' Recursively searches a directory for audio files with built-in retry logic
#' and verification scanning to handle unstable network connections.
#'
#' @param folder Character. Path to the directory to search.
#' @param max_retries Integer. Maximum retry attempts per directory. Default 3.
#' @param retry_delay Numeric. Seconds between retries. Default 0.5.
#' @param pattern Character. Regex for supported audio extensions.
#' @param verify_complete Logical. If TRUE, repeats each scan until consecutive
#'   results are identical. Recommended for network drives. Default TRUE.
#' @param min_stable_scans Integer. Consecutive identical scans required before
#'   accepting a result. Default 2.
#'
#' @return Character vector of unique full file paths.
#' @keywords internal
find_audio_files <- function(folder,
                             max_retries      = 3,
                             retry_delay      = 0.5,
                             pattern          = "\\.(mp3|wav|flac|m4a|aac|ogg|wma)$",
                             verify_complete  = TRUE,
                             min_stable_scans = 2) {

  all_files <- character(0)

  # Phase 1: stable directory listing
  dirs <- NULL
  for (attempt in seq_len(max_retries)) {
    tryCatch({
      dirs_candidate <- list.dirs(folder, full.names = TRUE, recursive = TRUE)
      if (!folder %in% dirs_candidate) dirs_candidate <- c(folder, dirs_candidate)

      if (verify_complete && attempt < max_retries) {
        Sys.sleep(retry_delay)
        dirs_verify <- list.dirs(folder, full.names = TRUE, recursive = TRUE)
        if (!folder %in% dirs_verify) dirs_verify <- c(folder, dirs_verify)

        if (!setequal(dirs_candidate, dirs_verify)) {
          message(sprintf(
            "  Directory list unstable on attempt %d (%d vs %d dirs), retrying...",
            attempt, length(dirs_candidate), length(dirs_verify)
          ))
          next
        }
      }

      dirs <- dirs_candidate
      break

    }, error = function(e) {
      if (attempt < max_retries) {
        message(sprintf("  Attempt %d failed listing directories, retrying in %.1fs...",
                        attempt, retry_delay))
        Sys.sleep(retry_delay)
      } else {
        stop(sprintf("Failed to access directory structure after %d attempts: %s",
                     max_retries, e$message))
      }
    })
  }

  if (is.null(dirs)) stop("Could not retrieve directory list for: ", folder)

  message(sprintf("  Found %d directories to scan", length(dirs)))

  # Phase 2: per-directory file listing with verification
  for (i in seq_along(dirs)) {
    d <- dirs[i]

    if (i %% 10 == 0 || i == length(dirs))
      message(sprintf("  Progress: %d/%d directories scanned", i, length(dirs)))

    dir_files    <- NULL
    stable_count <- 0
    last_result  <- NULL

    for (attempt in seq_len(max_retries)) {
      tryCatch({
        files <- list.files(d, pattern = pattern, ignore.case = TRUE,
                            full.names = TRUE)

        if (verify_complete) {
          if (!is.null(last_result)) {
            if (setequal(files, last_result)) {
              stable_count <- stable_count + 1
              if (stable_count >= min_stable_scans) {
                dir_files <- files
                break
              }
            } else {
              stable_count <- 1
              message(sprintf("  Unstable results in '%s' (%d vs %d files), continuing...",
                              basename(d), length(files), length(last_result)))
            }
          }
          last_result <- files
          Sys.sleep(retry_delay * 0.5)
        } else {
          dir_files <- files
          break
        }

      }, error = function(e) {
        if (attempt < max_retries) {
          message(sprintf("  Attempt %d failed for '%s', retrying in %.1fs...",
                          attempt, basename(d), retry_delay))
          Sys.sleep(retry_delay)
        } else {
          warning(sprintf("Failed to access '%s' after %d attempts: %s",
                          d, max_retries, e$message), call. = FALSE)
        }
      })
    }

    if (is.null(dir_files) && !is.null(last_result)) {
      warning(sprintf("Using potentially incomplete results for '%s' (%d files)",
                      basename(d), length(last_result)), call. = FALSE)
      dir_files <- last_result
    }

    if (!is.null(dir_files)) all_files <- c(all_files, dir_files)
  }

  unique(all_files)
}
