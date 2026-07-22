#' Ingest audio files for a project into the database
#'
#' Orchestrates the full audio file ingestion pipeline: project upsert, local
#' file scanning via \code{\link{retrieve_local_file_info}}, deployment
#' registration, duplicate timestamp resolution, corrupt-path filtering, and
#' audio file upload.
#'
#' The one project-specific task — building the deployment location table —
#' is left to the caller: construct an sf object with deployment names and
#' point geometries and pass it as \code{deployments_sf}.
#'
#' @param pool A database connection pool (from \code{\link{set_db_pool}}).
#' @param project_name_short Character. Short project identifier (max 30 chars).
#'   Used to look up or create the project in \code{import.projects}.
#' @param project_name_long Character. Full project name. Defaults to
#'   \code{project_name_short}.
#' @param description Character or NULL. Project description.
#' @param contact Character or NULL. Contact person.
#' @param organisation Character or NULL. Organisation name.
#' @param project_folder Character. Root folder whose subdirectories are
#'   deployment directories (at depth \code{folder_depth}).
#' @param deployments_sf An sf object (point geometry, EPSG:4326) with at least
#'   a \code{deployment_name} column whose values match the folder names found
#'   under \code{project_folder}. Optional extra columns accepted by
#'   \code{import.deployments}: \code{device_manufacturer},
#'   \code{device_modelname}, \code{notes}, \code{valid}.
#' @param folder_depth Integer. Directory levels below \code{project_folder} to
#'   scan for deployments. Default 1.
#' @param force_tz Character. Timezone to assign to all scanned timestamps.
#'   Default \code{"UTC"}.
#' @param force_exif Logical. Re-read EXIF even when an index file already
#'   exists. Default \code{FALSE}.
#' @param list_files_retries Integer. Retry count for directory scans (useful
#'   on network drives). Default 3.
#' @param list_files_verify Logical. Accept a directory scan only after
#'   consecutive identical results. Default \code{TRUE}.
#' @param list_files_min_stable_scans Integer. Consecutive identical scans
#'   required. Default 2.
#' @param default_required_annotation_type_id Integer. Annotation type assigned
#'   to all audio files. Default 3.
#' @param max_relative_path_length Integer. Paths longer than this are treated
#'   as corrupt exiftool output and excluded. Default 200.
#' @param remove_corrupt_indices Logical. If \code{TRUE}, deletes the .fst index
#'   files for deployments with corrupt paths so they will be re-scanned on the
#'   next run. Default \code{FALSE} (rows are dropped but files are kept).
#' @param exiftool_exe Character or NULL. Absolute path to the exiftool
#'   executable. Required on Windows when exiftool is not on PATH. Default NULL.
#' @param perl_path Character or NULL. Path to Perl (Windows only, used by
#'   exiftoolr). Default NULL.
#' @param update_if_exists Logical. Update existing deployment and audio_file
#'   records on conflict. Default \code{TRUE}.
#' @param verbose Logical. Print progress messages. Default \code{TRUE}.
#'
#' @return Invisibly returns a named list:
#'   \describe{
#'     \item{project_id}{Integer project_id.}
#'     \item{deployment_ids}{Integer vector of upserted deployment_ids.}
#'     \item{audio_file_ids}{Integer vector of upserted audio_file_ids.}
#'   }
#'
#' @export
ingest_audio_files <- function(pool,
                               project_name_short,
                               project_name_long                    = project_name_short,
                               description                          = NULL,
                               contact                              = NULL,
                               organisation                         = NULL,
                               project_folder,
                               deployments_sf,
                               folder_depth                         = 1L,
                               force_tz                             = "UTC",
                               force_exif                           = FALSE,
                               list_files_retries                   = 3L,
                               list_files_verify                    = TRUE,
                               list_files_min_stable_scans          = 2L,
                               default_required_annotation_type_id  = 3L,
                               max_relative_path_length             = 200L,
                               remove_corrupt_indices               = FALSE,
                               exiftool_exe                         = NULL,
                               perl_path                            = NULL,
                               update_if_exists                     = TRUE,
                               verbose                              = TRUE) {

  stopifnot(inherits(deployments_sf, "sf"))
  stopifnot("deployment_name" %in% names(deployments_sf))

  # ── ExifTool (Windows) ───────────────────────────────────────────────────────
  if (!is.null(exiftool_exe)) {
    exiftoolr::configure_exiftoolr(command = exiftool_exe, perl_path = perl_path)
  }

  # ── Project upsert ───────────────────────────────────────────────────────────
  projects <- DBI::dbReadTable(pool, DBI::Id("import", "projects"))

  if (project_name_short %in% projects$project_name_short) {
    project_id <- projects |>
      dplyr::filter(.data$project_name_short == !!project_name_short) |>
      dplyr::pull(.data$project_id)
    if (verbose) message("Project '", project_name_short, "' found (project_id=", project_id, ").")
  } else {
    project_id <- upsert_project(
      conn               = pool,
      project_name_short = project_name_short,
      project_name_long  = project_name_long,
      description        = description,
      contact            = contact,
      organisation       = organisation
    )
    if (verbose) message("Project '", project_name_short, "' created (project_id=", project_id, ").")
  }

  # ── Scan project folder ──────────────────────────────────────────────────────
  out <- retrieve_local_file_info(
    project_id                          = project_id,
    project_folder                      = project_folder,
    force_tz                            = force_tz,
    folder_depth                        = folder_depth,
    list_files_retries                  = list_files_retries,
    force_exif                          = force_exif,
    list_files_verify                   = list_files_verify,
    list_files_min_stable_scans         = list_files_min_stable_scans,
    default_required_annotation_type_id = default_required_annotation_type_id
  )

  # ── Build deployment sf for upsert ──────────────────────────────────────────
  deployment_index <- readr::read_csv(out$deployment_index, show_col_types = FALSE)

  # Join metadata from deployments_sf onto the file-scan index by deployment_name.
  # Geometry is transferred separately to keep the sf machinery happy.
  meta_df    <- sf::st_drop_geometry(deployments_sf)
  geom_named <- sf::st_geometry(deployments_sf)
  names(geom_named) <- deployments_sf$deployment_name

  deploy_df <- deployment_index |>
    dplyr::left_join(meta_df, by = "deployment_name") |>
    dplyr::filter(.data$deployment_name %in% deployments_sf$deployment_name) |>
    dplyr::mutate(
      project_id = project_id,
      valid      = dplyr::coalesce(.data$valid, TRUE)
    )

  if (nrow(deploy_df) == 0) {
    stop(
      "No deployments matched between the scanned folder names and deployments_sf$deployment_name.\n",
      "Check that deployment folder names match the 'deployment_name' column of deployments_sf."
    )
  }

  deploy_df$geometry <- geom_named[deploy_df$deployment_name]
  deploy_sf <- sf::st_as_sf(deploy_df, crs = sf::st_crs(deployments_sf))

  upserted_deployment_ids <- upsert_deployments_sf(
    conn              = pool,
    sf_deployments    = deploy_sf,
    update_if_exists  = update_if_exists
  )

  if (verbose) message(length(upserted_deployment_ids), " deployment(s) upserted.")

  # Re-read from DB to get canonical deployment_ids (handles pre-existing rows).
  canonical_deployments <- dplyr::tbl(pool, DBI::Id("import", "deployments")) |>
    dplyr::filter(.data$deployment_id %in% upserted_deployment_ids) |>
    dplyr::select(.data$deployment_id, .data$deployment_name) |>
    dplyr::collect()

  # ── Load and bind audio file indices ────────────────────────────────────────
  audio_file_indices <- if (length(out$new_audio_file_indices) > 0) {
    out$new_audio_file_indices
  } else {
    out$all_audio_file_indices
  }

  df_audio <- audio_file_indices |>
    lapply(fst::read_fst) |>
    dplyr::bind_rows() |>
    dplyr::rename(deployment_name = deployment_id) |>
    dplyr::left_join(canonical_deployments, by = "deployment_name") |>
    dplyr::relocate(.data$deployment_id, .before = "deployment_name") |>
    dplyr::select(-.data$deployment_name) |>
    dplyr::filter(.data$deployment_id %in% upserted_deployment_ids) |>
    dplyr::mutate(timestamp_start = as.POSIXct(.data$timestamp_start, tz = "UTC")) |>
    dplyr::filter(!is.na(.data$sample_rate)) |>
    dplyr::group_by(.data$deployment_id) |>
    dplyr::mutate(dupls = duplicated(.data$timestamp_start) |
                    duplicated(.data$timestamp_start, fromLast = TRUE)) |>
    dplyr::ungroup()

  # ── Resolve duplicate timestamps ─────────────────────────────────────────────
  df_audio <- .resolve_timestamp_duplicates(df_audio)

  # ── Filter corrupt exif paths ────────────────────────────────────────────────
  df_audio <- df_audio |>
    dplyr::mutate(path_length = stringr::str_length(.data$relative_path))

  long_path_rows          <- dplyr::filter(df_audio, .data$path_length > max_relative_path_length)
  problematic_dep_ids     <- unique(long_path_rows$deployment_id)

  if (length(problematic_dep_ids) > 0) {
    problematic_names <- canonical_deployments |>
      dplyr::filter(.data$deployment_id %in% problematic_dep_ids) |>
      dplyr::pull(.data$deployment_name)

    warning(
      length(problematic_dep_ids), " deployment(s) have potentially corrupt exif paths ",
      "(relative_path longer than ", max_relative_path_length, " chars): ",
      paste(problematic_names, collapse = ", "), ".\n",
      "These are excluded from the upload.",
      if (remove_corrupt_indices) " Their index files will be deleted so they are re-scanned next run." else
        " Set remove_corrupt_indices = TRUE to force a re-scan."
    )

    if (remove_corrupt_indices) {
      bad_index_files <- audio_file_indices[
        basename(audio_file_indices) %in% paste0(problematic_names, "_afi.fst")
      ]
      file.remove(bad_index_files)
    }

    df_audio <- dplyr::filter(df_audio, !.data$deployment_id %in% problematic_dep_ids)
  }

  df_final <- df_audio |>
    dplyr::select(-.data$dupls, -.data$path_length) |>
    dplyr::filter(!is.na(.data$timestamp_start))

  # ── Upload audio file records ────────────────────────────────────────────────
  audio_file_ids <- upsert_audio_files_df(
    conn             = pool,
    df_audio         = df_final,
    update_if_exists = update_if_exists
  )

  if (verbose) message(length(audio_file_ids), " audio file record(s) upserted.")

  invisible(list(
    project_id      = project_id,
    deployment_ids  = upserted_deployment_ids,
    audio_file_ids  = audio_file_ids
  ))
}


# Re-parse timestamps from filenames for duplicate rows and keep one per window.
# Expected filename suffix pattern: YYYYMMDD_HHMMSS (last two underscore-parts).
.resolve_timestamp_duplicates <- function(df) {
  non_dups <- dplyr::filter(df, !.data$dupls)
  dups     <- dplyr::filter(df, .data$dupls)

  if (nrow(dups) == 0) return(dplyr::bind_rows(non_dups, dups))

  # Extract YYYYMMDD_HHMMSS from anywhere in the filename (regex, not column-split,
  # so extra suffixes like _v2 or _b don't shift the field positions).
  raw <- dups$relative_path |>
    basename() |>
    stringr::str_extract("\\d{8}_\\d{6}")

  dttm_str <- ifelse(
    is.na(raw), NA_character_,
    paste0(
      substr(raw,  1L, 4L), "-", substr(raw, 5L, 6L), "-", substr(raw, 7L, 8L), " ",
      substr(raw, 10L, 11L), ":", substr(raw, 12L, 13L), ":", substr(raw, 14L, 15L)
    )
  )

  dups$timestamp_start <- lubridate::ymd_hms(dttm_str, quiet = TRUE) |>
    lubridate::force_tz("UTC")

  still_na <- sum(is.na(dups$timestamp_start))
  if (still_na > 0) {
    warning(still_na, " duplicate rows still have NA timestamp after filename re-parsing and will be dropped.")
    dups <- dplyr::filter(dups, !is.na(.data$timestamp_start))
  }

  resolved <- dups |>
    dplyr::group_by(.data$timestamp_start, .data$deployment_id) |>
    dplyr::arrange(dplyr::desc(.data$duration_s)) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup()

  dplyr::bind_rows(resolved, non_dups) |>
    dplyr::arrange(.data$deployment_id, .data$timestamp_start)
}
