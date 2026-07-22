#' Generate and upload audio clip spectrograms for a project
#'
#' Samples detection results from the database, deduplicates to one clip per
#' time window (keeping the highest-confidence detection as the representative),
#' and calls \code{\link{build_audio_clips_db}} to generate MP3 clips and
#' optionally upload them to \code{import.spectrograms}.
#'
#' @param pool A database connection pool (from \code{\link{set_db_pool}}).
#' @param project_id Integer or NULL. Filter deployments to this project.
#'   \code{NULL} uses all deployments (requires \code{deployment_ids} to be
#'   set, otherwise processes the entire database).
#' @param deployment_ids Integer vector or NULL. Explicit set of deployment IDs
#'   to process. If \code{NULL} (default), deployments are selected from the
#'   project with one representative per geographic location (earliest start
#'   date). Supply this to override the automatic selection entirely.
#' @param n_per_species Integer. Number of detections to select per group
#'   (see \code{grouping_by}). Default 30.
#' @param confidence_selection_mode Character. Passed to
#'   \code{\link{sample_results_table}}. One of \code{"top"}, \code{"random"},
#'   or \code{"stratified"}. Default \code{"top"}.
#' @param grouping_by Character vector. Grouping columns for
#'   \code{\link{sample_results_table}}. Default
#'   \code{c("species_id", "deployment_id")}.
#' @param padding_s Numeric. Seconds of audio context before and after the
#'   detection window. Default 2.
#' @param output_dir Character or NULL. Directory for MP3 cache files. If
#'   \code{NULL} (default), reads the \code{spectogram_folder} environment
#'   variable, falling back to \code{./spectograms}.
#' @param export_to_db Logical. Upload MP3 blobs to \code{import.spectrograms}.
#'   Default \code{TRUE}.
#' @param verbose Logical. Print progress messages. Default \code{TRUE}.
#' @param ... Additional arguments passed to \code{\link{build_audio_clips_db}}.
#'
#' @return Invisibly returns the number of unique clips generated.
#'
#' @export
generate_spectrograms <- function(pool,
                                  project_id                = NULL,
                                  deployment_ids            = NULL,
                                  n_per_species             = 30L,
                                  confidence_selection_mode = "top",
                                  grouping_by               = c("species_id", "deployment_id"),
                                  padding_s                 = 2,
                                  output_dir                = NULL,
                                  export_to_db              = TRUE,
                                  verbose                   = TRUE,
                                  ...) {

  if (is.null(output_dir)) {
    env_dir <- Sys.getenv("spectogram_folder")
    output_dir <- if (nzchar(env_dir)) env_dir else file.path(getwd(), "spectograms")
  }

  # ── Select deployments ───────────────────────────────────────────────────────
  if (!is.null(deployment_ids)) {
    selected_deployments <- dplyr::tbl(pool, DBI::Id("import", "deployments")) |>
      dplyr::filter(.data$deployment_id %in% !!deployment_ids) |>
      dplyr::collect()
    if (verbose) message(sprintf(
      "%d deployment(s) specified explicitly.", nrow(selected_deployments)
    ))
  } else {
    deployments <- sf::st_read(pool, DBI::Id("import", "deployments"), quiet = TRUE)

    if (!is.null(project_id)) {
      deployments <- dplyr::filter(deployments, .data$project_id == !!project_id)
    }

    if (nrow(deployments) == 0) {
      stop("No deployments found",
           if (!is.null(project_id)) paste0(" for project_id=", project_id) else "",
           ".")
    }

    # One representative per location (earliest start date)
    selected_deployments <- deployments |>
      dplyr::group_by(.data$geometry) |>
      dplyr::filter(.data$start_datetime == min(.data$start_datetime, na.rm = TRUE)) |>
      dplyr::ungroup()

    if (verbose) message(sprintf(
      "project_id=%s: %d deployment(s) total, %d selected (one per location).",
      project_id, nrow(deployments), nrow(selected_deployments)
    ))
  }

  # ── Sample detections ────────────────────────────────────────────────────────
  samples <- sample_results_table(
    confidence_selection_mode = confidence_selection_mode,
    n_per_species             = n_per_species,
    deployment_ids            = selected_deployments$deployment_id,
    grouping_by               = grouping_by,
    pool                      = pool
  )

  if (nrow(samples) == 0) {
    stop(
      "sample_results_table() returned 0 rows for deployment_id(s): ",
      paste(selected_deployments$deployment_id, collapse = ", "),
      ".\nCheck that import.results contains inference output for these deployments."
    )
  }

  # ── Deduplicate: one clip per (audio_file_id, begin_time_ms) ─────────────────
  # sample_results_table() groups by (species_id, deployment_id) by default, so
  # the same time window may appear for multiple species. Each window needs exactly
  # one audio clip; keep the highest-confidence row as the representative.
  samples_dedup <- samples |>
    dplyr::arrange(dplyr::desc(.data$confidence)) |>
    dplyr::distinct(.data$audio_file_id, .data$begin_time_ms, .keep_all = TRUE)

  samples_group <- samples_dedup |>
    dplyr::group_by(.data$deployment_id) |>
    dplyr::group_split()

  if (verbose) message(sprintf(
    "Generating %d unique clip(s) across %d deployment(s) (%d total detections sampled).",
    nrow(samples_dedup),
    dplyr::n_distinct(samples_dedup$deployment_id),
    nrow(samples)
  ))

  # ── Generate clips ───────────────────────────────────────────────────────────
  for (i in seq_along(samples_group)) {
    grp <- samples_group[[i]]
    if (verbose) message(sprintf("[%d/%d] deployment=%s  %d clip(s)",
                                 i, length(samples_group),
                                 grp$deployment_id[1L],
                                 nrow(grp)))
    build_audio_clips_db(
      data         = grp,
      pool         = pool,
      padding_s    = padding_s,
      output_dir   = output_dir,
      export_to_db = export_to_db,
      verbose      = FALSE,
      ...
    )
  }

  if (verbose) message("Done.")

  invisible(nrow(samples_dedup))
}
