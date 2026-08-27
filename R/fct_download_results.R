#' Download analysis-ready BirdNET detection results for a project
#'
#' Pulls all detections from \code{import.results} and joins the lookup and
#' metadata tables (\code{lut_species_code}, \code{lut_behavior_code},
#' \code{import.audio_files}, \code{import.deployments}, \code{import.settings})
#' into a single flat, analysis-ready tibble. Confidence values are converted
#' back to the 0-1 range and per-detection wall-clock timestamps are derived
#' from the audio file's \code{timestamp_start} plus the detection offset, so
#' the result carries human-readable species names, deployment names and real
#' timestamps rather than raw ids.
#'
#' @param pool A database connection pool (from \code{\link{set_db_pool}}).
#' @param project_id Integer or NULL. Restrict to deployments of this project.
#'   \code{NULL} (default) returns detections across all projects.
#' @param settings_ids Integer vector or NULL. Restrict to detections produced
#'   under these inference \code{settings_id} values. \code{NULL} (default)
#'   pools detections from all settings. Use this to download the output of one
#'   specific inference run/configuration.
#' @param deployment_ids Integer vector or NULL. Restrict to these deployments.
#'   \code{NULL} (default) applies no deployment constraint (beyond
#'   \code{project_id}).
#' @param species_ids Integer vector or NULL. Restrict to detections of these
#'   species. \code{NULL} (default) applies no species constraint.
#' @param min_confidence Numeric or NULL. Drop detections with confidence below
#'   this value (on the 0-1 scale). \code{NULL} (default) keeps all.
#' @param include_settings Logical. Join the \code{import.settings} columns
#'   (model name/version, min confidence, overlap, locale). Default \code{TRUE}.
#' @param collect Logical. When \code{TRUE} (default) execute the query and
#'   return a tibble. When \code{FALSE} return the un-collected lazy
#'   \code{dbplyr} query so the caller can add further verbs before collecting.
#'
#' @return A tibble (one row per detection) with, at minimum: \code{result_id},
#'   \code{project_id}, \code{deployment_id}, \code{deployment_name},
#'   \code{longitude}, \code{latitude}, \code{audio_file_id},
#'   \code{relative_path}, \code{file_start} (audio file start), \code{settings_id},
#'   \code{begin_time_ms}, \code{end_time_ms}, \code{detection_start},
#'   \code{detection_end} (wall-clock timestamps), \code{confidence} (0-1),
#'   \code{species_id}, \code{species_scientific}, \code{species_long_en},
#'   \code{species_long_de}, \code{species_short}, \code{behavior_id},
#'   \code{behavior_short}, \code{behavior_long_en}. When
#'   \code{include_settings = TRUE}, additionally \code{model_name},
#'   \code{model_version}, \code{settings_min_conf}, \code{settings_overlap} and
#'   \code{settings_locale}. When \code{collect = FALSE}, the lazy query instead
#'   (timestamp/confidence post-processing is skipped and left to the caller).
#'
#' @examples
#' \dontrun{
#' pool <- set_db_pool()
#' # Everything for project 1
#' res <- download_results(pool, project_id = 1)
#' # Only the detections from one inference configuration
#' res_s <- download_results(pool, project_id = 1, settings_ids = 3)
#' }
#'
#' @export
download_results <- function(pool,
                             project_id       = NULL,
                             settings_ids     = NULL,
                             deployment_ids   = NULL,
                             species_ids      = NULL,
                             min_confidence   = NULL,
                             include_settings = TRUE,
                             collect          = TRUE) {

  settings_ids   <- .normalise_id_filter(settings_ids)
  deployment_ids <- .normalise_id_filter(deployment_ids)
  species_ids    <- .normalise_id_filter(species_ids)

  # ── deployments (project / deployment filter + lon/lat from geometry) ────────
  deployments_tbl <- dplyr::tbl(pool, DBI::Id("import", "deployments"))
  if (!is.null(project_id)) {
    deployments_tbl <- dplyr::filter(deployments_tbl,
                                     .data$project_id == !!as.integer(project_id))
  }
  if (!is.null(deployment_ids)) {
    deployments_tbl <- dplyr::filter(deployments_tbl,
                                     .data$deployment_id %in% !!deployment_ids)
  }
  deployments_tbl <- deployments_tbl |>
    dplyr::mutate(longitude = dplyr::sql("ST_X(geometry)"),
                  latitude  = dplyr::sql("ST_Y(geometry)")) |>
    dplyr::select("deployment_id", "project_id", "deployment_name",
                  "longitude", "latitude")

  # ── audio files (carry the file start timestamp for detection timing) ────────
  audio_files_tbl <- dplyr::tbl(pool, DBI::Id("import", "audio_files")) |>
    dplyr::select("audio_file_id", "deployment_id", "relative_path",
                  file_start = "timestamp_start") |>
    dplyr::inner_join(deployments_tbl, by = "deployment_id")

  # ── results (settings / species / confidence filter) ─────────────────────────
  results_tbl <- dplyr::tbl(pool, DBI::Id("import", "results")) |>
    dplyr::select("result_id", "audio_file_id", "settings_id",
                  "begin_time_ms", "end_time_ms", "confidence",
                  "species_id", "behavior_id")
  if (!is.null(settings_ids)) {
    results_tbl <- dplyr::filter(results_tbl, .data$settings_id %in% !!settings_ids)
  }
  if (!is.null(species_ids)) {
    results_tbl <- dplyr::filter(results_tbl, .data$species_id %in% !!species_ids)
  }
  if (!is.null(min_confidence)) {
    # confidence is stored as round(confidence * 1000) (smallint)
    conf_threshold <- as.integer(round(min_confidence * 1000))
    results_tbl <- dplyr::filter(results_tbl, .data$confidence >= !!conf_threshold)
  }

  # ── lookups ──────────────────────────────────────────────────────────────────
  species_tbl <- dplyr::tbl(pool, DBI::Id("public", "lut_species_code")) |>
    dplyr::select("species_id",
                  species_scientific = "species_scientific",
                  species_long_en    = "species_long_en",
                  species_long_de    = "species_long_de",
                  species_short      = "species_short")

  behavior_tbl <- dplyr::tbl(pool, DBI::Id("public", "lut_behavior_code")) |>
    dplyr::select("behavior_id",
                  behavior_short   = "behavior_short",
                  behavior_long_en = "behavior_long_en")

  query <- results_tbl |>
    dplyr::inner_join(audio_files_tbl, by = "audio_file_id") |>
    dplyr::inner_join(species_tbl, by = "species_id") |>
    dplyr::left_join(behavior_tbl, by = "behavior_id")

  if (isTRUE(include_settings)) {
    settings_tbl <- dplyr::tbl(pool, DBI::Id("import", "settings")) |>
      dplyr::select("settings_id",
                    model_name        = "model_name",
                    model_version     = "model_version",
                    settings_min_conf = "min_conf",
                    settings_overlap  = "overlap",
                    settings_locale   = "locale")
    query <- dplyr::left_join(query, settings_tbl, by = "settings_id")
  }

  if (!isTRUE(collect)) {
    return(query)
  }

  out <- dplyr::collect(query)

  if (nrow(out) == 0) {
    return(out)
  }

  # Post-process into analysis-ready columns: confidence back to 0-1 and
  # wall-clock detection timestamps from the file start plus the ms offset.
  out |>
    dplyr::mutate(
      confidence      = .data$confidence / 1000,
      detection_start = .data$file_start + .data$begin_time_ms / 1000,
      detection_end   = .data$file_start + .data$end_time_ms / 1000
    ) |>
    dplyr::relocate(
      "result_id", "project_id", "deployment_id", "deployment_name",
      "longitude", "latitude", "audio_file_id", "relative_path", "file_start",
      "settings_id", "begin_time_ms", "end_time_ms",
      "detection_start", "detection_end", "confidence",
      "species_id", "species_scientific", "species_long_en", "species_long_de",
      "species_short", "behavior_id", "behavior_short", "behavior_long_en"
    )
}
