#' Prepare spectrogram input data
#'
#' Selects detection results from the database to be used as input for
#' spectrogram generation. The selection can be based on highest confidence,
#' random sampling, or stratified sampling across confidence levels.
#'
#' The function queries the `import.audio_files` and `import.results`
#' tables, filters by deployment IDs, groups the results according to
#' user-defined variables, and applies the chosen sampling strategy.
#'
#' @param confidence_selection_mode Character string specifying how results
#'   should be selected. One of:
#'   \describe{
#'     \item{"top"}{Select the \code{n_per_species} highest-confidence detections per group.}
#'     \item{"random"}{Randomly sample \code{n_per_species} detections per group.}
#'     \item{"stratified"}{Sample detections stratified across equally-spaced confidence intervals, where number of intervals equals \code{n_per_species}}
#'   }
#'
#' @param n_per_species Integer. Number of detections to select per group.
#'
#' @param deployment_ids Vector of deployment IDs used to filter audio files.
#'
#' @param species_ids Optional integer vector of species IDs. When supplied,
#'   only detections of these species are considered. \code{NULL} (default)
#'   applies no species constraint. Combined with \code{audio_file_ids} as a
#'   logical AND.
#'
#' @param audio_file_ids Optional integer vector of audio-file IDs. When
#'   supplied, only detections in these files are considered, in addition to
#'   the \code{deployment_ids} filter. \code{NULL} (default) applies no file
#'   constraint.
#'
#' @param settings_ids Optional integer vector of \code{settings_id} values.
#'   When supplied, only detections produced under these inference settings are
#'   considered. \code{NULL} (default) applies no settings constraint (results
#'   from all settings are pooled).
#'
#' @param result_ids Optional integer vector of \code{result_id} values. How it
#'   is used depends on \code{result_id_mode}. \code{NULL} (default) applies no
#'   result-id constraint.
#'
#' @param result_id_mode Character, one of \code{"exclusive"} (default) or
#'   \code{"prioritize"}; only relevant when \code{result_ids} is supplied.
#'   \describe{
#'     \item{\code{"exclusive"}}{The listed \code{result_ids} act as a hard
#'       filter: rows whose \code{result_id} is not in the list are removed
#'       before sampling.}
#'     \item{\code{"prioritize"}}{The listed \code{result_ids} are preferred but
#'       not required. Within each group the listed rows are taken first (chosen
#'       via \code{confidence_selection_mode} and capped at \code{n_per_species});
#'       if a group holds fewer than \code{n_per_species} listed rows, the
#'       remaining slots are filled with non-listed rows using the same mode.
#'       Not supported with \code{confidence_selection_mode = "stratified"}
#'       (errors), because stratified sampling returns one row per confidence
#'       bin and can collapse a listed set to fewer than \code{n_per_species}
#'       rows, making the cap/fill behaviour ambiguous.}
#'   }
#'
#' @param grouping_by Character vector specifying columns used for grouping.
#'   Defaults to \code{c("species_id", "deployment_id")}.
#'
#' @param pool A database connection pool (e.g. created with
#'   \code{pool::dbPool()}).
#'
#' @return A tibble containing the selected detection results.
#'
#' @details
#' The function performs database-side grouping when possible and only
#' collects data into memory after sampling (except for stratified mode,
#' where sampling is performed locally).
#'
#' Stratified sampling divides confidence values into
#' \code{n_per_species} bins and randomly selects one observation per bin.
#'
#' @examples
#' \dontrun{
#' prepare_spectrogram_input_data(
#'   confidence_selection_mode = "top",
#'   n_per_species = 30,
#'   deployment_ids = c(1, 2, 3),
#'   pool = pool
#' )
#' }
#'
#' @export
sample_results_table <- function(confidence_selection_mode = "top",
                                             n_per_species = 30,
                                             deployment_ids,
                                             species_ids = NULL,
                                             audio_file_ids = NULL,
                                             settings_ids = NULL,
                                             result_ids = NULL,
                                             result_id_mode = c("exclusive", "prioritize"),
                                             grouping_by = c("species_id", "deployment_id"),
                                             pool){


  # Validate arguments (and their combinations) before touching the DB.
  confidence_selection_mode <- match.arg(confidence_selection_mode,
                                         c("top", "random", "stratified"))
  result_id_mode <- match.arg(result_id_mode)

  # Normalise optional id filters: drop NAs, treat empty as "no constraint".
  species_ids    <- .normalise_id_filter(species_ids)
  audio_file_ids <- .normalise_id_filter(audio_file_ids)
  settings_ids   <- .normalise_id_filter(settings_ids)
  result_ids     <- .normalise_id_filter(result_ids)

  # In "prioritize" mode we keep non-listed rows as fallback but rank the listed
  # result_ids first (per group), so an extra `is_priority` split drives the
  # selection. In "exclusive" mode the list is a hard filter and no fallback is
  # kept. With no result_ids the argument is a no-op.
  use_priority <- !is.null(result_ids) && result_id_mode == "prioritize"

  # Stratified sampling keeps one row per confidence bin, so a group can yield
  # fewer than n_per_species rows even when many exist. That makes the
  # priority-first "cap vs. fill" logic ill-defined (a listed set may collapse
  # to <n strata and get silently topped up from non-listed rows), so the
  # combination is disallowed.
  if (use_priority && confidence_selection_mode == "stratified") {
    stop("`result_id_mode = \"prioritize\"` is not supported with ",
         "`confidence_selection_mode = \"stratified\"`. Use \"top\" or ",
         "\"random\" for prioritisation, or \"exclusive\" mode for a hard ",
         "result_id filter.")
  }

  available_names <- dplyr::tbl(pool, DBI::Id("import", "results")) |>
    colnames() |>
    c("deployment_id")

  match.arg(grouping_by, available_names, several.ok = TRUE)

  audio_files_tbl <- dplyr::tbl(pool, DBI::Id("import", "audio_files")) |>
    dplyr::filter(.data$deployment_id %in% !!deployment_ids)

  if (!is.null(audio_file_ids)) {
    audio_files_tbl <- audio_files_tbl |>
      dplyr::filter(.data$audio_file_id %in% !!audio_file_ids)
  }

  audio_files_tbl <- audio_files_tbl |>
    dplyr::select(audio_file_id, deployment_id)

  results_query_based <- dplyr::tbl(pool, DBI::Id("import", "results")) |>
    dplyr::select(-created_at)

  if (!is.null(settings_ids)) {
    results_query_based <- results_query_based |>
      dplyr::filter(.data$settings_id %in% !!settings_ids)
  }

  results_query_based <- results_query_based |>
    dplyr::select(-settings_id) |>
    dplyr::inner_join(audio_files_tbl, by = "audio_file_id")

  if (!is.null(species_ids)) {
    results_query_based <- results_query_based |>
      dplyr::filter(.data$species_id %in% !!species_ids)
  }

  # result_id restriction. "exclusive": hard filter to the listed rows.
  # "prioritize": keep every candidate but tag listed rows so they rank first.
  if (!is.null(result_ids) && !use_priority) {
    results_query_based <- results_query_based |>
      dplyr::filter(.data$result_id %in% !!result_ids)
  }
  if (use_priority) {
    results_query_based <- results_query_based |>
      dplyr::mutate(is_priority = .data$result_id %in% !!result_ids)
  }

  # When prioritising, sampling happens within each group x priority split so
  # that at most n_per_species listed and n_per_species fallback rows survive to
  # be combined (priority-first) below.
  selection_grouping <- if (use_priority) c(grouping_by, "is_priority") else grouping_by

  results_query_based <- results_query_based |>
    dplyr::group_by(dplyr::across(dplyr::all_of(selection_grouping)))

  if(confidence_selection_mode == "top"){
    result_selection <- results_query_based |>
      dplyr::slice_max(confidence, n = n_per_species) |>
      dplyr::collect()
  }

  if(confidence_selection_mode == "stratified"){

    results_query_based_strat <- results_query_based |>
      dplyr::mutate(
        min_conf = min(confidence),
        max_conf = max(confidence),
        bin_width = ifelse(min_conf == max_conf, 10000, (max_conf - min_conf) / n_per_species),
        conf_class = floor((confidence - min_conf) / bin_width)
      ) |>
      dplyr::mutate(
        conf_class = pmin(conf_class, n_per_species - 1)
      )

    result_selection <- results_query_based_strat |>
      dplyr::group_by(conf_class, .add = TRUE) |>
      dplyr::slice_sample(n = 1) |>
      dplyr::ungroup() |>
      dplyr::select(-min_conf, -max_conf, -bin_width, -conf_class) |>
      dplyr::collect()
  }

  if(confidence_selection_mode == "random"){
    result_selection <-  results_query_based |>
      dplyr::slice_sample(n = n_per_species) |>
      dplyr::collect()
  }

  # Prioritize mode: per group, take the listed (is_priority) rows first, then
  # top up with fallback rows, capping the whole group at n_per_species. The
  # split-selection above already limited each side to <= n_per_species, so a
  # priority-first ordering plus slice_head(n_per_species) yields:
  #   - >= n_per_species listed  -> n_per_species listed, no fallback (cap);
  #   - <  n_per_species listed  -> all listed + fallback filling the remainder.
  # For "top" the fallback drop must keep the highest-confidence rows, so
  # confidence is the secondary key; "random"/"stratified" keep the sampled
  # order (already mode-appropriate).
  if (use_priority) {
    result_selection <- result_selection |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grouping_by)))
    result_selection <- if (confidence_selection_mode == "top") {
      dplyr::arrange(result_selection, dplyr::desc(.data$is_priority),
                     dplyr::desc(.data$confidence), .by_group = TRUE)
    } else {
      dplyr::arrange(result_selection, dplyr::desc(.data$is_priority),
                     .by_group = TRUE)
    }
    result_selection <- result_selection |>
      dplyr::slice_head(n = n_per_species) |>
      dplyr::ungroup() |>
      dplyr::select(-"is_priority")
  }

  result_selection
}


#' Normalise an optional id filter vector
#'
#' Coerces an optional id vector to a clean integer filter. Returns \code{NULL}
#' when the input is \code{NULL} or contains no usable (non-NA) values, so
#' callers can treat \code{NULL} uniformly as "apply no constraint".
#'
#' @param x A vector of ids, or \code{NULL}.
#' @return A de-duplicated integer vector, or \code{NULL}.
#' @noRd
.normalise_id_filter <- function(x) {
  if (is.null(x)) return(NULL)
  x <- suppressWarnings(as.integer(x))
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NULL)
  unique(x)
}

