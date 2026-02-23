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
                                             grouping_by = c("species_id", "deployment_id"),
                                             pool){


  available_names <- dplyr::tbl(pool, DBI::Id("import", "results")) |>
    colnames() |>
    c("deployment_id")

  match.arg(confidence_selection_mode, c("top", "random", "stratified"))
  match.arg(grouping_by, available_names, several.ok = TRUE)

  audio_file_ids <- dplyr::tbl(pool, DBI::Id("import", "audio_files")) |>
    dplyr::filter(.data$deployment_id %in% !!deployment_ids) |>
    dplyr::select(audio_file_id, deployment_id)

  results_query_based <- dplyr::tbl(pool, DBI::Id("import", "results")) |>
    dplyr::select(-created_at, -settings_id) |>
    dplyr::inner_join(audio_file_ids, by = "audio_file_id") |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_by)))

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

  result_selection
}

