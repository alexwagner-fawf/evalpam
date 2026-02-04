#' Setup a BirdNET model for inference
#'
#' Initializes a BirdNET model and optionally restricts species predictions
#' based on geographic location and time of year.
#'
#' @param version Character. BirdNET model version (e.g. "v2.4").
#' @param latitude Numeric. Latitude for species filtering (optional).
#' @param longitude Numeric. Longitude for species filtering (optional).
#' @param week Integer. Week of year (1–52) for species filtering (optional).
#' @param min_confidence Numeric. Minimum eBird confidence for species filtering (optional).
#' @param tflite_num_threads Integer. Number of threads for the TensorFlow Lite model (optional).
#'
#' @return A list with the BirdNET model, metadata, and optional species filter.
#' @export
#'
#' @examples
#' \dontrun{
#' bnm <- setup_birdnet_model(
#'   version = "v2.4",
#'   latitude = 52.5,
#'   longitude = 13.4,
#'   week = 20,
#'   min_confidence = 0.5
#' )
#' }
setup_birdnet_model <- function(version = "v2.4",
                                latitude = NULL,
                                longitude = NULL,
                                week = NULL,
                                min_confidence = NULL,
                                tflite_num_threads = NULL){

  species <- NULL
  try({
    species <- birdnetR::predict_species_at_location_and_time(
      model = birdnetR::birdnet_model_meta(version),
      latitude = latitude,
      longitude = longitude,
      week = week,
      min_confidence = min_confidence
    )
  }, silent = TRUE)

  model <- birdnetR::birdnet_model_tflite(
    version = version,
    language = "en_us",
    tflite_num_threads = tflite_num_threads
  )

  list(
    model_version = version,
    model_name = "birdnet",
    species = species,
    model = model,
    latitude = latitude,
    longitude = longitude,
    min_ebird_confidence = min_confidence,
    week = week
  )
}



#' Apply BirdNET model to an audio file
#'
#' Runs BirdNET inference on a single audio file using a pre-configured
#' BirdNET setup object created by [setup_birdnet_model()].
#'
#' @param audio_file Character. Path to an audio file.
#' @param birdnet_setup List. Object returned by [setup_birdnet_model()].
#' @param batch_size Integer. Batch size for inference.
#' @param use_arrow Logical. Whether to use Apache Arrow for IO.
#' @param birdnet_params List. Optional list of BirdNET inference parameters.
#'
#' @return A list containing the prediction results and parameters used.
#' @export
#'
#' @examples
#' \dontrun{
#' bnm <- setup_birdnet_model(version = "v2.4")
#' res <- apply_birdnet_model("example.wav", bnm)
#' head(res$prediction_raw)
#' }
apply_birdnet_model <- function(audio_file = list.files(system.file("extdata", package = "birdnetR"), pattern = ".mp3", full.names = TRUE),
                                birdnet_setup,
                                batch_size = 1L,
                                use_arrow = FALSE,
                                birdnet_params = NULL){

  if (is.null(birdnet_params)) {
    birdnet_params <- list()
  }

  defaults <- list(
    min_confidence       = 0.2,
    chunk_overlap_s      = 0,
    use_bandpass         = TRUE,
    bandpass_fmin        = 150L,
    bandpass_fmax        = 15000L,
    apply_sigmoid        = TRUE,
    sigmoid_sensitivity  = 1,
    keep_empty           = FALSE
  )

  missing_params <- setdiff(names(defaults), names(birdnet_params))
  params <- modifyList(defaults, birdnet_params)

  if (length(missing_params) > 0) {
    default_values <- vapply(
      missing_params,
      function(nm) {
        val <- defaults[[nm]]
        if (is.null(val)) "NULL" else paste0(deparse(val), collapse = "")
      },
      character(1)
    )

    message(
      "BirdNET defaults applied:\n",
      paste(sprintf("  - %s = %s", missing_params, default_values), collapse = "\n")
    )
  }

  prediction_raw <- birdnetR::predict_species_from_audio_file(
    model                = birdnet_setup$model,
    filter_species       = birdnet_setup$species$label,
    audio_file           = audio_file,
    min_confidence       = params$min_confidence,
    chunk_overlap_s      = params$chunk_overlap_s,
    use_bandpass         = params$use_bandpass,
    bandpass_fmin        = params$bandpass_fmin,
    bandpass_fmax        = params$bandpass_fmax,
    apply_sigmoid        = params$apply_sigmoid,
    sigmoid_sensitivity  = params$sigmoid_sensitivity,
    keep_empty           = params$keep_empty,
    batch_size           = batch_size,
    use_arrow            = use_arrow
  )

  birdnet_setup$prediction_raw <- prediction_raw
  birdnet_setup$params <- params

  birdnet_setup$species <- NULL
  birdnet_setup$model <- NULL

  birdnet_setup
}




