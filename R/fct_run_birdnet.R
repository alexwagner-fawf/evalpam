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
      min_confidence = min_confidence)
  })

  model <- birdnetR::birdnet_model_tflite(version = version,
                                          language = "en_us",
                                          tflite_num_threads = tflite_num_threads)


  return(
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
  )
}

apply_birdnet_model <- function(audio_file = list.files(system.file("extdata", package = "birdnetR"), pattern = ".mp3", full.names = TRUE),
                                birdnet_setup,
                                batch_size = 1L,
                                use_arrow = FALSE,
                                birdnet_params = NULL
                                ){

  if(is.null(birdnet_params)){
    birdnet_params = list()
  }

  # ---- defaults for BirdNET parameters ----
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

  # ---- identify which defaults will be used ----
  missing_params <- setdiff(names(defaults), names(birdnet_params))



  # ---- merge user parameters over defaults ----
  params <- modifyList(defaults, birdnet_params)

  # ---- print message about defaults ----
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
      paste(
        sprintf("  - %s = %s", missing_params, default_values),
        collapse = "\n"
      )
    )
  }

  # ---- run BirdNET ----
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

  return(birdnet_setup)
  }





