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
                                tflite_num_threads = NULL) {

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
#' File existence, readability, and size are checked with an optional retry
#' loop (controlled by `retry_n` and `retry_wait_s`) to handle files on
#' network-mounted or slow storage that may appear transiently unavailable.
#' Inference itself is similarly retried on transient I/O-class errors.
#'
#' @param audio_file Character. Path to an audio file.
#' @param birdnet_setup List. Object returned by [setup_birdnet_model()].
#' @param batch_size Integer. Batch size for inference.
#' @param use_arrow Logical. Whether to use Apache Arrow for IO.
#' @param birdnet_params List. Optional list of BirdNET inference parameters.
#' @param retry_n Integer. Maximum number of additional attempts after the
#'   first failure for transient file-access and inference errors (default 3).
#'   Set to 0 to disable retries entirely.
#' @param retry_wait_s Numeric. Seconds to wait between retry attempts
#'   (default 5). Waits are doubled on each successive attempt (exponential
#'   back-off) up to a ceiling of 60 seconds.
#'
#' @return A list containing the prediction results and parameters used, or a
#'   list with `prediction_raw = NULL` and an `error` field describing the
#'   failure if the file could not be processed.
#' @export
#'
#' @examples
#' \dontrun{
#' bnm <- setup_birdnet_model(version = "v2.4")
#'
#' # With default retries (3 attempts, 5 s initial wait)
#' res <- apply_birdnet_model("example.wav", bnm)
#'
#' # No retries – fail fast
#' res <- apply_birdnet_model("example.wav", bnm, retry_n = 0)
#'
#' head(res$prediction_raw)
#' }
apply_birdnet_model <- function(audio_file = list.files(system.file("extdata", package = "birdnetR"), pattern = ".mp3", full.names = TRUE),
                                birdnet_setup,
                                batch_size = 1L,
                                use_arrow = FALSE,
                                birdnet_params = NULL,
                                retry_n = 3L,
                                retry_wait_s = 5) {

  # ---------------------------------------------------------------------------
  # Input validation
  # ---------------------------------------------------------------------------

  if (!is.character(audio_file) || length(audio_file) != 1L) {
    stop("`audio_file` must be a single character string.")
  }

  if (!is.list(birdnet_setup) || is.null(birdnet_setup$model)) {
    stop("`birdnet_setup` must be a valid object returned by `setup_birdnet_model()`.")
  }

  if (!is.numeric(retry_n) || length(retry_n) != 1L || retry_n < 0) {
    stop("`retry_n` must be a non-negative integer scalar.")
  }
  retry_n <- as.integer(retry_n)

  if (!is.numeric(retry_wait_s) || length(retry_wait_s) != 1L || retry_wait_s < 0) {
    stop("`retry_wait_s` must be a non-negative numeric scalar.")
  }

  # ---------------------------------------------------------------------------
  # Parameter defaults
  # ---------------------------------------------------------------------------

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

  # ---------------------------------------------------------------------------
  # File accessibility checks (with retry for flaky network paths)
  # ---------------------------------------------------------------------------

  file_check <- .retry_file_check(audio_file, retry_n = retry_n, retry_wait_s = retry_wait_s)

  if (!is.null(file_check$error_type)) {
    warning(sprintf("File check failed for '%s' [%s]: %s",
                    audio_file, file_check$error_type, file_check$message),
            call. = FALSE)
    return(.birdnet_error_result(birdnet_setup, audio_file,
                                 file_check$error_type,
                                 file_check$message,
                                 params = params))
  }

  # ---------------------------------------------------------------------------
  # Inference (with retry for transient I/O errors)
  # ---------------------------------------------------------------------------

  prediction_raw <- .retry_inference(
    audio_file     = audio_file,
    birdnet_setup  = birdnet_setup,
    params         = params,
    batch_size     = batch_size,
    use_arrow      = use_arrow,
    retry_n        = retry_n,
    retry_wait_s   = retry_wait_s
  )

  if (inherits(prediction_raw, "birdnet_inference_error")) {
    return(.birdnet_error_result(birdnet_setup, audio_file,
                                 prediction_raw$error_type,
                                 prediction_raw$message,
                                 params = params))
  }

  # ---------------------------------------------------------------------------
  # Assemble result
  # ---------------------------------------------------------------------------

  birdnet_setup$prediction_raw <- prediction_raw
  birdnet_setup$prediction_time <- Sys.time()
  birdnet_setup$params         <- params
  birdnet_setup$audio_file     <- audio_file
  birdnet_setup$error          <- NULL
  birdnet_setup$species        <- NULL
  birdnet_setup$model          <- NULL

  birdnet_setup
}


# ==============================================================================
# Internal helpers
# ==============================================================================

#' Retry helper: exponential back-off sleep
#'
#' Sleeps for `wait * 2^(attempt - 1)` seconds, capped at 15 s, and emits a
#' message so the caller can see what is happening.
#'
#' @param attempt Integer. Current attempt number (1-based).
#' @param wait Numeric. Base wait in seconds.
#' @param reason Character. Short description used in the progress message.
#' @noRd
.backoff_sleep <- function(attempt, wait, reason) {
  delay <- min(wait * 2^(attempt - 1), 15)
  message(sprintf("  Retrying in %.0f s (attempt %d, reason: %s)...", delay, attempt, reason))
  Sys.sleep(delay)
}


#' Retry helper: file existence / readability / size checks
#'
#' Returns a list with `error_type` and `message` (both NULL on success).
#'
#' Only `file_not_found` and `empty_file` are considered transient and will be
#' retried; `permission_denied` is structural and fails immediately.
#'
#' @noRd
.retry_file_check <- function(audio_file, retry_n, retry_wait_s) {

  for (attempt in seq_len(retry_n + 1L)) {

    # 1. Existence
    if (!file.exists(audio_file)) {
      if (attempt <= retry_n) {
        message(sprintf("File not found (attempt %d/%d): %s",
                        attempt, retry_n + 1L, audio_file))
        .backoff_sleep(attempt, retry_wait_s, "file_not_found")
        next
      }
      return(list(
        error_type = "file_not_found",
        message    = sprintf("File does not exist after %d attempt(s): %s",
                             retry_n + 1L, audio_file)
      ))
    }

    # 2. Read permission (structural – no retry)
    if (file.access(audio_file, mode = 4L) != 0L) {
      return(list(
        error_type = "permission_denied",
        message    = sprintf("Cannot read file (check permissions): %s", audio_file)
      ))
    }

    # 3. Non-zero size (a freshly-written network file may briefly be 0 bytes)
    file_size <- file.info(audio_file)$size
    if (is.na(file_size) || file_size == 0L) {
      if (attempt <= retry_n) {
        message(sprintf("File is empty/unreadable (attempt %d/%d): %s",
                        attempt, retry_n + 1L, audio_file))
        .backoff_sleep(attempt, retry_wait_s, "empty_file")
        next
      }
      return(list(
        error_type = "empty_file",
        message    = sprintf("File is empty after %d attempt(s): %s",
                             retry_n + 1L, audio_file)
      ))
    }

    # All checks passed
    return(list(error_type = NULL, message = NULL))
  }
}


#' Retry helper: BirdNET inference
#'
#' Wraps `birdnetR::predict_species_from_audio_file()` and retries on errors
#' that look transient (I/O, network). Corruption errors are not retried.
#'
#' Returns either the raw prediction data frame or an object of class
#' `"birdnet_inference_error"`.
#'
#' @noRd
.retry_inference <- function(audio_file, birdnet_setup, params,
                             batch_size, use_arrow, retry_n, retry_wait_s) {

  # Patterns that suggest the file itself is corrupt (no point retrying)
  corruption_patterns <- c(
    "invalid", "corrupt", "decode", "header", "format",
    "unsupported", "truncat", "EOF", "codec", "audio"
  )

  # Patterns that suggest a transient I/O / network problem (worth retrying)
  transient_patterns <- c(
    "connection", "timeout", "network", "refused", "reset",
    "unavailable", "temporarily", "interrupted", "stale", "NFS",
    "input/output", "i/o error", "host"
  )

  last_error <- NULL

  for (attempt in seq_len(retry_n + 1L)) {

    result <- tryCatch(
      withCallingHandlers(
        birdnetR::predict_species_from_audio_file(
          model               = birdnet_setup$model,
          filter_species      = birdnet_setup$species$label,
          audio_file          = audio_file,
          min_confidence      = params$min_confidence,
          chunk_overlap_s     = params$chunk_overlap_s,
          use_bandpass        = params$use_bandpass,
          bandpass_fmin       = params$bandpass_fmin,
          bandpass_fmax       = params$bandpass_fmax,
          apply_sigmoid       = params$apply_sigmoid,
          sigmoid_sensitivity = params$sigmoid_sensitivity,
          keep_empty          = params$keep_empty,
          batch_size          = batch_size,
          use_arrow           = use_arrow
        ),
        warning = function(w) {
          warning(sprintf("BirdNET warning for '%s': %s", audio_file, conditionMessage(w)),
                  call. = FALSE)
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        msg <- conditionMessage(e)

        is_corrupt    <- any(grepl(paste(corruption_patterns, collapse = "|"), msg, ignore.case = TRUE))
        is_transient  <- any(grepl(paste(transient_patterns,  collapse = "|"), msg, ignore.case = TRUE))

        error_type <- dplyr::case_when(
          is_corrupt   ~ "corrupt_file",
          is_transient ~ "transient_io_error",
          TRUE         ~ "inference_error"
        )
        # Plain R equivalent (no dplyr dependency):
        # error_type <- if (is_corrupt) "corrupt_file" else if (is_transient) "transient_io_error" else "inference_error"

        structure(
          list(error_type = error_type, message = msg),
          class = "birdnet_inference_error"
        )
      }
    )

    # Success
    if (!inherits(result, "birdnet_inference_error")) {
      return(result)
    }

    last_error <- result

    # Corruption errors are deterministic – retrying won't help
    if (result$error_type == "corrupt_file") {
      warning(sprintf("BirdNET inference failed for '%s' [corrupt_file]: %s",
                      audio_file, result$message),
              call. = FALSE)
      return(result)
    }

    # For transient or unknown errors, retry if attempts remain
    if (attempt <= retry_n) {
      message(sprintf("BirdNET inference error (attempt %d/%d) for '%s' [%s]: %s",
                      attempt, retry_n + 1L, audio_file,
                      result$error_type, result$message))
      .backoff_sleep(attempt, retry_wait_s, result$error_type)
    } else {
      warning(sprintf("BirdNET inference failed for '%s' [%s] after %d attempt(s): %s",
                      audio_file, result$error_type, retry_n + 1L, result$message),
              call. = FALSE)
    }
  }

  last_error
}


#' Build a consistent error-result list
#' @noRd
.birdnet_error_result <- function(birdnet_setup, audio_file, error_type,
                                  error_message, params = NULL) {
  birdnet_setup$prediction_raw <- NULL
  birdnet_setup$prediction_time <- Sys.time()
  birdnet_setup$audio_file     <- audio_file
  birdnet_setup$error          <- list(type = error_type, message = error_message)
  birdnet_setup$params         <- params
  birdnet_setup$species        <- NULL
  birdnet_setup$model          <- NULL
  birdnet_setup
}
