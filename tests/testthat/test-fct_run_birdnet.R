# test-fct_run_birdnet.R
#
# Tests for apply_birdnet_model() and its internal helpers:
#   .retry_file_check(), .retry_inference(), .backoff_sleep(),
#   .birdnet_error_result()
#
# HOW STUBBING WORKS HERE
# -----------------------
# mockery::stub(where, what, how)
#   where = the function whose *body* we are patching
#   what  = the name of the call to intercept, as seen inside `where`
#   how   = replacement value / function
#
# KEY RULE: the stub is assigned to the *test frame*, not the package namespace.
# That means:
#   - stub(apply_birdnet_model, ".retry_inference", fn)  → works because
#     apply_birdnet_model is the function called directly in the test
#   - stub(.retry_inference, ".predict_birdnet", fn) → works when
#     .retry_inference is called directly in the test; does NOT work when
#     apply_birdnet_model calls .retry_inference through the namespace
#
# Design consequence:
#   • Tests about *apply_birdnet_model integration*: stub .retry_inference /
#     .retry_file_check directly FROM apply_birdnet_model.
#   • Tests about *.retry_inference retry logic*: call .retry_inference()
#     directly so the stub on .predict_birdnet is visible.
#   • Tests about *.retry_file_check retry logic*: call .retry_file_check()
#     directly so stubs on file.exists / file.info / file.access are visible.
# ---------------------------------------------------------------------------

library(testthat)
library(mockery)

# ===========================================================================
# Shared fixtures
# ===========================================================================

make_setup <- function(model = list(dummy = TRUE)) {
  list(
    model_version        = "v2.4",
    model_name           = "birdnet",
    species              = data.frame(
      label = c("Turdus merula_Blackbird", "Parus major_Great Tit"),
      stringsAsFactors = FALSE
    ),
    model                = model,
    latitude             = 52.5,
    longitude            = 13.4,
    min_ebird_confidence = 0.5,
    week                 = 20L
  )
}

make_prediction <- function() {
  data.frame(
    start_s = c(0, 3),
    end_s   = c(3, 6),
    label   = c("Turdus merula_Blackbird", "Parus major_Great Tit"),
    score   = c(0.91, 0.76),
    stringsAsFactors = FALSE
  )
}

# Default inference params (mirrors apply_birdnet_model's built-in defaults)
make_params <- function() {
  list(
    min_confidence      = 0.2,
    chunk_overlap_s     = 0,
    use_bandpass        = TRUE,
    bandpass_fmin       = 150L,
    bandpass_fmax       = 15000L,
    apply_sigmoid       = TRUE,
    sigmoid_sensitivity = 1,
    keep_empty          = FALSE
  )
}

# Helper: create a real non-empty temp wav file
make_tmp_wav <- function() {
  tmp <- tempfile(fileext = ".wav")
  writeBin(as.raw(rep(0x01, 100)), tmp)
  tmp
}

# Helper: suppress the expected warning so test output stays clean
mute_warnings <- function(expr) {
  withCallingHandlers(expr, warning = function(w) invokeRestart("muffleWarning"))
}


# ===========================================================================
# 1. Input validation
# ===========================================================================

test_that("stops when audio_file is not a single string", {
  setup <- make_setup()
  expect_error(apply_birdnet_model(123L,         setup, retry_n = 0), "`audio_file`")
  expect_error(apply_birdnet_model(c("a", "b"),  setup, retry_n = 0), "`audio_file`")
  expect_error(apply_birdnet_model(character(0), setup, retry_n = 0), "`audio_file`")
})

test_that("stops when birdnet_setup is not a valid list", {
  expect_error(apply_birdnet_model("x.wav", list(),       retry_n = 0), "`birdnet_setup`")
  expect_error(apply_birdnet_model("x.wav", "not_a_list", retry_n = 0), "`birdnet_setup`")
  expect_error(apply_birdnet_model("x.wav", make_setup(model = NULL), retry_n = 0), "`birdnet_setup`")
})

test_that("stops when retry_n is invalid", {
  setup <- make_setup()
  expect_error(apply_birdnet_model("x.wav", setup, retry_n = -1),     "`retry_n`")
  expect_error(apply_birdnet_model("x.wav", setup, retry_n = "3"),    "`retry_n`")
  expect_error(apply_birdnet_model("x.wav", setup, retry_n = c(1,2)), "`retry_n`")
})

test_that("stops when retry_wait_s is invalid", {
  setup <- make_setup()
  expect_error(apply_birdnet_model("x.wav", setup, retry_wait_s = -1),     "`retry_wait_s`")
  expect_error(apply_birdnet_model("x.wav", setup, retry_wait_s = "5"),    "`retry_wait_s`")
  expect_error(apply_birdnet_model("x.wav", setup, retry_wait_s = c(1,2)), "`retry_wait_s`")
})


# ===========================================================================
# 2. Successful inference  (stub .retry_inference from apply_birdnet_model)
# ===========================================================================

test_that("returns correct structure on success", {
  setup <- make_setup()
  pred  <- make_prediction()
  tmp   <- make_tmp_wav()
  on.exit(unlink(tmp), add = TRUE)

  stub(apply_birdnet_model, ".retry_inference", function(...) pred)

  res <- suppressMessages(
    apply_birdnet_model(audio_file = tmp, birdnet_setup = setup, retry_n = 0, retry_wait_s = 0)
  )

  expect_null(res$error)
  expect_identical(res$prediction_raw, pred)
  expect_equal(res$audio_file, tmp)
  expect_null(res$model)
  expect_null(res$species)
  expect_false(is.null(res$params))
})

test_that("custom birdnet_params override defaults and are stored", {
  setup <- make_setup()
  pred  <- make_prediction()
  tmp   <- make_tmp_wav()
  on.exit(unlink(tmp), add = TRUE)

  stub(apply_birdnet_model, ".retry_inference", function(...) pred)

  res <- suppressMessages(
    apply_birdnet_model(tmp, setup,
                        birdnet_params = list(min_confidence = 0.5,
                                              use_bandpass   = FALSE),
                        retry_n = 0, retry_wait_s = 0)
  )

  expect_equal(res$params$min_confidence, 0.5)
  expect_false(res$params$use_bandpass)
  expect_equal(res$params$bandpass_fmin, 150L)   # untouched default
})

test_that("default params message is emitted for missing keys", {
  setup <- make_setup()
  pred  <- make_prediction()
  tmp   <- make_tmp_wav()
  on.exit(unlink(tmp), add = TRUE)

  stub(apply_birdnet_model, ".retry_inference", function(...) pred)

  expect_message(
    apply_birdnet_model(tmp, setup, retry_n = 0, retry_wait_s = 0),
    "BirdNET defaults applied"
  )
})


# ===========================================================================
# 3. File-not-found (no retry)
# ===========================================================================

test_that("returns file_not_found error immediately when retry_n = 0", {
  setup <- make_setup()

  res <- mute_warnings(
    apply_birdnet_model("/nonexistent/bird.wav", setup,
                        retry_n = 0, retry_wait_s = 0)
  )

  expect_null(res$prediction_raw)
  expect_equal(res$error$type, "file_not_found")
  expect_match(res$error$message, "does not exist")
})

test_that("emits a warning on file_not_found", {
  setup <- make_setup()
  expect_warning(
    apply_birdnet_model("/nonexistent/bird.wav", setup,
                        retry_n = 0, retry_wait_s = 0),
    "file_not_found"
  )
})


# ===========================================================================
# 4. File-not-found with retries (.retry_file_check called directly)
# ===========================================================================

test_that("retries on file_not_found and succeeds when file appears later", {
  tmp         <- tempfile(fileext = ".wav")
  check_count <- 0L
  on.exit(unlink(tmp), add = TRUE)

  stub(.retry_file_check, "file.exists", function(path) {
    check_count <<- check_count + 1L
    if (check_count < 3L) return(FALSE)
    writeBin(as.raw(rep(0x01, 100)), path)   # "file appears on network"
    TRUE
  })
  stub(.retry_file_check, "Sys.sleep", function(...) invisible(NULL))

  result <- .retry_file_check(tmp, retry_n = 3L, retry_wait_s = 0)

  expect_null(result$error_type)
  expect_gte(check_count, 3L)
})

test_that("exhausts retries and returns file_not_found after retry_n attempts", {
  setup <- make_setup()

  stub(.retry_file_check, "file.exists", function(...) FALSE)
  stub(.retry_file_check, "Sys.sleep",   function(...) invisible(NULL))

  res <- mute_warnings(
    apply_birdnet_model("/ghost/file.wav", setup,
                        retry_n = 2L, retry_wait_s = 0)
  )

  expect_null(res$prediction_raw)
  expect_equal(res$error$type, "file_not_found")
  expect_match(res$error$message, "3 attempt")   # 1 initial + 2 retries
})

test_that("emits progress messages during file-not-found retries", {
  setup <- make_setup()

  stub(.retry_file_check, "file.exists", function(...) FALSE)
  stub(.retry_file_check, "Sys.sleep",   function(...) invisible(NULL))

  expect_message(
    mute_warnings(
      apply_birdnet_model("/ghost/file.wav", setup,
                          retry_n = 1L, retry_wait_s = 0)
    ),
    "attempt"
  )
})


# ===========================================================================
# 5. Empty file (with and without retries)
# ===========================================================================

test_that("returns empty_file error without retry when retry_n = 0", {
  setup <- make_setup()
  tmp   <- tempfile(fileext = ".wav")
  file.create(tmp)   # zero bytes
  on.exit(unlink(tmp), add = TRUE)

  res <- mute_warnings(
    apply_birdnet_model(tmp, setup, retry_n = 0, retry_wait_s = 0)
  )

  expect_null(res$prediction_raw)
  expect_equal(res$error$type, "empty_file")
})

test_that("retries on empty_file and succeeds when content is written later", {
  tmp        <- tempfile(fileext = ".wav")
  info_count <- 0L
  file.create(tmp)   # start empty
  on.exit(unlink(tmp), add = TRUE)

  # Call .retry_file_check directly so the file.info stub is visible.
  stub(.retry_file_check, "file.info", function(path, ...) {
    info_count <<- info_count + 1L
    if (info_count >= 2L) writeBin(as.raw(rep(0x01, 100)), path)
    base::file.info(path, ...)
  })
  stub(.retry_file_check, "Sys.sleep", function(...) invisible(NULL))

  result <- .retry_file_check(tmp, retry_n = 3L, retry_wait_s = 0)

  expect_null(result$error_type)
})


# ===========================================================================
# 6. Permission denied (.retry_file_check called directly)
# ===========================================================================

test_that("returns permission_denied immediately without retrying", {
  tmp         <- make_tmp_wav()
  sleep_calls <- 0L
  on.exit(unlink(tmp), add = TRUE)

  # Call .retry_file_check directly so the file.access stub is visible.
  stub(.retry_file_check, "file.access", function(names, mode = 0L) -1L)
  stub(.retry_file_check, "Sys.sleep",   function(...) {
    sleep_calls <<- sleep_calls + 1L
  })

  result <- .retry_file_check(tmp, retry_n = 3L, retry_wait_s = 0)

  expect_equal(result$error_type, "permission_denied")
  expect_equal(sleep_calls, 0L)   # structural – must never retry
})


# ===========================================================================
# 7. Corrupt file (.retry_inference called directly)
# ===========================================================================

test_that("returns corrupt_file error without retrying", {
  setup       <- make_setup()
  tmp         <- make_tmp_wav()
  sleep_calls <- 0L
  on.exit(unlink(tmp), add = TRUE)

  stub(.retry_inference, ".predict_birdnet",
       function(...) stop("invalid audio header: corrupt data"))
  stub(.retry_inference, "Sys.sleep", function(...) {
    sleep_calls <<- sleep_calls + 1L
  })

  result <- suppressWarnings(
    .retry_inference(tmp, setup, make_params(), 1L, FALSE,
                     retry_n = 3L, retry_wait_s = 0)
  )

  expect_s3_class(result, "birdnet_inference_error")
  expect_equal(result$error_type, "corrupt_file")
  expect_equal(sleep_calls, 0L)   # must not retry corrupt files
})

test_that("corrupt_file error message is preserved in result", {
  setup <- make_setup()
  tmp   <- make_tmp_wav()
  on.exit(unlink(tmp), add = TRUE)

  stub(.retry_inference, ".predict_birdnet",
       function(...) stop("codec: unsupported format XYZ"))

  result <- suppressWarnings(
    .retry_inference(tmp, setup, make_params(), 1L, FALSE,
                     retry_n = 0L, retry_wait_s = 0)
  )

  expect_match(result$message, "unsupported format XYZ")
})


# ===========================================================================
# 8. Transient inference / network error (.retry_inference called directly)
# ===========================================================================

test_that("retries on transient_io_error and eventually succeeds", {
  setup      <- make_setup()
  pred       <- make_prediction()
  tmp        <- make_tmp_wav()
  call_count <- 0L
  on.exit(unlink(tmp), add = TRUE)

  stub(.retry_inference, ".predict_birdnet", function(...) {
    call_count <<- call_count + 1L
    if (call_count < 3L) stop("network connection reset by peer")
    pred
  })
  stub(.retry_inference, "Sys.sleep", function(...) invisible(NULL))

  result <- suppressMessages(
    .retry_inference(tmp, setup, make_params(), 1L, FALSE,
                     retry_n = 3L, retry_wait_s = 0)
  )

  expect_false(inherits(result, "birdnet_inference_error"))
  expect_identical(result, pred)
  expect_equal(call_count, 3L)
})

test_that("exhausts retries and returns transient_io_error", {
  setup <- make_setup()
  tmp   <- make_tmp_wav()
  on.exit(unlink(tmp), add = TRUE)

  stub(.retry_inference, ".predict_birdnet",
       function(...) stop("NFS stale file handle"))
  stub(.retry_inference, "Sys.sleep", function(...) invisible(NULL))

  result <- suppressMessages(suppressWarnings(
    .retry_inference(tmp, setup, make_params(), 1L, FALSE,
                     retry_n = 2L, retry_wait_s = 0)
  ))

  expect_s3_class(result, "birdnet_inference_error")
  expect_equal(result$error_type, "transient_io_error")
  expect_match(result$message, "stale file handle")
})

test_that("correct number of inference calls made during retries", {
  setup      <- make_setup()
  tmp        <- make_tmp_wav()
  call_count <- 0L
  on.exit(unlink(tmp), add = TRUE)

  stub(.retry_inference, ".predict_birdnet", function(...) {
    call_count <<- call_count + 1L
    stop("connection timeout")
  })
  stub(.retry_inference, "Sys.sleep", function(...) invisible(NULL))

  suppressMessages(suppressWarnings(
    .retry_inference(tmp, setup, make_params(), 1L, FALSE,
                     retry_n = 3L, retry_wait_s = 0)
  ))

  expect_equal(call_count, 4L)   # 1 initial + 3 retries
})


# ===========================================================================
# 9. Generic (unclassified) inference error (.retry_inference called directly)
# ===========================================================================

test_that("returns inference_error for unknown error messages", {
  setup <- make_setup()
  tmp   <- make_tmp_wav()
  on.exit(unlink(tmp), add = TRUE)

  stub(.retry_inference, ".predict_birdnet",
       function(...) stop("something entirely unexpected happened"))
  stub(.retry_inference, "Sys.sleep", function(...) invisible(NULL))

  result <- suppressMessages(suppressWarnings(
    .retry_inference(tmp, setup, make_params(), 1L, FALSE,
                     retry_n = 0L, retry_wait_s = 0)
  ))

  expect_s3_class(result, "birdnet_inference_error")
  expect_equal(result$error_type, "inference_error")
})

test_that("unknown inference errors ARE retried (up to retry_n times)", {
  setup      <- make_setup()
  tmp        <- make_tmp_wav()
  call_count <- 0L
  on.exit(unlink(tmp), add = TRUE)

  stub(.retry_inference, ".predict_birdnet", function(...) {
    call_count <<- call_count + 1L
    stop("something entirely unexpected happened")
  })
  stub(.retry_inference, "Sys.sleep", function(...) invisible(NULL))

  suppressMessages(suppressWarnings(
    .retry_inference(tmp, setup, make_params(), 1L, FALSE,
                     retry_n = 2L, retry_wait_s = 0)
  ))

  expect_equal(call_count, 3L)   # 1 initial + 2 retries
})


# ===========================================================================
# 10. Inference warning propagation (.retry_inference called directly)
# ===========================================================================

test_that("inference warnings are re-emitted with file context", {
  setup <- make_setup()
  pred  <- make_prediction()
  tmp   <- make_tmp_wav()
  on.exit(unlink(tmp), add = TRUE)

  stub(.retry_inference, ".predict_birdnet", function(...) {
    warning("low signal-to-noise ratio detected")
    pred
  })

  expect_warning(
    .retry_inference(tmp, setup, make_params(), 1L, FALSE,
                     retry_n = 0L, retry_wait_s = 0),
    "low signal-to-noise"
  )
})


# ===========================================================================
# 11. Back-off timing (.backoff_sleep directly)
# ===========================================================================

test_that(".backoff_sleep caps delay at 15 seconds", {
  captured <- numeric(0)
  stub(.backoff_sleep, "Sys.sleep", function(d) captured <<- c(captured, d))

  .backoff_sleep(attempt = 7L, wait = 5, reason = "test")

  expect_equal(captured, 15)   # 5 * 2^6 = 320 → capped at 15
})

test_that(".backoff_sleep doubles delay on successive attempts", {
  captured <- numeric(0)
  stub(.backoff_sleep, "Sys.sleep", function(d) captured <<- c(captured, d))

  for (i in 1:4) .backoff_sleep(attempt = i, wait = 1, reason = "test")

  expect_equal(captured, c(1, 2, 4, 8))
})

test_that(".backoff_sleep emits a message", {
  stub(.backoff_sleep, "Sys.sleep", function(...) invisible(NULL))
  expect_message(.backoff_sleep(1L, 2, "unit_test"), "Retrying")
})


# ===========================================================================
# 12. retry_n = 0 disables all retries (fast-fail mode)
# ===========================================================================

test_that("retry_n = 0 never calls Sys.sleep", {
  setup       <- make_setup()
  sleep_calls <- 0L

  stub(.retry_file_check, "file.exists", function(...) FALSE)
  stub(.retry_file_check, "Sys.sleep",   function(...) {
    sleep_calls <<- sleep_calls + 1L
  })

  mute_warnings(
    apply_birdnet_model("/nonexistent/file.wav", setup,
                        retry_n = 0L, retry_wait_s = 5)
  )

  expect_equal(sleep_calls, 0L)
})


# ===========================================================================
# 13. .birdnet_error_result structure
# ===========================================================================

test_that(".birdnet_error_result always includes required fields", {
  result <- .birdnet_error_result(make_setup(), "x.wav", "test_error", "test message")

  expect_null(result$prediction_raw)
  expect_null(result$model)
  expect_null(result$species)
  expect_equal(result$audio_file, "x.wav")
  expect_equal(result$error$type, "test_error")
  expect_equal(result$error$message, "test message")
  # Setup metadata preserved
  expect_equal(result$model_version, "v2.4")
  expect_equal(result$latitude, 52.5)
})

test_that(".birdnet_error_result params is NULL when not supplied", {
  result <- .birdnet_error_result(make_setup(), "x.wav", "e", "m")
  expect_null(result$params)
})

test_that(".birdnet_error_result params is populated when supplied", {
  result <- .birdnet_error_result(make_setup(), "x.wav", "e", "m",
                                  params = list(min_confidence = 0.3))
  expect_equal(result$params$min_confidence, 0.3)
})
