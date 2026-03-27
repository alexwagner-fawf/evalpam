#' Generate Spectrogram Videos for Verification
#'
#' This function takes a set of BirdNET detections, extracts the audio segments
#' with a defined padding (context), generates a spectrogram video (.mp4),
#' and registers the metadata in the database.
#'
#' @param data A data frame containing at least `audio_file_id`, `begin_time_ms`,
#' `species_id`, and `confidence`.
#' @param pool A DBI connection pool for database access.
#' @param padding_s Numeric. Seconds of context before and after the detection. Default is 5.
#' @param analysis_range Numeric. The length of the BirdNET analysis window (usually 3s).
#' @param output_dir Character. Directory where the .mp4 files will be saved.
#' @param temp_dir Character. Directory for temporary audio chunks.
#' @param video_width Integer. Width of the output video.
#' @param video_height Integer. Height of the output video.
#' @param video_res Integer. Resolution (DPI) of the spectrogram.
#' @param verbose Logical. Whether to show a progress bar.
#'
#' @return A list with the processing status and a list of any errors encountered.
#'
#' @importFrom DBI dbGetQuery dbExecute
#' @importFrom pool poolWithTransaction
#' @importFrom av av_audio_convert av_spectrogram_video
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom magrittr %>%
#'
#' @export

build_spectrogram_db <- function(data, pool, padding_s = 5, analysis_range = 3,
                                 output_dir = "spectrograms", temp_dir = tempdir(),
                                 video_width = 1280, video_height = 720, video_res = 144, verbose = TRUE) {

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # FFmpeg Check (einmal am Anfang)
  ffmpeg_path <- Sys.which("ffmpeg")
  has_ffmpeg <- nchar(ffmpeg_path) > 0

  if (!has_ffmpeg) {
    warning(
      "\n",
      "===========================================================\n",
      " FFmpeg nicht gefunden! Spektrogramme werden OHNE\n",
      " Detektionslinien generiert.\n",
      "\n",
      " Installation:\n",
      " 1. https://github.com/BtbN/FFmpeg-Builds/releases\n",
      " 2. ffmpeg-master-latest-win64-gpl.zip herunterladen\n",
      " 3. Entpacken (z.B. C:/ffmpeg/bin/)\n",
      " 4. In R: Sys.setenv(PATH = paste0('C:/ffmpeg/bin;', Sys.getenv('PATH')))\n",
      "===========================================================",
      call. = FALSE, immediate. = TRUE
    )
  }

  # Plot-Ränder einmal berechnen (Offset für Achsen-Labels)
  temp_margin_png <- file.path(temp_dir, "margin_calc.png")
  grDevices::png(temp_margin_png, width = video_width, height = video_height, res = video_res)
  graphics::par(mar = c(5.1, 4.1, 4.1, 2.1))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
  plt <- graphics::par("plt")
  grDevices::dev.off()
  if(file.exists(temp_margin_png)) file.remove(temp_margin_png)

  plot_left_px  <- as.integer(plt[1] * video_width)
  plot_right_px <- as.integer(plt[2] * video_width)
  plot_width_px <- plot_right_px - plot_left_px

  if (verbose) pb <- utils::txtProgressBar(min = 0, max = nrow(data), style = 3)
  errors <- list()

  for (i in seq_len(nrow(data))) {
    row <- data[i, ]
    tryCatch({

      start_sec_original <- as.numeric(row$begin_time_ms) / 1000.0
      clip_start_sec <- max(0, start_sec_original - padding_s)
      actual_padding_before <- start_sec_original - clip_start_sec
      clip_duration <- actual_padding_before + analysis_range + padding_s

      pool::poolWithTransaction(pool, function(conn) {

        q_path <- "
          SELECT af.relative_path, d.deployment_path
          FROM import.audio_files af
          JOIN import.deployments d ON af.deployment_id = d.deployment_id
          WHERE af.audio_file_id = $1
        "
        path_res <- DBI::dbGetQuery(conn, q_path, params = list(row$audio_file_id))
        if (nrow(path_res) == 0) stop("Audio file ID not found in DB.")

        rel_clean <- sub("^/+|\\\\+", "", path_res$relative_path)
        full_path <- file.path(path_res$deployment_path, rel_clean)
        if (!file.exists(full_path)) stop("Source file not found on disk: ", full_path)

        insert_q <- "
          INSERT INTO import.spectrograms
          (audio_file_id, begin_time_ms, result_id, buffer_ms, duration_ms, resolution_x, resolution_y, freq_min, freq_max)
          VALUES ($1, $2, $3, $4, $5, $6, $7, 0, 15000)
          RETURNING spectrogram_id
        "
        val_result_id <- if(!is.null(row$result_id) && !is.na(row$result_id)) row$result_id else NA

        spec_db <- DBI::dbGetQuery(conn, insert_q, params = list(
          row$audio_file_id,
          row$begin_time_ms,
          val_result_id,
          actual_padding_before,
          as.integer(clip_duration),
          video_width,
          video_height
        ))
        spec_id <- spec_db$spectrogram_id

        final_filename <- paste0(spec_id, ".mp4")
        out_file <- file.path(output_dir, final_filename)
        temp_audio <- file.path(temp_dir, paste0("temp_", spec_id, ".mp3"))

        # Audio zuschneiden
        av::av_audio_convert(full_path, temp_audio,
                             start_time = clip_start_sec,
                             total_time = clip_duration,
                             verbose = FALSE)

        if (has_ffmpeg) {
          # --- MIT Linien: Roh-Video → FFmpeg drawbox → Final ---
          temp_video <- file.path(temp_dir, paste0("temp_vid_", spec_id, ".mp4"))

          av::av_spectrogram_video(temp_audio, temp_video,
                                   width = video_width, height = video_height,
                                   res = video_res, verbose = FALSE)


          # --- DER FIX: Harte FFmpeg Pixel-Ränder nutzen ---
          margin_left <- 130  # Platz für die linke Y-Achse (FREQUENCY)
          margin_right <- 0   # Spektrogramm geht rechts bis ans Ende

          # 1. Pixel pro Sekunde im AKTIVEN Bereich berechnen
          active_width <- video_width - margin_left - margin_right
          px_per_sec <- active_width / clip_duration

          # 2. Den linken Rand als Startpunkt addieren!
          line1_x <- margin_left + as.integer(actual_padding_before * px_per_sec)
          line2_x <- margin_left + as.integer((actual_padding_before + analysis_range) * px_per_sec)
          # -------------------------------------------------

          vfilter_str <- sprintf(
            "drawbox=x=%d:y=0:w=2:h=ih:color=green@0.8:t=fill,drawbox=x=%d:y=0:w=2:h=ih:color=green@0.8:t=fill",
            line1_x, line2_x
          )

          system2(ffmpeg_path, c(
            "-y", "-i", shQuote(temp_video),
            "-vf", shQuote(vfilter_str),
            "-c:a", "copy",
            shQuote(out_file)
          ), stdout = FALSE, stderr = FALSE)

          if (file.exists(temp_video)) file.remove(temp_video)
        } else {
          # --- OHNE Linien: Direkt ins Ziel ---
          av::av_spectrogram_video(temp_audio, out_file,
                                   width = video_width, height = video_height,
                                   res = video_res, verbose = FALSE)
        }

        if (file.exists(temp_audio)) file.remove(temp_audio)
      })
    }, error = function(e) {
      errors[[length(errors) + 1]] <<- list(row_index = i, error = e$message)
    })
    if (verbose) utils::setTxtProgressBar(pb, i)
  }
  if (verbose) close(pb)

  if(length(errors) > 0) print(errors)
  return(list(status = "done", errors = errors))
}



#' Generate Audio Clips (mp3) for Verification
#'
#' Ersetzt build_spectrogram_db(). Statt Video wird nur ein Audio-Clip
#' erzeugt; das Spektrogramm rendert der Browser per wavesurfer.js.
#'
#' @param data A data frame with audio_file_id, begin_time_ms, result_id, confidence.
#' @param pool A DBI connection pool.
#' @param padding_s Numeric. Seconds of context before and after the detection.
#' @param analysis_range Numeric. Length of the BirdNET analysis window (usually 3s).
#' @param output_dir Character. Directory where the .mp3 files will be saved.
#' @param verbose Logical. Whether to show a progress bar.
#'
#' @return A list with processing status and errors.
#' @importFrom DBI dbGetQuery dbExecute
#' @importFrom pool poolWithTransaction
#' @importFrom av av_audio_convert
#' @export
build_audio_clips_db <- function(data, pool, padding_s = 5, analysis_range = 3,
                                 output_dir = "spectrograms", verbose = TRUE) {

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  if (verbose) pb <- utils::txtProgressBar(min = 0, max = nrow(data), style = 3)
  errors <- list()

  for (i in seq_len(nrow(data))) {
    row <- data[i, ]
    tryCatch({

      # 1. Zeiten berechnen
      start_sec_original <- as.numeric(row$begin_time_ms) / 1000.0
      clip_start_sec <- max(0, start_sec_original - padding_s)
      actual_padding_before <- start_sec_original - clip_start_sec
      clip_duration <- actual_padding_before + analysis_range + padding_s

      pool::poolWithTransaction(pool, function(conn) {

        # 2. Pfad holen
        q_path <- "
          SELECT af.relative_path, d.deployment_path
          FROM import.audio_files af
          JOIN import.deployments d ON af.deployment_id = d.deployment_id
          WHERE af.audio_file_id = $1
        "
        path_res <- DBI::dbGetQuery(conn, q_path, params = list(row$audio_file_id))
        if (nrow(path_res) == 0) stop("Audio file ID not found in DB.")

        rel_clean <- sub("^/+|\\\\+", "", path_res$relative_path)
        full_path <- file.path(path_res$deployment_path, rel_clean)
        if (!file.exists(full_path)) stop("Source file not found: ", full_path)

        # 3. DB Eintrag
        insert_q <- "
          INSERT INTO import.spectrograms
          (audio_file_id, begin_time_ms, result_id, buffer_ms, duration_ms,
           resolution_x, resolution_y, freq_min, freq_max)
          VALUES ($1, $2, $3, $4, $5, 0, 0, 0, 15000)
          RETURNING spectrogram_id
        "
        val_result_id <- if(!is.null(row$result_id) && !is.na(row$result_id)) row$result_id else NA

        spec_db <- DBI::dbGetQuery(conn, insert_q, params = list(
          row$audio_file_id,
          row$begin_time_ms,
          val_result_id,
          actual_padding_before,      # buffer_ms (Sekunden bis Detektion)
          as.integer(clip_duration)   # duration_ms
        ))
        spec_id <- spec_db$spectrogram_id

        # 4. Audio Clip (mp3) erzeugen – KEIN Video mehr!
        out_file <- file.path(output_dir, paste0(spec_id, ".mp3"))

        av::av_audio_convert(
          full_path, out_file,
          start_time = clip_start_sec,
          total_time = clip_duration,
          verbose = FALSE
        )
      })
    }, error = function(e) {
      errors[[length(errors) + 1]] <<- list(row_index = i, error = e$message)
    })
    if (verbose) utils::setTxtProgressBar(pb, i)
  }
  if (verbose) close(pb)

  if(length(errors) > 0) print(errors)
  return(list(status = "done", errors = errors))
}
