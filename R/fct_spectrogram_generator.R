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

        # --- FIX: PFAD HOLEN MIT JOIN ---
        # Wir holen relative_path UND deployment_path aus der deployment Tabelle
        q_path <- "
          SELECT af.relative_path, d.deployment_path
          FROM import.audio_files af
          JOIN import.deployments d ON af.deployment_id = d.deployment_id
          WHERE af.audio_file_id = $1
        "
        path_res <- DBI::dbGetQuery(conn, q_path, params = list(row$audio_file_id))

        if (nrow(path_res) == 0) stop("Audio file ID not found in DB.")

        # Pfad sauber zusammenbauen: Deployment Pfad + Dateiname
        rel_clean <- sub("^/+|\\\\+", "", path_res$relative_path)
        full_path <- file.path(path_res$deployment_path, rel_clean)

        if (!file.exists(full_path)) stop("Source file not found on disk: ", full_path)

        # 3. DB Eintrag anlegen
        # 3. DB Eintrag anlegen
        insert_q <- "
          INSERT INTO import.spectrograms
          (audio_file_id, begin_time_ms, result_id, buffer_ms, duration_ms, resolution_x, resolution_y, freq_min, freq_max)
          VALUES ($1, $2, $3, $4, $5, $6, $7, 0, 15000)
          RETURNING spectrogram_id
        "

        # Prüfung: Falls result_id in den Daten fehlt oder NA ist, wird NULL (NA) gesendet
        val_result_id <- if(!is.null(row$result_id) && !is.na(row$result_id)) row$result_id else NA

        spec_db <- DBI::dbGetQuery(conn, insert_q, params = list(
          row$audio_file_id,           # $1: ZWINGEND (NOT NULL)
          row$begin_time_ms,           # $2: ZWINGEND (NOT NULL)
          val_result_id,               # $3: Optional (kann NULL sein)
          actual_padding_before,       # $4
          as.integer(clip_duration),   # $5
          video_width,                 # $6
          video_height                 # $7
        ))
        spec_id <- spec_db$spectrogram_id

        # 4. Video rendern
        final_filename <- paste0(spec_id, ".mp4")
        out_file <- file.path(output_dir, final_filename)
        temp_audio <- file.path(temp_dir, paste0("temp_", spec_id, ".mp3"))

        av::av_audio_convert(full_path, temp_audio, start_time = clip_start_sec, total_time = clip_duration, verbose = FALSE)
        av::av_spectrogram_video(temp_audio, out_file, width = video_width, height = video_height, res = video_res, verbose = FALSE)

        if (file.exists(temp_audio)) file.remove(temp_audio)
      })
    }, error = function(e) {
      errors[[length(errors) + 1]] <<- list(row_index = i, error = e$message)
    })
    if (verbose) utils::setTxtProgressBar(pb, i)
  }
  if (verbose) close(pb)

  # Zeige Fehler direkt an, falls welche da sind
  if(length(errors) > 0) print(errors)

  return(list(status = "done", errors = errors))
}
