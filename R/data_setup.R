# ============================================================================
# Bird Sound Verification App - Data Setup (golem-ready)
# ============================================================================

# ---- Define file locations inside the installed package ----

# Read-only CSVs shipped with the package
METADATA_FILE <- system.file("extdata", "metadata.csv", package = "evalpam")
SPECIES_FILE  <- system.file("extdata", "arten_liste.csv", package = "evalpam")
COORDS_FILE   <- system.file("extdata", "ffk_asf.csv", package = "evalpam")

# Audio folder inside inst/
AUDIO_DIR <- system.file("app/audio", package = "evalpam")

# Add Shiny resource path for serving audio files
if (dir.exists(AUDIO_DIR)) {
  shiny::addResourcePath("sample_audio", AUDIO_DIR)
}

# ---- Create writable directory for exports ----
EXPORT_DIR <- file.path(getwd(), "export")
if (!dir.exists(EXPORT_DIR)) dir.create(EXPORT_DIR, recursive = TRUE)

EXPORT_FILE <- file.path(EXPORT_DIR, "export.csv")


# ---- Load Species List ----
arten <- readr::read_csv(SPECIES_FILE, show_col_types = FALSE) |>
  dplyr::pull(art)


# ---- Load coordinates for map links ----
coords <- readr::read_csv(COORDS_FILE, show_col_types = FALSE) |>
  dplyr::mutate(url = paste0(
    "https://www.google.com/maps/search/?api=1&query=", Y, ",", X
  )) |>
  dplyr::select(id, url)


# ---- Load Audio Files ----
audio_files <- data.frame(
  path = file.path("sample_audio", list.files(AUDIO_DIR, pattern = ".mp4$")),
  stringsAsFactors = FALSE
) |>
  dplyr::mutate(
    id_seq = stringi::stri_extract_first_regex(path, "[0-9]+"),
    id_numeric = as.numeric(id_seq)
  ) |>
  dplyr::arrange(id_numeric) |>
  dplyr::select(path, id_seq)


# ---- Load Metadata ----
meta_data <- readr::read_csv(METADATA_FILE, locale = readr::locale(),
                             show_col_types = FALSE) |>
  dplyr::filter(!is.na(path)) |>
  dplyr::select(
    prediction = art_deutsch,
    start = begin_time_s,
    end = end_time_s,
    species_to_check = art_to_check,
    time_stamp,
    fk_plots_id,
    score = confidence,
    id_seq
  ) |>
  dplyr::mutate(
    id_seq = as.character(id_seq),
    plot_info = paste(fk_plots_id, format(time_stamp, tz = "Europe/Berlin"))
  ) |>
  dplyr::left_join(audio_files, by = "id_seq") |>
  dplyr::select(-time_stamp, -fk_plots_id)


# ---- Load previous verifications (export.csv) ----
if (!file.exists(EXPORT_FILE)) {
  readr::write_csv(
    data.frame(
      verification = character(0),
      id_seq = character(0),
      start = numeric(0),
      end = numeric(0),
      test_file = character(0),
      path = character(0),
      timestamp = character(0),
      verification_by = character(0),
      plot_info = character(0)
    ),
    EXPORT_FILE
  )
}

temp_export <- readr::read_csv(EXPORT_FILE, show_col_types = FALSE)

if (nrow(temp_export) > 0) {
  temp_data <- temp_export |>
    dplyr::group_by(id_seq) |>
    dplyr::slice_max(timestamp, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::mutate(id_seq = as.character(id_seq)) |>
    dplyr::rename(prediction = verification)

  meta_data <- meta_data |>
    dplyr::filter(!(id_seq %in% temp_data$id_seq)) |>
    dplyr::bind_rows(temp_data)
}


# ---- Selection list for UI ----
mylist <- as.list(audio_files$path)
names(mylist) <- audio_files$id_seq
