#' Internal function to load package data
#' This will be replaced by database imports
#'
#' @noRd
data_setup <- function() {
# =====================================================================
# data_setup.R  — data loading
# =====================================================================

# Safe internal paths -------------------------------------------------
audio_dir     <- dirname(system.file("app/data/metadata.csv", package = "evalpam"))
metadata_file <- system.file("app/data/metadata.csv", package = "evalpam")
species_file  <- system.file("app/data/arten_liste.csv", package = "evalpam")
coords_file  <- system.file("app/data/ffk_asf.csv", package = "evalpam")
export_file   <- system.file("app/data/export.csv", package = "evalpam")

# ---- Read species ----
arten <- readr::read_csv(species_file, show_col_types = FALSE) |>
  dplyr::pull(art)

# ---- Read coordinates ----
coords <- readr::read_csv(coords_file, show_col_types = FALSE) |>
  dplyr::mutate(url = paste0("https://www.google.com/maps/search/?api=1&query=", Y, ",", X)) |>
  dplyr::select(id, url)

# ---- Audio files ----
shiny::addResourcePath("sample_audio", audio_dir)

audio_files <- data.frame(
  path = file.path("sample_audio", list.files(audio_dir, ".mp4$")),
  stringsAsFactors = FALSE
) |>
  dplyr::mutate(
    id_seq = stringi::stri_extract_first_regex(path, "[0-9]+"),
    id_numeric = as.numeric(id_seq)
  ) |>
  dplyr::arrange(id_numeric) |>
  dplyr::select(path, id_seq)

mylist <- audio_files$path
names(mylist) <- audio_files$id_seq

# ---- Metadata ----
meta_data <- readr::read_csv(metadata_file, show_col_types = FALSE) |>
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

# ---- Save into options() so app_ui & app_server can access it ----
options(evalpam.data = list(
  arten = arten,
  coords = coords,
  data = meta_data,
  audio_files = audio_files,
  mylist = mylist,
  export_file = export_file
))

invisible(TRUE)
}
