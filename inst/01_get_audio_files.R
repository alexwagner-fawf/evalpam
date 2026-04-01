library(evalpam)

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
# Adjust all values in this block before running.

project_folder  <- "~/Dokumente/sound_db/project_1/"
folder_depth    <- 1L    # directory levels below project_folder to scan for deployments

# ExifTool path (Windows only). Set to NULL to use system PATH (Linux/Mac).
exiftool_exe <- "C:/Users/awagner/Documents/evalpam/exiftool.exe"
perl_path    <- "C:/Users/awagner/Documents/perl"

# retrieve_local_file_info options
force_tz                            <- "UTC"
force_exif                          <- FALSE
list_files_retries                  <- 3L
list_files_verify                   <- TRUE
list_files_min_stable_scans         <- 2L
default_required_annotation_type_id <- 3L

# Path length threshold above which an exif result is considered corrupt
# (exiftoolr sometimes concatenates error messages into the path field)
max_relative_path_length <- 200L
# ─────────────────────────────────────────────────────────────────────────────

# ── Project / deployment metadata ─────────────────────────────────────────────
# This block is project-specific. Replace with your own data source that
# produces a data frame `pam_locs` with columns:
#   deployment_name  (character, matches the folder name used as deployment_name)
#   geometry_x_4326  (numeric, longitude WGS84)
#   geometry_y_4326  (numeric, latitude  WGS84)
# plus any optional metadata columns (notes, device type, etc.).

db_con   <- frwf::get_proj_attr("kw2100", "db_group")
pam_locs <- frwf::db_read("pam_locations", db_con) |>
  dplyr::filter(aktiv) |>
  sf::st_transform(4326) |>
  dplyr::rename(notes = behandlungsvariante) |>
  dplyr::mutate(standortID = paste("Klimawald2100", gebiet, standortID, sep = "_")) |>
  dplyr::select(notes, standortID)

pam_locs <- pam_locs |>
  sf::st_drop_geometry() |>
  dplyr::bind_cols(sf::st_coordinates(pam_locs)) |>
  dplyr::rename(geometry_x_4326 = X, geometry_y_4326 = Y)
# ─────────────────────────────────────────────────────────────────────────────

pool <- set_db_pool()
on.exit(pool::poolClose(pool), add = TRUE)

# ── Upsert project ────────────────────────────────────────────────────────────

projects <- DBI::dbReadTable(pool, DBI::Id("import", "projects"))

if ("kw2100" %in% projects$project_name_short) {
  project_id <- projects |>
    dplyr::filter(project_name_short == "kw2100") |>
    dplyr::pull(project_id)
} else {
  project_id <- evalpam::upsert_project(
    conn               = pool,
    project_name_short = "kw2100",
    project_name_long  = "Klimawald 2100 - Biodiversität auf Störungsflächen",
    description        = "In diesem Projekt werden Soundboxen im Totholz, auf Freiflächen sowie im lebenden Fichtenwald platziert (2024-2026)",
    contact            = "alexander.wagner@wald-rlp.de",
    organisation       = "FAWF"
  )
}

# ── Scan project folder for audio files ───────────────────────────────────────

if (!is.null(exiftool_exe)) {
  exiftoolr::configure_exiftoolr(command = exiftool_exe, perl_path = perl_path)
}

out <- retrieve_local_file_info(
  project_id                          = project_id,
  project_folder                      = project_folder,
  force_tz                            = force_tz,
  folder_depth                        = folder_depth,
  list_files_retries                  = list_files_retries,
  force_exif                          = force_exif,
  list_files_verify                   = list_files_verify,
  list_files_min_stable_scans         = list_files_min_stable_scans,
  default_required_annotation_type_id = default_required_annotation_type_id
)

# ── Build and upload deployments ───────────────────────────────────────────────
# WARNING: this section requires manual revision to match the column naming and
# join keys of your project-specific pam_locs data frame.

deployment_ids <- out$deployment_index |>
  readr::read_csv()

deployments <- deployment_ids

deployments <- deployments |>
  dplyr::mutate(
    standortID = apply(
      stringr::str_split(deployment_name, "_", simplify = TRUE)[, 1:3],
      1, paste0, collapse = "_"
    ),
    .before = start_datetime
  ) |>
  dplyr::select(-geometry_x_4326, -geometry_y_4326, -notes) |>
  dplyr::left_join(pam_locs) |>
  dplyr::mutate(valid = TRUE) |>
  dplyr::filter(!is.na(geometry_y_4326))
# deployments$geometry_x_4326 <- runif(nrow(deployment_ids), 5,6)
# deployments$geometry_y_4326 <- runif(nrow(deployment_ids), 43,44)
# deployments$valid <- TRUE
# deployments$project_id <- 1

if (nrow(deployments) == 0) {
  stop("No deployments with valid coordinates after joining pam_locs. ",
       "Check that deployment_name values match the standortID column in pam_locs.")
}

deployments <- deployments |>
  dplyr::mutate(
    device_manufacturer = "Wildlife Acoustics",
    device_modelname    = ifelse(
      stringr::str_detect(deployment_name, "2024"),
      "SongMeter Micro 1/2",
      "SongMeter Mini 2 Li"
    )
  ) |>
  sf::st_as_sf(coords = c("geometry_x_4326", "geometry_y_4326")) |>
  sf::st_set_crs(4326)

upserted_deployment_ids <- upsert_deployments_sf(deployments, conn = pool)

# Re-read from DB to get canonical deployment_id values
deployments <- dplyr::tbl(pool, DBI::Id("import", "deployments")) |>
  dplyr::filter(deployment_id %in% upserted_deployment_ids) |>
  dplyr::collect()

# ── Build and upload audio file records ───────────────────────────────────────

# Only upload indices for new deployments unless all are needed
if (length(out$new_audio_file_indices) == 0) {
  audio_file_indices <- out$all_audio_file_indices
} else {
  audio_file_indices <- out$new_audio_file_indices
}

df <- audio_file_indices |>
  lapply(fst::read_fst) |>
  dplyr::bind_rows() |>
  dplyr::rename(deployment_name = deployment_id) |>
  dplyr::left_join(deployments |> dplyr::select(deployment_id, deployment_name)) |>
  dplyr::relocate(deployment_id, .before = "deployment_name") |>
  dplyr::select(-deployment_name)

df_filtered <- df |>
  dplyr::filter(deployment_id %in% upserted_deployment_ids) |>
  dplyr::mutate(timestamp_start = as.POSIXct(timestamp_start, tz = "UTC")) |>
  dplyr::filter(!is.na(sample_rate)) |>
  dplyr::group_by(deployment_id) |>
  dplyr::mutate(dupls = duplicated(timestamp_start) | duplicated(timestamp_start, fromLast = TRUE))

# ── Resolve duplicated timestamps ─────────────────────────────────────────────
# Duplicates arise when copies of files exist on the network share.
# Strategy: re-parse the timestamp from the filename; keep the copy with the
# longest duration; fall back to date-only parsing for midnight recordings.

df_filtered_dupls_handling <- df_filtered |>
  dplyr::filter(dupls) |>
  dplyr::group_by(timestamp_start)

dttm <- df_filtered_dupls_handling$relative_path |>
  basename() |>
  tools::file_path_sans_ext() |>
  stringr::str_split(pattern = "_", simplify = TRUE) |>
  as.data.frame() |>
  (\(d) d[, (ncol(d) - 1):ncol(d)])() |>
  (\(d) apply(d, 1, paste, collapse = " "))() |>
  strptime(format = "%Y%m%d %H%M%S")

df_filtered_dupls_handling$timestamp_start <- dttm |>
  lubridate::ymd_hms() |>
  lubridate::force_tz("UTC")

dttm_nas <- is.na(df_filtered_dupls_handling$timestamp_start)

# Midnight recordings: ymd_hms fails on "YYYYMMDD 000000" — fall back to date only
if (any(dttm_nas)) {
  df_filtered_dupls_handling$timestamp_start[dttm_nas] <- (dttm |>
    lubridate::ymd() |>
    lubridate::force_tz("UTC"))[dttm_nas]
}

if (any(is.na(df_filtered_dupls_handling$timestamp_start))) {
  warning(sum(is.na(df_filtered_dupls_handling$timestamp_start)),
          " duplicate rows still have NA timestamp after fallback parsing. ",
          "These will be dropped.")
}

df_filtered_dupls_handling <- df_filtered_dupls_handling |>
  dplyr::group_by(timestamp_start, deployment_id) |>
  dplyr::mutate(dupls = dplyr::n() > 1) |>
  dplyr::arrange(dplyr::desc(duration_s))

df_filtered_dupls_cleaned <- df_filtered_dupls_handling |>
  dplyr::arrange(timestamp_start) |>
  dplyr::slice_head(n = 1)

df_filtered_cleaned <- df_filtered_dupls_cleaned |>
  dplyr::ungroup() |>
  dplyr::bind_rows(dplyr::filter(df_filtered, !dupls)) |>
  dplyr::arrange(deployment_id, timestamp_start)

# ── Detect corrupt exif paths ─────────────────────────────────────────────────
# exiftoolr occasionally concatenates error messages into the relative_path
# field, producing very long strings. Flag and optionally remove those indices.

df_filtered_cleaned <- df_filtered_cleaned |>
  dplyr::mutate(path_length = stringr::str_length(relative_path))

long_path_rows <- dplyr::filter(df_filtered_cleaned, path_length > max_relative_path_length)
problematic_deployment_ids <- unique(long_path_rows$deployment_id)

if (length(problematic_deployment_ids) > 0) {

  problematic_names <- deployments |>
    dplyr::filter(deployment_id %in% problematic_deployment_ids) |>
    dplyr::pull(deployment_name)

  audio_file_indices_2_remove <- audio_file_indices[
    basename(audio_file_indices) %in% paste0(problematic_names, "_afi.fst")
  ]

  confirm <- readline(paste0(
    "Remove the following index files due to potentially corrupt exif paths?\n",
    paste(audio_file_indices_2_remove, collapse = "\n"),
    "\n[y/n]: "
  ))

  if (confirm == "y") {
    file.remove(audio_file_indices_2_remove)
    df_filtered_cleaned <- dplyr::filter(df_filtered_cleaned,
                                          !deployment_id %in% problematic_deployment_ids)
  }
}

df_filtered_cleaned_final <- dplyr::select(df_filtered_cleaned, -dupls, -path_length) |>
  dplyr::filter(!is.na(timestamp_start))

# ── Upload audio file records ─────────────────────────────────────────────────

audio_file_ids <- upsert_audio_files_df(
  conn          = pool,
  df_audio      = df_filtered_cleaned_final,
  update_if_exists = TRUE
)

# Spot-check: print structure without downloading full result set
dplyr::tbl(pool, DBI::Id("import", "audio_files")) |>
  dplyr::filter(audio_file_id %in% audio_file_ids) |>
  dplyr::glimpse()
