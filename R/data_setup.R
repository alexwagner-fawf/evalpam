#' Load data from DB into Global Options
#' @param pool The database connection pool
data_setup <- function(pool) {

  # 1. Arten laden (aus public.lut_species_code)
  # Annahme: Du willst den deutschen Namen oder den Code
  arten <- dplyr::tbl(pool, dbplyr::in_schema("public", "lut_species_code")) |>
    dplyr::pull(species_long_de) # Oder species_short, je nachdem was deine UI braucht

  # 2. Koordinaten / Deployments laden (aus import.deployments)
  # Hinweis: Deine DB Tabelle braucht Spalten für X/Y (Latitude/Longitude)!
  # Falls die noch fehlen, ist das ok, dann lass diesen Teil erstmal leer oder nimm Dummies.
  coords_db <- dplyr::tbl(pool, dbplyr::in_schema("import", "deployments")) |>
    dplyr::collect()

  # Hier müsstest du prüfen, ob du X/Y Spalten in der DB hast.
  # Falls nicht, nimm vorerst weiter die CSV oder ergänze die Spalten in der DB.
  coords <- if("X" %in% names(coords_db)) {
    coords_db |>
      dplyr::mutate(url = paste0("https://www.google.com/maps/search/?api=1&query=", Y, ",", X)) |>
      dplyr::select(deployment_id, url) # id -> deployment_id anpassen
  } else {
    # Fallback falls DB noch keine Geodaten hat
    data.frame(id = character(0), url = character(0))
  }

  # 3. Metadaten & Audio Files (Das Herzstück)
  # Wir joinen Results mit AudioFiles, um den Pfad zu bekommen
  meta_data_db <- dplyr::tbl(pool, dbplyr::in_schema("import", "results")) |>
    dplyr::left_join(
      dplyr::tbl(pool, dbplyr::in_schema("import", "audio_files")),
      by = "audio_file_id"
    ) |>
    dplyr::left_join(
      dplyr::tbl(pool, dbplyr::in_schema("public", "lut_species_code")),
      by = "species_id"
    ) |>
    dplyr::collect()

  # Daten für die UI aufbereiten (Spalten umbenennen, damit es zum alten Code passt)
  meta_data <- meta_data_db |>
    dplyr::mutate(
      prediction = species_long_de,     # oder species_short
      start = begin_time_ms,
      end = end_time_ms,
      species_to_check = "TODO",        # Das müsstest du definieren (wo kommt das her?)
      score = confidence,
      id_seq = as.character(audio_file_id),
      # Pfad zusammenbauen: Basis-Ordner + Dateiname aus DB
      path = file.path("spectograms", relative_path)
    ) |>
    dplyr::select(prediction, start, end, score, id_seq, path)

  # 4. Audio File Liste für die Playlist
  # Wir nehmen einfach die Pfade, die wir oben schon gebaut haben
  audio_files <- meta_data |>
    dplyr::select(path, id_seq) |>
    dplyr::distinct()

  mylist <- audio_files$path
  names(mylist) <- audio_files$id_seq

  # Export Pfad bleibt lokal
  export_file <- system.file("app/data/export.csv", package = "evalpam")

  # ---- Save into options() ----
  # Das Format bleibt exakt gleich, damit deine UI nicht kaputt geht!
  options(evalpam.data = list(
    arten = arten,
    coords = coords,
    data = meta_data,
    audio_files = audio_files,
    mylist = mylist,
    export_file = export_file
  ))

  message("Daten erfolgreich aus der Datenbank geladen!")
}

#' #' Load data from DB into Global Options
#' #' @param pool The database connection pool
#' data_setup <- function(pool) {
#'
#'   # 1. Arten laden (aus public.lut_species_code)
#'   # Annahme: Du willst den deutschen Namen oder den Code
#'   arten <- dplyr::tbl(pool, dbplyr::in_schema("public", "lut_species_code")) |>
#'     dplyr::pull(species_long_de) # Oder species_short, je nachdem was deine UI braucht
#'
#'   # 2. Koordinaten / Deployments laden (aus import.deployments)
#'   # Hinweis: Deine DB Tabelle braucht Spalten für X/Y (Latitude/Longitude)!
#'   # Falls die noch fehlen, ist das ok, dann lass diesen Teil erstmal leer oder nimm Dummies.
#'   coords_db <- dplyr::tbl(pool, dbplyr::in_schema("import", "deployments")) |>
#'     dplyr::collect()
#'
#'   # Hier müsstest du prüfen, ob du X/Y Spalten in der DB hast.
#'   # Falls nicht, nimm vorerst weiter die CSV oder ergänze die Spalten in der DB.
#'   coords <- if("X" %in% names(coords_db)) {
#'     coords_db |>
#'       dplyr::mutate(url = paste0("https://www.google.com/maps/search/?api=1&query=", Y, ",", X)) |>
#'       dplyr::select(deployment_id, url) # id -> deployment_id anpassen
#'   } else {
#'     # Fallback falls DB noch keine Geodaten hat
#'     data.frame(id = character(0), url = character(0))
#'   }
#'
#'   # 3. Metadaten & Audio Files (Das Herzstück)
#'   # Wir joinen Results mit AudioFiles, um den Pfad zu bekommen
#'   meta_data_db <- dplyr::tbl(pool, dbplyr::in_schema("import", "results")) |>
#'     dplyr::left_join(
#'       dplyr::tbl(pool, dbplyr::in_schema("import", "audio_files")),
#'       by = "audio_file_id"
#'     ) |>
#'     dplyr::left_join(
#'       dplyr::tbl(pool, dbplyr::in_schema("public", "lut_species_code")),
#'       by = "species_id"
#'     ) |>
#'     dplyr::collect()
#'
#'   # Daten für die UI aufbereiten (Spalten umbenennen, damit es zum alten Code passt)
#'   meta_data <- meta_data_db |>
#'     dplyr::mutate(
#'       prediction = species_long_de,     # oder species_short
#'       start = begin_time_ms,
#'       end = end_time_ms,
#'       species_to_check = "TODO",        # Das müsstest du definieren (wo kommt das her?)
#'       score = confidence,
#'       id_seq = as.character(audio_file_id),
#'       # Pfad zusammenbauen: Basis-Ordner + Dateiname aus DB
#'       path = file.path("spectrograms", relative_path)
#'     ) |>
#'     dplyr::select(prediction, start, end, score, id_seq, path)
#'
#'   # 4. Audio File Liste für die Playlist
#'   # Wir nehmen einfach die Pfade, die wir oben schon gebaut haben
#'   audio_files <- meta_data |>
#'     dplyr::select(path, id_seq) |>
#'     dplyr::distinct()
#'
#'   mylist <- audio_files$path
#'   names(mylist) <- audio_files$id_seq
#'
#'   # Export Pfad bleibt lokal
#'   export_file <- system.file("app/data/export.csv", package = "evalpam")
#'
#'   # ---- Save into options() ----
#'   # Das Format bleibt exakt gleich, damit deine UI nicht kaputt geht!
#'   options(evalpam.data = list(
#'     arten = arten,
#'     coords = coords,
#'     data = meta_data,
#'     audio_files = audio_files,
#'     mylist = mylist,
#'     export_file = export_file
#'   ))
#'
#'   message("Daten erfolgreich aus der Datenbank geladen!")
#' }
