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
default_required_annotation_type_id <- 3L
# ─────────────────────────────────────────────────────────────────────────────

pool <- set_db_pool()
on.exit(pool::poolClose(pool), add = TRUE)

# ── Project / deployment metadata ──────────────────────────────────────────────
# Build an sf object `deployments_sf` with:
#   deployment_name  (character, must match folder names under project_folder)
#   geometry         (POINT, EPSG:4326)
#   plus any optional columns: device_manufacturer, device_modelname, notes, valid
#
# The block below is project-specific — replace with your own data source.

db_con   <- frwf::get_proj_attr("kw2100", "db_group")
pam_locs <- frwf::db_read("pam_locations", db_con) |>
  dplyr::filter(aktiv) |>
  sf::st_transform(4326) |>
  dplyr::rename(notes = behandlungsvariante) |>
  dplyr::mutate(standortID = paste("Klimawald2100", gebiet, standortID, sep = "_")) |>
  dplyr::select(notes, standortID)

# Example: derive deployment_name from standortID and join to scanning index
# (adjust the key expression to match your naming convention)
deployments_sf <- pam_locs |>
  dplyr::rename(deployment_name = standortID) |>
  dplyr::mutate(
    device_manufacturer = "Wildlife Acoustics",
    device_modelname    = "SongMeter Mini 2 Li"
  )
# ─────────────────────────────────────────────────────────────────────────────

ingest_audio_files(
  pool                                = pool,
  project_name_short                  = "kw2100",
  project_name_long                   = "Klimawald 2100 - Biodiversität auf Störungsflächen",
  description                         = "In diesem Projekt werden Soundboxen im Totholz, auf Freiflächen sowie im lebenden Fichtenwald platziert (2024-2026)",
  contact                             = "alexander.wagner@wald-rlp.de",
  organisation                        = "FAWF",
  project_folder                      = project_folder,
  deployments_sf                      = deployments_sf,
  folder_depth                        = folder_depth,
  force_tz                            = force_tz,
  force_exif                          = force_exif,
  default_required_annotation_type_id = default_required_annotation_type_id,
  exiftool_exe                        = exiftool_exe,
  perl_path                           = perl_path
)
