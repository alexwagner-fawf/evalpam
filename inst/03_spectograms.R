library(evalpam)

# ── CONFIGURATION ─────────────────────────────────────────────────────────────
project_id    <- 1          # filter deployments to this project (NULL = all)
n_per_species <- 30          # top-N detections per species × deployment
padding_s     <- 2          # seconds of context before/after detection
export_to_db  <- TRUE       # upload MP3 + PNG blobs to import.spectrograms
generate_image <- TRUE      # render PNG spectrogram (stored in image_data)

# Output directory for MP3 cache files.
# Reads spectogram_folder from .Renviron; falls back to ./spectograms if unset.
output_dir <- Sys.getenv("spectogram_folder",
                         unset = file.path(getwd(), "spectograms"))
# ─────────────────────────────────────────────────────────────────────────────
#devtools::load_all()
pool <- set_db_pool()
on.exit(pool::poolClose(pool), add = TRUE)

deployments <- sf::st_read(pool, DBI::Id("import", "deployments"))


if (!is.null(project_id)) {
  deployments <- dplyr::filter(deployments, .data$project_id == !!project_id)
}

# One representative deployment per location (earliest start date).
# na.rm = TRUE prevents NA start_datetime from poisoning min(), which would
# make the equality check return NA and drop every row in the group.
# If all deployments have empty geometry (dev mode) they form one group — that
# is intentional; only the earliest deployment per unique location is used.
selected_deployments <- deployments |>
  dplyr::group_by(geometry) |>
  dplyr::filter(start_datetime == min(start_datetime, na.rm = TRUE)) |>
  dplyr::ungroup()

message(sprintf("project_id=%s: %d deployment(s) total, %d selected (one per location).",
                project_id, nrow(deployments), nrow(selected_deployments)))

if (nrow(selected_deployments) == 0) {
  stop("No deployments found for project_id=", project_id,
       ". Check that the project exists and has deployments in import.deployments.")
}

samples <- sample_results_table(
  confidence_selection_mode = "top",
  n_per_species             = n_per_species,
  deployment_ids            = selected_deployments$deployment_id,
  grouping_by               = c("species_id", "deployment_id"),
  pool                      = pool
)

if (nrow(samples) == 0) {
  stop("sample_results_table() returned 0 rows for deployment_id(s): ",
       paste(selected_deployments$deployment_id, collapse = ", "),
       ".\nCheck that import.results contains inference output for these deployments ",
       "(run inst/02_inference.R first).")
}

samples_group <- samples |>
  dplyr::arrange(dplyr::desc(confidence)) |>
  dplyr::group_by(deployment_id, species_id) |>
  dplyr::group_split()

message(sprintf(
  "Generating spectrograms for %d group(s) across %d deployment(s).",
  length(samples_group), dplyr::n_distinct(samples$deployment_id)
))

for (sample_i in seq_along(samples_group)) {
  message(sprintf("[%d/%d] deployment=%s species=%s",
                  sample_i, length(samples_group),
                  samples_group[[sample_i]]$deployment_id[1],
                  samples_group[[sample_i]]$species_id[1]))

  evalpam::build_audio_clips_db(
    data          = samples_group[[sample_i]] |> dplyr::arrange(dplyr::desc(confidence)),
    pool          = pool,
    padding_s     = padding_s,
    output_dir    = output_dir,
    export_to_db  = export_to_db,
    generate_image = generate_image,
    verbose       = FALSE
  )
}

message("Done.")
