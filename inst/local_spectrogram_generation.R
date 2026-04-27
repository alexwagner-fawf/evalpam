pool <- set_db_pool()

sampled_results <- sample_results_table(
  confidence_selection_mode = "top",
  n_per_species = 30,
  deployment_ids = 1:1000,
  grouping_by = c("species_id", "deployment_id"),
  pool = pool)

build_spectrogram_db(sampled_results,
                     pool =  pool,
                     output_dir = "/home/alex/Dokumente/sound_db/project_1/spectrograms/")
