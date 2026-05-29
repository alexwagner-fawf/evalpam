# Alle audio_files für Plot 01
plot01_ids <- DBI::dbGetQuery(pool,
                              "SELECT af.audio_file_id FROM import.audio_files af
   JOIN import.deployments d USING (deployment_id)
   WHERE d.deployment_name = 'Plot_01'")$audio_file_id

group_from_audio_files(pool, project_id = 1,
                       group_name = "Plot_01",
                       target_count = 3L,
                       audio_file_ids = plot01_ids)

# Nur Morgenstunden 04:00–07:00
plot01_morning_ids <- DBI::dbGetQuery(pool,
                                      "SELECT af.audio_file_id FROM import.audio_files af
   JOIN import.deployments d USING (deployment_id)
   WHERE d.deployment_name = 'Plot_01'
     AND EXTRACT(HOUR FROM af.timestamp_start) BETWEEN 4 AND 6")$audio_file_id

group_from_audio_files(pool, project_id = 1,
                       group_name = "Plot_01_Morgens",
                       target_count = 3L,
                       audio_file_ids = plot01_morning_ids)
