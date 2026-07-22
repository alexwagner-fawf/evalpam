-- Hot path 1: project_data() - die größte Query der App
CREATE INDEX IF NOT EXISTS idx_results_audio_score
  ON import.results (audio_file_id, confidence DESC);

CREATE INDEX IF NOT EXISTS idx_spectrograms_result
  ON import.spectrograms (result_id);

-- Hot path 2: time-based joins everywhere
CREATE INDEX IF NOT EXISTS idx_spectrograms_time
  ON import.spectrograms (audio_file_id, begin_time_ms);

-- Hot path 3: annotation_status lookups (used in filtered_files, status check)
CREATE INDEX IF NOT EXISTS idx_annotation_status_lookup
  ON import.annotation_status (audio_file_id, begin_time_ms, target_species_id);

-- Hot path 4: ground_truth_annotations for occupancy counts
CREATE INDEX IF NOT EXISTS idx_gt_occupancy_lookup
  ON import.ground_truth_annotations (audio_file_id, begin_time_ms, species_id)
  WHERE certainty_id = 1 AND is_present = TRUE;
