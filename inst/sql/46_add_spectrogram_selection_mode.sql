-- Migration: record how each spectrogram clip was selected.
-- Safe to re-run: ADD COLUMN IF NOT EXISTS skips the whole clause (incl. the
-- inline CHECK) once the column exists, so no duplicate constraint is created.
--
-- Values:
--   'top' / 'random' / 'stratified' -> the confidence_selection_mode that
--        sample_results_table() used to pick the underlying detection;
--   'custom' -> clip built from a caller-supplied set (e.g. build_audio_clips_db
--        called directly, or an explicit result_id/spectrogram_id list);
--   NULL -> legacy rows created before this column existed.

ALTER TABLE import.spectrograms
  ADD COLUMN IF NOT EXISTS selection_mode varchar(20)
    CHECK (selection_mode IN ('top', 'random', 'stratified', 'custom'));

CREATE INDEX IF NOT EXISTS idx_spectrograms_selection_mode
  ON import.spectrograms (selection_mode);
