-- Migration: add/remove blob columns on import.spectrograms
-- Safe to re-run (IF NOT EXISTS / IF EXISTS guards).

ALTER TABLE import.spectrograms
  ADD COLUMN IF NOT EXISTS audio_data BYTEA;

-- image_data is no longer used; the browser renders spectrograms via
-- wavesurfer.js directly from the audio clip.
ALTER TABLE import.spectrograms
  DROP COLUMN IF EXISTS image_data;
