-- Migration: add binary blob columns to import.spectrograms
-- Run once on existing databases; safe to re-run (IF NOT EXISTS).

ALTER TABLE import.spectrograms
  ADD COLUMN IF NOT EXISTS audio_data BYTEA,
  ADD COLUMN IF NOT EXISTS image_data BYTEA;
