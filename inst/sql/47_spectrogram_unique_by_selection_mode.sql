-- Migration: make (audio_file_id, begin_time_ms, selection_mode) the unique key
-- for import.spectrograms.
--
-- The old key (audio_file_id, begin_time_ms, required_annotation_type_id) was
-- toothless: the generator never populates required_annotation_type_id, so the
-- NULLs-distinct rule disabled it and duplicate clips of the same window
-- accumulated across runs. Keying on selection_mode instead lets a window hold
-- at most one clip PER sampling batch ('top' / 'random' / 'stratified' /
-- 'custom'), which stops within-batch pseudo-replication while still allowing a
-- window to appear once in each phase.
--
-- Safe to re-run. NOTE: step 2 DELETES redundant duplicate rows (keeping the
-- most complete one per window+mode).

-- 1. Backfill legacy NULLs so the column can be NOT NULL and the key is enforced
--    (a NULL in the key would re-enable the NULLs-distinct loophole).
UPDATE import.spectrograms SET selection_mode = 'custom' WHERE selection_mode IS NULL;

-- 2. Collapse existing duplicates per (window, selection_mode): keep the row
--    with an audio blob, else a linked result, else the lowest id.
DELETE FROM import.spectrograms s
USING (
  SELECT spectrogram_id,
         ROW_NUMBER() OVER (
           PARTITION BY audio_file_id, begin_time_ms, selection_mode
           ORDER BY (audio_data IS NOT NULL) DESC,
                    (result_id  IS NOT NULL) DESC,
                    spectrogram_id ASC
         ) AS rn
  FROM import.spectrograms
) d
WHERE s.spectrogram_id = d.spectrogram_id
  AND d.rn > 1;

-- 3. Enforce the column now that it is fully populated.
ALTER TABLE import.spectrograms ALTER COLUMN selection_mode SET DEFAULT 'custom';
ALTER TABLE import.spectrograms ALTER COLUMN selection_mode SET NOT NULL;

-- 4. Swap the constraint. Keep the required_annotation_type_id column, just
--    drop it from the key.
ALTER TABLE import.spectrograms
  DROP CONSTRAINT IF EXISTS spectrograms_unique_time_annotation;

DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint
    WHERE conname  = 'spectrograms_unique_window_mode'
      AND conrelid = 'import.spectrograms'::regclass
  ) THEN
    ALTER TABLE import.spectrograms
      ADD CONSTRAINT spectrograms_unique_window_mode
      UNIQUE (audio_file_id, begin_time_ms, selection_mode);
  END IF;
END $$;
