-- Migration: update import.ground_truth_annotations
--   1. make species_id nullable (was NOT NULL)
--   2. add abiotic_sound_id column with FK to lut_abiotic_sound_code
-- Safe to re-run: DROP NOT NULL on already-nullable column is a no-op in PG;
--                 ADD COLUMN IF NOT EXISTS and the DO-block check guard the rest.
--
-- Run 33_add_lut_abiotic_sound_code.sql before this file.

ALTER TABLE import.ground_truth_annotations
    ALTER COLUMN species_id DROP NOT NULL;

ALTER TABLE import.ground_truth_annotations
    ADD COLUMN IF NOT EXISTS abiotic_sound_id smallint;

DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1
        FROM   pg_constraint
        WHERE  conname    = 'ground_truth_abiotic_sound_fkey'
          AND  conrelid   = 'import.ground_truth_annotations'::regclass
    ) THEN
        ALTER TABLE import.ground_truth_annotations
            ADD CONSTRAINT ground_truth_abiotic_sound_fkey
                FOREIGN KEY (abiotic_sound_id)
                REFERENCES public.lut_abiotic_sound_code (abiotic_sound_id);
    END IF;
END;
$$;
