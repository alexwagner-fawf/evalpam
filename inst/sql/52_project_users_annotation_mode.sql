-- Migration: ensure public.project_users has the annotation_mode column.
-- Safe to re-run.
--
-- 51_create_junction_tables.sql defines project_users WITH annotation_mode, but
-- via CREATE TABLE IF NOT EXISTS -- so databases whose project_users predates
-- that column never gained it (the IF NOT EXISTS skips the whole table). The
-- app and the dummy seed both write annotation_mode ('full' vs 'binary'), so add
-- it here for drifted databases. On a fresh database this is a no-op.

ALTER TABLE public.project_users
  ADD COLUMN IF NOT EXISTS annotation_mode varchar(10) NOT NULL DEFAULT 'full';

DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint
    WHERE conname  = 'check_annotation_mode'
      AND conrelid = 'public.project_users'::regclass
  ) THEN
    ALTER TABLE public.project_users
      ADD CONSTRAINT check_annotation_mode
      CHECK (annotation_mode IN ('full', 'binary'));
  END IF;
END $$;
