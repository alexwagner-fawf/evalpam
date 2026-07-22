-- Migration: Occupancy-Gruppen für Auto-Stopp im Binary-Modus
-- Safe to re-run.

CREATE TABLE IF NOT EXISTS import.occupancy_groups (
    group_id     BIGSERIAL PRIMARY KEY,
    project_id   INTEGER NOT NULL REFERENCES import.projects(project_id) ON DELETE CASCADE,
    group_name   VARCHAR(100) NOT NULL,
    description  TEXT,
    target_count SMALLINT NOT NULL CHECK (target_count > 0),
    created_at   timestamptz DEFAULT NOW(),
    CONSTRAINT occupancy_groups_unique_name UNIQUE (project_id, group_name)
);

CREATE TABLE IF NOT EXISTS import.spectrogram_groups (
    spectrogram_id BIGINT NOT NULL REFERENCES import.spectrograms(spectrogram_id) ON DELETE CASCADE,
    group_id       BIGINT NOT NULL REFERENCES import.occupancy_groups(group_id) ON DELETE CASCADE,
    PRIMARY KEY (spectrogram_id, group_id)
);

CREATE INDEX IF NOT EXISTS idx_spectrogram_groups_group_id
    ON import.spectrogram_groups (group_id);
