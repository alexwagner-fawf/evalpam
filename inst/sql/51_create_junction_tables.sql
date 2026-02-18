-- Junction table for many-to-many relationship between users and projects
CREATE TABLE IF NOT EXISTS public.project_users
(
    project_id integer NOT NULL,
    user_id    bigint NOT NULL,
    created_at timestamptz DEFAULT now(),
    CONSTRAINT project_users_pkey PRIMARY KEY (project_id, user_id),
    CONSTRAINT project_users_project_id_fkey FOREIGN KEY (project_id)
        REFERENCES import.projects (project_id)
        ON UPDATE CASCADE
        ON DELETE CASCADE,
    CONSTRAINT project_users_user_id_fkey FOREIGN KEY (user_id)
        REFERENCES app_users (user_id)
        ON UPDATE CASCADE
        ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_project_users_user_id
    ON public.project_users (user_id);


-- Junction table for many-to-many relationship between settings and species
CREATE TABLE IF NOT EXISTS import.settings_species
(
    settings_id bigint NOT NULL,
    species_id  integer NOT NULL,
    CONSTRAINT settings_species_pkey PRIMARY KEY (settings_id, species_id),
    CONSTRAINT settings_species_settings_id_fkey FOREIGN KEY (settings_id)
        REFERENCES import.settings (settings_id)
        ON UPDATE CASCADE
        ON DELETE CASCADE,
    CONSTRAINT settings_species_species_id_fkey FOREIGN KEY (species_id)
        REFERENCES public.lut_species_code (species_id)
        ON UPDATE CASCADE
        ON DELETE RESTRICT
);

CREATE INDEX IF NOT EXISTS idx_settings_species_species_id
    ON import.settings_species (species_id);



-- 9. ANALYSIS LOG (which audio files have been processed)
CREATE TYPE import.analysis_status AS ENUM (
    'success',
    'failed_file_not_found',
    'failed_permission_denied',
    'failed_empty_file',
    'failed_corrupt_file',
    'failed_transient_io_error',
    'failed_birdnet_inference_error',
    'unknown',
    'other'
);

CREATE TABLE import.analysis_log (
    audio_file_id bigint NOT NULL REFERENCES import.audio_files(audio_file_id),
    settings_id bigint NOT NULL REFERENCES import.settings(settings_id),
    analysed_at timestamptz DEFAULT NOW(),
    status import.analysis_status NOT NULL DEFAULT 'success',
    UNIQUE (audio_file_id, settings_id)
);

-- index to quickly get status of failed analyses
CREATE INDEX idx_analysis_log_audio_not_success
ON import.analysis_log (audio_file_id)
WHERE status != 'success';
