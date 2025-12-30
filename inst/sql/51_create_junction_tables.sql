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

-- Optional: index on species_id for reverse lookups
CREATE INDEX IF NOT EXISTS idx_settings_species_species_id
    ON import.settings_species (species_id);
