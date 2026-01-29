-- 0. EXTENSIONS (Zwingend notwendig für die Constraints!)
CREATE EXTENSION IF NOT EXISTS btree_gist;
CREATE EXTENSION IF NOT EXISTS postgis;

-- 1. PROJECTS
CREATE SEQUENCE IF NOT EXISTS import.projects_id_seq;
CREATE TABLE IF NOT EXISTS import.projects (
    project_id integer NOT NULL DEFAULT nextval('import.projects_id_seq'::regclass),
    project_name_short character varying(30),
    project_name_long character varying(150),
    description text,
    contact character varying(100),
    organisation character varying(100),
    created_at timestamptz DEFAULT NOW(),
    CONSTRAINT projects_pkey PRIMARY KEY (project_id)
);

-- 2. DEPLOYMENTS
CREATE SEQUENCE IF NOT EXISTS import.deployments_id_seq;
CREATE TABLE IF NOT EXISTS import.deployments (
    deployment_id bigint NOT NULL DEFAULT nextval('import.deployments_id_seq'::regclass),
    project_id integer NOT NULL,
    deployment_name character varying(99),
    deployment_path text,
    start_datetime timestamptz,
    end_datetime timestamptz,
    device_manufacturer character varying(99),
    device_modelname character varying(99),
    valid boolean NOT NULL,
    notes text,
    geometry geometry (Point,4326),
    created_at timestamptz DEFAULT NOW(),
    CONSTRAINT deployments_pkey PRIMARY KEY (deployment_id),
    CONSTRAINT deployments_unique_name UNIQUE (deployment_name),
    CONSTRAINT deployments_project_id_fkey FOREIGN KEY (project_id) REFERENCES import.projects (project_id)
);



-- 3. SETTINGS
CREATE SEQUENCE IF NOT EXISTS import.settings_id_seq;
CREATE TABLE IF NOT EXISTS import.settings (
    settings_id bigint NOT NULL DEFAULT nextval('import.settings_id_seq'::regclass),
    model_name varchar(99) NOT NULL,
    model_version varchar(50),
    min_conf double precision NOT NULL,
    overlap double precision NOT NULL,
    locale varchar(30),
    model_params jsonb NOT NULL,
    created_at timestamptz DEFAULT now(),
    CONSTRAINT settings_pkey PRIMARY KEY (settings_id)
);

-- 4. AUDIO FILES
CREATE SEQUENCE IF NOT EXISTS import.audio_file_id_seq;
CREATE TABLE IF NOT EXISTS import.audio_files (
    audio_file_id bigint NOT NULL DEFAULT nextval('import.audio_file_id_seq'::regclass),
    deployment_id bigint NOT NULL,
    sample_rate bigint NOT NULL,
    relative_path character varying(120),
    timestamp_start timestamptz NOT NULL,
    duration_s int NOT NULL,
    deleted boolean,
    required_annotation_type_id integer REFERENCES public.lut_annotation_type_code(annotation_type_id),
    created_at timestamptz DEFAULT NOW(),
    CONSTRAINT audio_files_pkey PRIMARY KEY (audio_file_id),
    CONSTRAINT audio_files_deployment_fkey FOREIGN KEY (deployment_id) REFERENCES import.deployments (deployment_id),

    -- >> NEUER SCHUTZ (UNIQUE) <<
    CONSTRAINT no_duplicate_start_time UNIQUE (deployment_id, timestamp_start)
);
CREATE INDEX ON import.audio_files (deployment_id);
-- 5. RESULTS
CREATE SEQUENCE IF NOT EXISTS import.results_data_id_seq;
CREATE TABLE IF NOT EXISTS import.results (
    result_id bigint NOT NULL DEFAULT nextval('import.results_data_id_seq'::regclass),
    audio_file_id bigint NOT NULL,
    settings_id bigint NOT NULL,
    begin_time_ms integer NOT NULL,
    end_time_ms integer NOT NULL,
    -- confidence will be converted by round(confidence * 1000) and back-converted by confidence / 1000
    confidence smallint,
    species_id integer NOT NULL,
    behavior_id integer,
    created_at timestamptz DEFAULT NOW(),
    CONSTRAINT results_unique_entry UNIQUE(audio_file_id, settings_id, begin_time_ms, end_time_ms, species_id),
    CONSTRAINT results_pkey PRIMARY KEY (result_id),
    CONSTRAINT results_settings_id_fkey FOREIGN KEY (settings_id) REFERENCES import.settings (settings_id),
    CONSTRAINT results_audio_file_id_fkey FOREIGN KEY (audio_file_id) REFERENCES import.audio_files (audio_file_id),
    CONSTRAINT results_species_id_fkey FOREIGN KEY (species_id) REFERENCES public.lut_species_code (species_id),
    CONSTRAINT results_behavior_id_fkey FOREIGN KEY (behavior_id) REFERENCES public.lut_behavior_code (behavior_id)
);
CREATE INDEX ON import.results (audio_file_id);
CREATE INDEX ON import.results (species_id);
CREATE UNIQUE INDEX ON import.results (audio_file_id, settings_id, begin_time_ms, end_time_ms, species_id);

-- 6. SPECTROGRAMS
CREATE SEQUENCE IF NOT EXISTS import.spectrogram_data_id_seq;
CREATE TABLE IF NOT EXISTS import.spectrograms (
    spectrogram_id bigint NOT NULL DEFAULT nextval('import.spectrogram_data_id_seq'::regclass),
    result_id bigint NOT NULL,
    buffer_s numeric(5,1) NOT NULL,
    duration_s int NOT NULL,
    resolution_x int NOT NULL,
    resolution_y int NOT NULL,
    freq_min int NOT NULL,
    freq_max int NOT NULL,

    -- >> NEU: Hier speichern wir den festen Modus für die Aufgabe (z.B. "Full Segmentation") <<
    required_annotation_type_id integer REFERENCES public.lut_annotation_type_code(annotation_type_id),

    created_at timestamptz DEFAULT NOW(),
    CONSTRAINT spectrograms_pkey PRIMARY KEY (spectrogram_id),
    CONSTRAINT spectrograms_result_id_fkey FOREIGN KEY (result_id) REFERENCES import.results (result_id)
);
CREATE INDEX ON import.spectrograms (result_id);

-- (Tabelle import.validations WURDE ENTFERNT)

-- 7. ANNOTATION STATUS (Das Logbuch)
CREATE SEQUENCE IF NOT EXISTS import.annotation_status_data_id_seq;
CREATE TABLE IF NOT EXISTS import.annotation_status (
    status_id bigint NOT NULL DEFAULT nextval('import.annotation_status_data_id_seq'::regclass),
    audio_file_id bigint NOT NULL,
    user_id bigint NOT NULL,
    begin_time_ms integer NOT NULL,
    end_time_ms integer NOT NULL,
    annotation_type_id integer NOT NULL,
    target_species_id integer,
    created_at timestamptz DEFAULT NOW(),

    CONSTRAINT annotation_status_pkey PRIMARY KEY (status_id),
    CONSTRAINT status_audio_file_fkey FOREIGN KEY (audio_file_id) REFERENCES import.audio_files (audio_file_id) ON DELETE CASCADE,
    CONSTRAINT annotation_status_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.app_users (user_id),
    CONSTRAINT annotation_status_annotation_type_id_fkey FOREIGN KEY (annotation_type_id) REFERENCES public.lut_annotation_type_code (annotation_type_id),
    CONSTRAINT annotation_status_target_species_id_fkey FOREIGN KEY (target_species_id) REFERENCES public.lut_species_code (species_id),

    -- Constraint: Zeit muss logisch sein
    CONSTRAINT check_valid_status_time CHECK (begin_time_ms < end_time_ms),

    -- >> STRENGER CONSTRAINT (Strict Locking) <<
    -- Keine Überlappung erlaubt, egal welcher User. Single Source of Truth.
    CONSTRAINT no_status_overlap_global EXCLUDE USING GIST (
        audio_file_id WITH =,
        int4range(begin_time_ms, end_time_ms) WITH &&
    )
);
CREATE INDEX ON import.annotation_status (audio_file_id);
CREATE INDEX ON import.annotation_status (user_id);

-- 8. GROUND TRUTH ANNOTATIONS (Die Wahrheit)
CREATE SEQUENCE IF NOT EXISTS import.ground_truth_annotations_data_id_seq;
CREATE TABLE IF NOT EXISTS import.ground_truth_annotations (
    annotation_id bigint NOT NULL DEFAULT nextval('import.ground_truth_annotations_data_id_seq'::regclass),
    audio_file_id bigint NOT NULL,
    user_id integer NOT NULL,
    species_id integer NOT NULL,
    begin_time_ms integer NOT NULL,
    end_time_ms integer NOT NULL,
    is_present boolean NOT NULL DEFAULT TRUE,
    behavior_id integer,
    created_at timestamptz DEFAULT NOW(),

    CONSTRAINT ground_truth_pkey PRIMARY KEY (annotation_id),
    CONSTRAINT ground_truth_audio_fkey FOREIGN KEY (audio_file_id) REFERENCES import.audio_files (audio_file_id) ON DELETE CASCADE,
    CONSTRAINT ground_truth_user_fkey FOREIGN KEY (user_id) REFERENCES public.app_users (user_id),
    CONSTRAINT ground_truth_species_fkey FOREIGN KEY (species_id) REFERENCES public.lut_species_code (species_id),
    CONSTRAINT ground_truth_behavior_fkey FOREIGN KEY (behavior_id) REFERENCES public.lut_behavior_code (behavior_id),

    -- Constraint: Zeit muss positiv sein
    CONSTRAINT check_valid_gt_time CHECK (begin_time_ms < end_time_ms),

    -- >> STRENGER CONSTRAINT (Strict Locking) <<
    -- Verhindert doppelte Arten zur gleichen Zeit in der gleichen Datei.
    -- (Egal von welchem User -> Single Source of Truth)
    CONSTRAINT no_duplicate_species_overlap_global EXCLUDE USING GIST (
        audio_file_id WITH =,
        species_id WITH =,
        int4range(begin_time_ms, end_time_ms) WITH &&
    )
);
CREATE INDEX ON import.ground_truth_annotations (audio_file_id);
CREATE INDEX ON import.ground_truth_annotations (species_id);
