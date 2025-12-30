
-- Table: import.deployments

-- DROP TABLE IF EXISTS import.deployments;

CREATE TABLE IF NOT EXISTS import.deployments
(
    deployment_id character varying(8) COLLATE pg_catalog."default" NOT NULL,
    deployment_name character varying(99) COLLATE pg_catalog."default",
    deployment_path text COLLATE pg_catalog."default",
    start_datetime timestamptz,
    end_datetime timestamptz,
    device_manufacturer character varying(99),
    device_modelname character varying(99),
    geometry geometry (Point,4326),
    created_at timestamptz DEFAULT NOW(),
    CONSTRAINT deployments_pkey PRIMARY KEY (deployment_id)
);


-- Table: import.settings

-- DROP TABLE IF EXISTS import.settings;
CREATE SEQUENCE IF NOT EXISTS import.settings_id_seq;
CREATE TABLE IF NOT EXISTS import.settings
(
    settings_id   bigint NOT NULL DEFAULT nextval('import.settings_id_seq'::regclass),
    model_name    varchar(99) NOT NULL,
    model_version varchar(50),

    -- stable parameters
    min_conf      double precision NOT NULL,
    overlap       double precision NOT NULL,
    locale        varchar(30),

    -- model-specific parameters
    model_params  jsonb NOT NULL,

    created_at    timestamptz DEFAULT now(),
   CONSTRAINT settings_pkey PRIMARY KEY (settings_id)
);



-- Table: import.audio_files
-- DROP TABLE IF EXISTS import.audio_files;
CREATE SEQUENCE IF NOT EXISTS import.audio_file_id_seq;
CREATE TABLE IF NOT EXISTS import.audio_files
(
    audio_file_id bigint NOT NULL DEFAULT nextval('import.audio_file_id_seq'::regclass),
    deployment_id character varying(8) NOT NULL COLLATE pg_catalog."default",
    sample_rate bigint NOT NULL,
    relative_path character varying(120) COLLATE pg_catalog."default",
    timestamp_start timestamptz,
    duration_s int,
    created_at timestamptz DEFAULT NOW(),
    CONSTRAINT audio_files_pkey PRIMARY KEY (audio_file_id),
    CONSTRAINT audio_files_deployment_id_fkey FOREIGN KEY (deployment_id)
        REFERENCES import.deployments (deployment_id) MATCH SIMPLE
        ON UPDATE NO ACTION
        ON DELETE NO ACTION
        NOT VALID
);
CREATE INDEX ON import.audio_files (deployment_id);

-- Table: import.results
-- DROP TABLE IF EXISTS import.results;


CREATE SEQUENCE IF NOT EXISTS import.results_data_id_seq;
CREATE TABLE IF NOT EXISTS import.results
(

    result_id bigint NOT NULL DEFAULT nextval('import.results_data_id_seq'::regclass),
    audio_file_id bigint NOT NULL,
    settings_id bigint NOT NULL,
    begin_time_s numeric(6,1),
    end_time_s numeric(6,1),
    confidence double precision,
    species_id integer NOT NULL,
    behavior_id int,
    created_at timestamptz DEFAULT NOW(),
    CONSTRAINT results_pkey PRIMARY KEY (result_id),
    CONSTRAINT results_settings_id_fkey FOREIGN KEY (settings_id)
        REFERENCES import.settings (settings_id) MATCH SIMPLE
        ON UPDATE NO ACTION
        ON DELETE NO ACTION
        NOT VALID,
   CONSTRAINT results_audio_file_id_fkey FOREIGN KEY (audio_file_id)
        REFERENCES import.audio_files (audio_file_id) MATCH SIMPLE
        ON UPDATE NO ACTION
        ON DELETE NO ACTION
        NOT VALID,
    CONSTRAINT results_species_id_fkey FOREIGN KEY (species_id)
        REFERENCES public.lut_species_code (species_id) MATCH SIMPLE
        ON UPDATE NO ACTION
        ON DELETE NO ACTION
        NOT VALID,
    CONSTRAINT results_behavior_id_fkey FOREIGN KEY (behavior_id)
        REFERENCES public.lut_behavior_code (behavior_id) MATCH SIMPLE
        ON UPDATE NO ACTION
        ON DELETE NO ACTION
        NOT VALID
);
CREATE INDEX ON import.results (audio_file_id);
CREATE INDEX ON import.results (settings_id);
CREATE INDEX ON import.results (species_id);



-- Table: import.spectrograms
-- DROP TABLE IF EXISTS import.spectrograms;

CREATE SEQUENCE IF NOT EXISTS import.spectrogram_data_id_seq;
CREATE TABLE IF NOT EXISTS import.spectrograms
(
    spectrogram_id bigint NOT NULL DEFAULT nextval('import.spectrogram_data_id_seq'::regclass),
    result_id bigint NOT NULL,
    buffer_s numeric(5,1) NOT NULL,
    duration_s int NOT NULL,
    resolution_x int NOT NULL,
    resolution_y int NOT NULL,
    freq_min int NOT NULL,
    freq_max int NOT NULL,
    created_at timestamptz DEFAULT NOW(),
   CONSTRAINT spectrograms_pkey PRIMARY KEY (spectrogram_id),
   CONSTRAINT spectrograms_result_id_fkey FOREIGN KEY (result_id)
        REFERENCES import.results (result_id) MATCH SIMPLE
        ON UPDATE NO ACTION
        ON DELETE NO ACTION
        NOT VALID
);
CREATE INDEX ON import.spectrograms (result_id);

-- Table: import.validations
-- DROP TABLE IF EXISTS import.validations;

CREATE SEQUENCE IF NOT EXISTS import.validation_data_id_seq;
CREATE TABLE IF NOT EXISTS import.validations
(
    validation_id bigint NOT NULL DEFAULT nextval('import.validation_data_id_seq'::regclass),
    spectrogram_id bigint NOT NULL,
    user_id int NOT NULL,
    species_id int NOT NULL,
    behavior_id int,
    created_at timestamptz DEFAULT NOW(),
   CONSTRAINT validations_pkey PRIMARY KEY (validation_id),
   CONSTRAINT validations_spectrogram_id_fkey FOREIGN KEY (spectrogram_id)
        REFERENCES import.spectrograms (spectrogram_id) MATCH SIMPLE
        ON UPDATE NO ACTION
        ON DELETE NO ACTION
        NOT VALID,
    CONSTRAINT validations_species_id_fkey FOREIGN KEY (species_id)
        REFERENCES public.lut_species_code (species_id) MATCH SIMPLE
        ON UPDATE NO ACTION
        ON DELETE NO ACTION
        NOT VALID,
   CONSTRAINT validations_user_id_fkey FOREIGN KEY (user_id)
        REFERENCES public.app_users (user_id) MATCH SIMPLE
        ON UPDATE NO ACTION
        ON DELETE NO ACTION
        NOT VALID,
    CONSTRAINT validations_behavior_id_fkey FOREIGN KEY (behavior_id)
        REFERENCES public.lut_behavior_code (behavior_id) MATCH SIMPLE
        ON UPDATE NO ACTION
        ON DELETE NO ACTION
        NOT VALID
);
CREATE INDEX ON import.validations (spectrogram_id);
CREATE INDEX ON import.validations (user_id);



-- DROP TABLE IF EXISTS import.annotation_status;

CREATE SEQUENCE IF NOT EXISTS import.annotation_status_data_id_seq;
CREATE TABLE IF NOT EXISTS import.annotation_status
(
    status_id bigint NOT NULL DEFAULT nextval('import.annotation_status_data_id_seq'::regclass),
    audio_file_id bigint NOT NULL,
    user_id bigint NOT NULL,

    -- ZEITFENSTER: Welchen Teil der Datei deckt dieser Status ab?
    begin_time_s numeric(6,1) NOT NULL,
    end_time_s numeric(6,1) NOT NULL,

    -- DER MODUS: Was wurde in dieser Zeit gemacht?
    -- 'full_segmentation': Alles wurde exakt mit Start/Ende Boxen markiert.
    -- 'target_segmentation': Nur eine bestimmte Art wurde exakt markiert.
    -- 'weak_presence': Nur grob "Ja/Nein" für den Abschnitt (keine exakten Boxen).
    -- 'negative_check': Bestätigt, dass hier NICHTS ist (explizit leer).
    annotation_type_id integer NOT NULL,
    -- Wenn target_segmentation, welche Art?
    target_species_id integer,

    created_at timestamptz DEFAULT NOW(),
   CONSTRAINT annotation_status_pkey PRIMARY KEY (status_id),
   CONSTRAINT status_audio_file_fkey FOREIGN KEY (audio_file_id)
        REFERENCES import.audio_files (audio_file_id) ON DELETE CASCADE,
   CONSTRAINT annotation_status_annotation_type_id_fkey FOREIGN KEY (annotation_type_id)
        REFERENCES public.lut_annotation_type_code (annotation_type_id) MATCH SIMPLE
        ON UPDATE NO ACTION
        ON DELETE NO ACTION
        NOT VALID,
   CONSTRAINT annotation_status_user_id_fkey FOREIGN KEY (user_id)
        REFERENCES public.app_users (user_id) MATCH SIMPLE
        ON UPDATE NO ACTION
        ON DELETE NO ACTION
        NOT VALID,
    CONSTRAINT annotation_status_target_species_id_fkey FOREIGN KEY (target_species_id)
        REFERENCES public.lut_species_code (species_id) MATCH SIMPLE
        ON UPDATE NO ACTION
        ON DELETE NO ACTION
        NOT VALID
);
CREATE INDEX ON import.annotation_status (audio_file_id);
CREATE INDEX ON import.annotation_status (user_id);

CREATE SEQUENCE IF NOT EXISTS import.ground_truth_annotations_data_id_seq;
CREATE TABLE IF NOT EXISTS import.ground_truth_annotations
(
    annotation_id bigint NOT NULL DEFAULT nextval('import.ground_truth_annotations_data_id_seq'::regclass),
    audio_file_id bigint NOT NULL,
    user_id integer NOT NULL,
    species_id integer NOT NULL,
    begin_time_s numeric(6,1) NOT NULL,
    end_time_s numeric(6,1) NOT NULL,
    -- NEU: Ist die Art anwesend?
    -- TRUE = Ja, hier singt sie.
    -- FALSE = Nein, hier singt sie NICHT (Explizites "Hard Negative", z.B. um False Positives zu korrigieren)
    is_present boolean NOT NULL DEFAULT TRUE,
    behavior_id integer,
    created_at timestamptz DEFAULT NOW(),

    CONSTRAINT ground_truth_pkey PRIMARY KEY (annotation_id),
    CONSTRAINT ground_truth_audio_fkey FOREIGN KEY (audio_file_id)
        REFERENCES import.audio_files (audio_file_id) ON DELETE CASCADE,
    CONSTRAINT ground_truth_user_id_fkey FOREIGN KEY (user_id)
        REFERENCES public.app_users (user_id) MATCH SIMPLE
        ON UPDATE NO ACTION
        ON DELETE NO ACTION
        NOT VALID,
    CONSTRAINT ground_truth_species_id_fkey FOREIGN KEY (species_id)
        REFERENCES public.lut_species_code (species_id) MATCH SIMPLE
        ON UPDATE NO ACTION
        ON DELETE NO ACTION
        NOT VALID,
    CONSTRAINT ground_truth_behavior_id_fkey FOREIGN KEY (behavior_id)
        REFERENCES public.lut_behavior_code (behavior_id) MATCH SIMPLE
        ON UPDATE NO ACTION
        ON DELETE NO ACTION
        NOT VALID
);
CREATE INDEX ON import.ground_truth_annotations (audio_file_id);
CREATE INDEX ON import.ground_truth_annotations (species_id);
