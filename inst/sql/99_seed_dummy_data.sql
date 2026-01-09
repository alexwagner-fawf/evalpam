-- 0. Alles leeren und Zähler zurücksetzen
TRUNCATE TABLE public.lut_species_code RESTART IDENTITY CASCADE;
TRUNCATE TABLE public.lut_behavior_code RESTART IDENTITY CASCADE;
TRUNCATE TABLE public.lut_annotation_type_code RESTART IDENTITY CASCADE; -- NEU: Auch diese Tabelle leeren
TRUNCATE TABLE public.app_users RESTART IDENTITY CASCADE;
TRUNCATE TABLE import.projects RESTART IDENTITY CASCADE;
TRUNCATE TABLE import.settings RESTART IDENTITY CASCADE;

-- 1. Lookups erstellen

-- A) Spezies
INSERT INTO public.lut_species_code (species_id, species_short, species_scientific, species_long_de, species_long_en) VALUES
(1, 'TURMER', 'Turdus merula', 'Amsel', 'Common Blackbird'),
(2, 'PARMAJ', 'Parus major', 'Kohlmeise', 'Great Tit'),
(3, 'FRAFRA', 'Fringilla coelebs', 'Buchfink', 'Common Chaffinch');

-- B) Verhalten
INSERT INTO public.lut_behavior_code (behavior_id, behavior_short, behavior_long_de, behavior_long_en) VALUES
(1, 'song', 'Gesang', 'Song'),
(2, 'call', 'Ruf', 'Call');

-- C) Annotationstypen (MIT DESCRIPTION)
INSERT INTO public.lut_annotation_type_code
(annotation_type_id, annotation_type_short, annotation_type_long_de, annotation_type_long_en, annotation_type_description)
VALUES
(1, 'bbox', 'Bounding Box', 'Bounding Box', 'Zeichnet einen Kasten um den Ruf'),
(2, 'time', 'Zeitbereich', 'Time Selection', 'Markiert nur den Zeitbereich');

-- 2. App User (Passwort Hash für 'test')
INSERT INTO public.app_users (user_id, username, password_hash, first_name, last_name, email) VALUES
(1, 'admin', '$2a$12$lT6PqSFRzUJMJbwRGD9WmuvM/J2k2rW8RFpUIWnxDimR3uKAJr/jm', 'Chef', 'Admin', 'admin@example.com'),
(2, 'birder', '$2a$12$Cf1.j/u/M1R.sE5yWjQ...HASH_VON_BCRYPT...', 'Hans', 'Gucker', 'hans@example.com')
ON CONFLICT (username) DO NOTHING;

-- 3. Projekte
INSERT INTO import.projects (project_id, project_name_short, project_name_long) VALUES
(1, 'CITY_24', 'Stadtmonitoring 2024'),
(2, 'FOREST_24', 'Waldnaturschutz 2024');

-- 4. User zu Projekten & Rollen
INSERT INTO public.project_users (project_id, user_id) VALUES
(1, 1),
(2, 1),
(1, 2)
ON CONFLICT DO NOTHING;

INSERT INTO public.app_user_roles (user_id, pg_role) VALUES
(1, 'admin'),
(2, 'birder')
ON CONFLICT DO NOTHING;

-- 5. Deployments
INSERT INTO import.deployments (deployment_id, project_id, deployment_name, valid) VALUES
(1, 1, 'Park_Sued', TRUE);

-- 6. Audio Files (MIT NEUER SPALTE)
-- Wir setzen required_annotation_type_id auf 1 (Bounding Box)
INSERT INTO import.audio_files (
    audio_file_id,
    deployment_id,
    sample_rate,
    relative_path,
    duration_s,
    timestamp_start,
    required_annotation_type_id
) VALUES
(1, 1, 48000, 'test_audio_1.mp4', 60, NOW(), 1),
(2, 1, 48000, 'test_audio_2.mp4', 60, NOW() + interval '1 hour', 1);

-- 7. Settings
INSERT INTO import.settings (settings_id, model_name, min_conf, overlap, model_params) VALUES
(1, 'BirdNET_v2.4', 0.5, 0.0, '{{}}');

-- 8. Predictions (Results)
INSERT INTO import.results (audio_file_id, settings_id, begin_time_s, end_time_s, confidence, species_id) VALUES
(1, 1, 0.0, 3.0, 0.95, 1),
(1, 1, 10.0, 13.0, 0.65, 2);
