-- 0. Alles leeren und Zähler zurücksetzen
-- TRUNCATE TABLE public.lut_species_code RESTART IDENTITY CASCADE;
-- TRUNCATE TABLE public.lut_behavior_code RESTART IDENTITY CASCADE;
-- TRUNCATE TABLE public.lut_annotation_type_code RESTART IDENTITY CASCADE;


TRUNCATE TABLE import.results RESTART IDENTITY CASCADE;
ALTER SEQUENCE import.results_data_id_seq RESTART WITH 1;

TRUNCATE TABLE import.audio_files RESTART IDENTITY CASCADE;
ALTER SEQUENCE import.audio_file_id_seq RESTART WITH 1;

TRUNCATE TABLE import.deployments RESTART IDENTITY CASCADE;
ALTER SEQUENCE import.deployments_id_seq RESTART WITH 1;

TRUNCATE TABLE import.settings_species RESTART IDENTITY CASCADE;

ALTER SEQUENCE import.settings_id_seq RESTART WITH 1;
TRUNCATE TABLE import.settings RESTART IDENTITY CASCADE;

TRUNCATE TABLE public.project_users RESTART IDENTITY CASCADE;

TRUNCATE TABLE import.projects RESTART IDENTITY CASCADE;
ALTER SEQUENCE import.projects_id_seq RESTART WITH 1;

TRUNCATE TABLE public.app_users RESTART IDENTITY CASCADE;
ALTER SEQUENCE app_users_user_id_seq RESTART WITH 1;

-- settings_species wird durch CASCADE bei settings automatisch geleert, aber sicherheitshalber:


-- 1. Lookups erstellen (removed lookups as they are included by default)

-- lookup tables now included in database setup - dummy values no longer needed

-- 2. App User
INSERT INTO public.app_users (username, password_hash, first_name, last_name, email) VALUES
('admin', '$2a$12$lT6PqSFRzUJMJbwRGD9WmuvM/J2k2rW8RFpUIWnxDimR3uKAJr/jm', 'Chef', 'Admin', 'admin@example.com'),
('birder', '$2a$12$Cf1.j/u/M1R.sE5yWjQ...HASH...', 'Hans', 'Gucker', 'hans@example.com')
ON CONFLICT (username) DO NOTHING;

-- 3. Projekte
INSERT INTO import.projects (project_name_short, project_name_long) VALUES
('CITY_24', 'Stadtmonitoring 2024'),
('FOREST_24', 'Waldnaturschutz 2024');

-- 4. User zu Projekten
INSERT INTO public.project_users (project_id, user_id) VALUES (1, 1), (2, 1), (1, 2) ON CONFLICT DO NOTHING;
INSERT INTO public.app_user_roles (user_id, pg_role) VALUES (1, 'admin'), (2, 'birder') ON CONFLICT DO NOTHING;

-- 5. Deployments
INSERT INTO import.deployments (project_id, deployment_name, valid) VALUES
(1, 'Park_Sued', TRUE);

-- 6. Audio Files (JETZT 3 STÜCK)
INSERT INTO import.audio_files (deployment_id, sample_rate, relative_path, duration_s, timestamp_start, required_annotation_type_id) VALUES
(1, 48000, 'test_audio_1.mp4', 60, NOW(), 1),
(1, 48000, 'test_audio_2.mp4', 60, NOW() + interval '1 hour', 1),
(1, 48000, 'test_audio_3.mp4', 60, NOW() + interval '2 hours', 1);

-- 7. Settings
INSERT INTO import.settings (model_name, min_conf, overlap, model_params) VALUES
('BirdNET_v2.4', 0.5, 0.0, '{"dummy_parameter": 123}'::jsonb);

-- >>> NEU: SETTINGS SPECIES WHITELIST <<<
-- Wir erlauben für Setting 1 nur Amsel (1) und Kohlmeise (2).
-- Der Buchfink (3) wird im Dropdown NICHT erscheinen, obwohl er in der Datenbank existiert.
INSERT INTO import.settings_species (settings_id, species_id) VALUES
(1, 6254), -- Amsel erlaubt
(1, 4238); -- Kohlmeise erlaubt

-- 8. Predictions (Results)
-- Wir fügen auch eine Prediction für Buchfink hinzu, um zu sehen, ob das System robust ist,
-- auch wenn die Art eigentlich durch den Filter fallen würde (Edge Case).
INSERT INTO import.results (audio_file_id, settings_id, begin_time_ms, end_time_ms, confidence, species_id) VALUES
(1, 1, 0, 3000, 9500, 6254),   -- Amsel (Erlaubt)
(2, 1, 10000, 13000, 6500, 4238), -- Kohlmeise (Erlaubt)
(3, 1, 5000, 8000, 8800, 2371);   -- Buchfink (Nicht auf Whitelist -> Testfall!)
