-- 0. Alles leeren und Zähler zurücksetzen
TRUNCATE TABLE public.lut_species_code RESTART IDENTITY CASCADE;
TRUNCATE TABLE public.lut_behavior_code RESTART IDENTITY CASCADE;
TRUNCATE TABLE public.app_users RESTART IDENTITY CASCADE;
TRUNCATE TABLE import.projects RESTART IDENTITY CASCADE;
TRUNCATE TABLE import.settings RESTART IDENTITY CASCADE;

-- 1. Lookups mit expliziten IDs
INSERT INTO public.lut_species_code (species_id, species_short, species_long_de, species_long_en) VALUES
(1, 'TURMER', 'Amsel', 'Common Blackbird'),
(2, 'PARMAJ', 'Kohlmeise', 'Great Tit'),
(3, 'FRAFRA', 'Buchfink', 'Common Chaffinch');

INSERT INTO public.lut_behavior_code (behavior_id, behavior_short, behavior_long_de, behavior_long_en) VALUES
(1, 'song', 'Gesang', 'Song'),
(2, 'call', 'Ruf', 'Call');

-- 2. App User (Passwort Hash für 'test')
INSERT INTO public.app_users (user_id, username, password_hash, first_name, last_name, email) VALUES
(1, 'admin', '$2a$12$lT6PqSFRzUJMJbwRGD9WmuvM/J2k2rW8RFpUIWnxDimR3uKAJr/jm', 'Chef', 'Admin', 'admin@example.com'),
(2, 'birder', '$2a$12$Cf1.j/u/M1R.sE5yWjQ...HASH_VON_BCRYPT...', 'Hans', 'Gucker', 'hans@example.com')
ON CONFLICT (username) DO NOTHING;

-- 3. Projekte mit expliziter ID 1
INSERT INTO import.projects (project_id, project_name_short, project_name_long) VALUES
(1, 'CITY_24', 'Stadtmonitoring 2024'),
(2, 'FOREST_24', 'Waldnaturschutz 2024');

-- 4. User zu Projekten UND Rollen zuweisen

-- A) User zu Projekten (wie gehabt)
INSERT INTO public.project_users (project_id, user_id) VALUES
(1, 1), -- Admin in City
(2, 1), -- Admin in Forest
(1, 2)  -- Birder in City
ON CONFLICT DO NOTHING;

-- B) Rollen zuweisen (HIER WAR DER FEHLER)
-- Wir schreiben den Text-Namen der Rolle hinein, keine ID.
-- Spaltenname ist 'pg_role', nicht 'role_id'.
INSERT INTO public.app_user_roles (user_id, pg_role) VALUES
(1, 'admin'),  -- User 1 bekommt die Rolle "admin" (oder "evalpam_admin", je nachdem was die App erwartet)
(2, 'birder')  -- User 2 bekommt die Rolle "birder"
ON CONFLICT DO NOTHING;

-- 5. Deployments & Audio Files
-- Deployment ID 1 explizit setzen
INSERT INTO import.deployments (deployment_id, project_id, deployment_name, valid) VALUES
(1, 1, 'Park_Sued', TRUE);

-- Audio File ID 1 setzen
INSERT INTO import.audio_files (audio_file_id, deployment_id, sample_rate, relative_path, duration_s) VALUES
(1, 1, 48000, 'test_audio_1.mp4', 60),
(2, 1, 48000, 'test_audio_2.mp4', 60);

-- 6. Settings
-- Das löst das JSON Problem UND das glue Problem.
INSERT INTO import.settings (settings_id, model_name, min_conf, overlap, model_params) VALUES
(1, 'BirdNET_v2.4', 0.5, 0.0, '{{}}');

-- 7. Predictions (Results)
-- Hier verweisen wir explizit auf audio_file_id=1, settings_id=1, species_id=1
INSERT INTO import.results (audio_file_id, settings_id, begin_time_s, end_time_s, confidence, species_id) VALUES
(1, 1, 0.0, 3.0, 0.95, 1),
(1, 1, 10.0, 13.0, 0.65, 2);
