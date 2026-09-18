-- ============================================================================
-- 99_seed_dummy_data.sql — comprehensive dummy dataset for development/testing
-- ============================================================================
-- Populates EVERY data table so the app can be exercised end-to-end without a
-- real ingest run. Loaded by setup_app(dummy = TRUE) and by the dev-DB builder
-- inst/dev/setup_dev_db.R. Reference/lookup tables (lut_*) are NOT touched here:
-- they are seeded by 31/32/33 and treated as constant reference data.
--
-- Conventions used below (must match the running app):
--   * confidence is stored as smallint = round(raw_confidence * 10000)
--     (the 0..1 score slider multiplies by 10000 before comparing).
--   * a spectrogram is matched to its detection by
--     (audio_file_id, begin_time_ms) — these MUST be equal, or the clip never
--     appears in the queue (see project_data()/clip_detail() in app_server.R).
--   * app_users passwords are bcrypt hashes -- in this seed each user password
--     equals the username (admin/admin, birder1/birder1, birder2/birder2).
--   * spectrograms.audio_data is left NULL here (keeps this file small and
--     git-friendly) -- the dev builder injects playable MP3 blobs at build time.
--   * three projects: project 1 = CITY_24 (full-segmentation mode),
--     project 2 = FOREST_24 and project 3 = WETLAND_24 (binary presence mode
--     with occupancy auto-stop).
--   * species used are reference rows: 6254 Amsel / Turdus merula,
--     4238 Kohlmeise / Parus major, 2371 Buchfink / Fringilla coelebs.
--
-- Idempotent: wipes all data tables first, then reinserts with explicit ids and
-- resets every sequence at the end, so the app's own inserts continue cleanly.
-- Brace-free (uses jsonb_build_object, never a JSON string literal) so it runs
-- unchanged through psql, DBI immediate = TRUE, and the glue path in setup_db().
-- IMPORTANT: comments here must not contain a semicolon or curly braces --
-- setup_db() splits on every semicolon and pipes each chunk through glue_sql().
--
-- ---------------------------------------------------------------------------
-- OCCUPANCY / AUTO-STOP TEST SCENARIOS
-- ---------------------------------------------------------------------------
-- A "hit" for a group needs BOTH an annotation_status row (target_species_id =
-- species) AND a ground_truth row for that species with is_present = TRUE and
-- certainty_id = 1 (certain). A group auto-stops when distinct hits reach its
-- target_count, after which its remaining clips drop out of the queue (your own
-- annotated clips stay visible for review). Occupancy stop-state is GLOBAL (it
-- counts confirmed presences from any user), so each annotator gets their own
-- binary project for a clean, independent playground:
--
--   birder1 -> project 2 FOREST_24  (deployments Forest_A / Forest_B)
--   admin   -> project 3 WETLAND_24 (deployments Marsh_A  / Marsh_B)
--
-- Both projects carry the SAME five scenarios (target species Amsel unless
-- noted). For FOREST_24 the pre-annotations are by birder1, for WETLAND_24 by
-- admin -- so log in as the matching user:
--   * Group  Amsel @ <site A>  -- target 3, 5 clips, 2 pre-confirmed
--              -> ONE AWAY: confirm one more Amsel clip and watch it auto-stop,
--                 the 2 unworked clips vanish from the queue.
--   * Group  Kohlmeise @ <site A> -- target 2, 3 clips, 0 valid hits
--              -> only ABSENT and UNCERTAIN annotations exist, so it must NOT be
--                 counted done (tests is_present / certainty filtering).
--   * Group  Amsel @ <site B>  -- target 2, 4 clips, 2 pre-confirmed
--              -> ALREADY DONE: its 2 unworked clips are hidden from the queue
--                 from the start, the 2 confirmed clips remain for review.
--   * Group  Buchfink @ <site B> -- target 2, 3 clips, 1 pre-confirmed
--              -> ONE AWAY for Buchfink (switch target species to Buchfink).
--   * Group  Kohlmeise @ <site B> -- target 2, 2 clips, 0 hits -> FRESH.
-- DEPLOYMENT-FILTER TEST: every species appears in BOTH deployments of each
-- project, so with a species selected, toggling the Deployment queue-filter
-- changes which clips show.
-- ============================================================================

-- 0. Wipe all data tables (lut_* reference data is preserved) ------------------
TRUNCATE
  import.spectrogram_groups,
  import.occupancy_groups,
  import.ground_truth_annotations,
  import.annotation_status,
  import.analysis_log,
  import.spectrograms,
  import.results,
  import.settings_species,
  import.settings,
  import.audio_files,
  import.deployments,
  import.projects,
  public.project_users,
  public.app_user_roles,
  public.app_users
RESTART IDENTITY CASCADE;

-- 1. App users (password == username, bcrypt) ---------------------------------
INSERT INTO public.app_users (user_id, username, password_hash, first_name, last_name, email) VALUES
(1, 'admin',   '$2a$12$LSNjQqwa1XL0zeavJQUWxeW.z8fSXLgUONveD0k0uDnDH2E.MtBxq', 'Ada',  'Admin',   'admin@example.com'),
(2, 'birder1', '$2a$12$z0w2HT9wF2aN94swYb2uz.j1vFYs.JxifB3moOQjK/dfckM1nyKFG', 'Bea',  'Birder',  'birder1@example.com'),
(3, 'birder2', '$2a$12$VWZx/mZZRO.HIzu.01p2rOsQtCoe09Q7A/KX8WORtlAkdm0TGLp0S', 'Ben',  'Watcher', 'birder2@example.com');

INSERT INTO public.app_user_roles (user_id, pg_role) VALUES
(1, 'admin'),
(2, 'birder'),
(3, 'birder');

-- 2. Projects (1 = full mode, 2 & 3 = binary/occupancy mode) -------------------
INSERT INTO import.projects (project_id, project_name_short, project_name_long, description, contact, organisation) VALUES
(1, 'CITY_24',    'Stadtmonitoring 2024', 'Full-segmentation demo project (urban parks).',    'Ada Admin', 'Demo Org'),
(2, 'FOREST_24',  'Waldnaturschutz 2024', 'Binary presence + occupancy playground (birder1).','Ada Admin', 'Demo Org'),
(3, 'WETLAND_24', 'Feuchtgebiete 2024',   'Binary presence + occupancy playground (admin).',  'Ada Admin', 'Demo Org');

-- 3. User <-> project assignments (annotation_mode drives full vs binary) ------
INSERT INTO public.project_users (project_id, user_id, annotation_mode) VALUES
(1, 1, 'full'),    -- admin   -> CITY_24 (full)
(1, 3, 'full'),    -- birder2 -> CITY_24 (full)
(2, 2, 'binary'),  -- birder1 -> FOREST_24 (binary, occupancy playground)
(3, 1, 'binary');  -- admin   -> WETLAND_24 (binary, occupancy playground)

-- 4. Deployments (2 per project, with geometry) -------------------------------
INSERT INTO import.deployments
  (deployment_id, project_id, deployment_name, deployment_path, valid,
   device_manufacturer, device_modelname, geometry) VALUES
(1, 1, 'Park_Sued', '/data/city/park_sued',   TRUE, 'AudioMoth', 'AM 1.2.0', ST_SetSRID(ST_MakePoint(13.404, 52.500), 4326)),
(2, 1, 'Park_Nord', '/data/city/park_nord',   TRUE, 'AudioMoth', 'AM 1.2.0', ST_SetSRID(ST_MakePoint(13.388, 52.531), 4326)),
(3, 2, 'Forest_A',  '/data/forest/a',         TRUE, 'Wildlife Acoustics', 'SM4', ST_SetSRID(ST_MakePoint(13.201, 52.410), 4326)),
(4, 2, 'Forest_B',  '/data/forest/b',         TRUE, 'Wildlife Acoustics', 'SM4', ST_SetSRID(ST_MakePoint(13.245, 52.388), 4326)),
(5, 3, 'Marsh_A',   '/data/wetland/marsh_a',  TRUE, 'Wildlife Acoustics', 'SM4', ST_SetSRID(ST_MakePoint(12.980, 52.290), 4326)),
(6, 3, 'Marsh_B',   '/data/wetland/marsh_b',  TRUE, 'Wildlife Acoustics', 'SM4', ST_SetSRID(ST_MakePoint(12.940, 52.315), 4326));

-- 5. Model settings + species whitelist ---------------------------------------
INSERT INTO import.settings (settings_id, model_name, model_version, min_conf, overlap, locale, model_params) VALUES
(1, 'BirdNET', 'v2.4', 0.1, 0.0, 'de', jsonb_build_object('sensitivity', 1.0, 'week', -1, 'note', 'dev seed'));

INSERT INTO import.settings_species (settings_id, species_id) VALUES
(1, 6254),  -- Amsel / Turdus merula
(1, 4238),  -- Kohlmeise / Parus major
(1, 2371);  -- Buchfink / Fringilla coelebs

-- 6. Audio files. P1 files use FULL_SEG (1), P2/P3 files use WEAK_PRES (3) ------
INSERT INTO import.audio_files
  (audio_file_id, deployment_id, sample_rate, relative_path, duration_s,
   timestamp_start, required_annotation_type_id) VALUES
-- Project 1 / Park_Sued (d1)
(1, 1, 48000, 'park_sued/20240501_060000.wav', 60, TIMESTAMPTZ '2024-05-01 06:00:00+00', 1),
(2, 1, 48000, 'park_sued/20240501_070000.wav', 60, TIMESTAMPTZ '2024-05-01 07:00:00+00', 1),
-- Project 1 / Park_Nord (d2)
(3, 2, 48000, 'park_nord/20240501_060000.wav', 60, TIMESTAMPTZ '2024-05-01 06:00:00+00', 1),
(4, 2, 48000, 'park_nord/20240501_070000.wav', 60, TIMESTAMPTZ '2024-05-01 07:00:00+00', 1),
-- Project 2 / Forest_A (d3)
(5, 3, 48000, 'forest_a/20240510_053000.wav', 60, TIMESTAMPTZ '2024-05-10 05:30:00+00', 3),
(6, 3, 48000, 'forest_a/20240510_063000.wav', 60, TIMESTAMPTZ '2024-05-10 06:30:00+00', 3),
(7, 3, 48000, 'forest_a/20240510_073000.wav', 60, TIMESTAMPTZ '2024-05-10 07:30:00+00', 3),
(8, 3, 48000, 'forest_a/20240510_083000.wav', 60, TIMESTAMPTZ '2024-05-10 08:30:00+00', 3),
-- Project 2 / Forest_B (d4)
(9,  4, 48000, 'forest_b/20240510_053000.wav', 60, TIMESTAMPTZ '2024-05-10 05:30:00+00', 3),
(10, 4, 48000, 'forest_b/20240510_063000.wav', 60, TIMESTAMPTZ '2024-05-10 06:30:00+00', 3),
(11, 4, 48000, 'forest_b/20240510_073000.wav', 60, TIMESTAMPTZ '2024-05-10 07:30:00+00', 3),
(12, 4, 48000, 'forest_b/20240510_083000.wav', 60, TIMESTAMPTZ '2024-05-10 08:30:00+00', 3),
-- Project 3 / Marsh_A (d5)
(13, 5, 48000, 'marsh_a/20240515_053000.wav', 60, TIMESTAMPTZ '2024-05-15 05:30:00+00', 3),
(14, 5, 48000, 'marsh_a/20240515_063000.wav', 60, TIMESTAMPTZ '2024-05-15 06:30:00+00', 3),
(15, 5, 48000, 'marsh_a/20240515_073000.wav', 60, TIMESTAMPTZ '2024-05-15 07:30:00+00', 3),
(16, 5, 48000, 'marsh_a/20240515_083000.wav', 60, TIMESTAMPTZ '2024-05-15 08:30:00+00', 3),
-- Project 3 / Marsh_B (d6)
(17, 6, 48000, 'marsh_b/20240515_053000.wav', 60, TIMESTAMPTZ '2024-05-15 05:30:00+00', 3),
(18, 6, 48000, 'marsh_b/20240515_063000.wav', 60, TIMESTAMPTZ '2024-05-15 06:30:00+00', 3),
(19, 6, 48000, 'marsh_b/20240515_073000.wav', 60, TIMESTAMPTZ '2024-05-15 07:30:00+00', 3),
(20, 6, 48000, 'marsh_b/20240515_083000.wav', 60, TIMESTAMPTZ '2024-05-15 08:30:00+00', 3);

-- 7. BirdNET detections (results). confidence = raw * 10000 -------------------
--    result_id is kept in lockstep with spectrogram_id below (1:1).
INSERT INTO import.results
  (result_id, audio_file_id, settings_id, begin_time_ms, end_time_ms, confidence, species_id) VALUES
-- Project 1 / Park_Sued
( 1, 1, 1,    0, 3000, 9500, 6254),
( 2, 1, 1, 3000, 6000, 8200, 4238),
( 3, 1, 1, 6000, 9000, 7100, 2371),
( 4, 2, 1,    0, 3000, 9100, 6254),
( 5, 2, 1, 3000, 6000, 6000, 4238),
-- Project 1 / Park_Nord
( 6, 3, 1,    0, 3000, 8800, 6254),
( 7, 3, 1, 3000, 6000, 7600, 2371),
( 8, 4, 1,    0, 3000, 9300, 4238),
( 9, 4, 1, 3000, 6000, 6700, 6254),
-- Project 2 / Forest_A  (Amsel x5, Kohlmeise x3)
(10, 5, 1,    0, 3000, 9600, 6254),
(11, 5, 1, 3000, 6000, 8400, 4238),
(12, 6, 1,    0, 3000, 9000, 6254),
(13, 6, 1, 3000, 6000, 7800, 4238),
(14, 7, 1,    0, 3000, 8700, 6254),
(15, 7, 1, 3000, 6000, 7200, 4238),
(16, 8, 1,    0, 3000, 8300, 6254),
(17, 8, 1, 3000, 6000, 7900, 6254),
-- Project 2 / Forest_B  (Amsel x4, Buchfink x3, Kohlmeise x2)
(18,  9, 1,    0, 3000, 9400, 6254),
(19,  9, 1, 3000, 6000, 8100, 2371),
(20,  9, 1, 6000, 9000, 6500, 4238),
(21, 10, 1,    0, 3000, 9200, 6254),
(22, 10, 1, 3000, 6000, 7600, 2371),
(23, 11, 1,    0, 3000, 8800, 6254),
(24, 11, 1, 3000, 6000, 7000, 2371),
(25, 12, 1,    0, 3000, 8600, 6254),
(26, 12, 1, 3000, 6000, 6900, 4238),
-- Project 3 / Marsh_A  (Amsel x5, Kohlmeise x3)
(27, 13, 1,    0, 3000, 9600, 6254),
(28, 13, 1, 3000, 6000, 8400, 4238),
(29, 14, 1,    0, 3000, 9000, 6254),
(30, 14, 1, 3000, 6000, 7800, 4238),
(31, 15, 1,    0, 3000, 8700, 6254),
(32, 15, 1, 3000, 6000, 7200, 4238),
(33, 16, 1,    0, 3000, 8300, 6254),
(34, 16, 1, 3000, 6000, 7900, 6254),
-- Project 3 / Marsh_B  (Amsel x4, Buchfink x3, Kohlmeise x2)
(35, 17, 1,    0, 3000, 9400, 6254),
(36, 17, 1, 3000, 6000, 8100, 2371),
(37, 17, 1, 6000, 9000, 6500, 4238),
(38, 18, 1,    0, 3000, 9200, 6254),
(39, 18, 1, 3000, 6000, 7600, 2371),
(40, 19, 1,    0, 3000, 8800, 6254),
(41, 19, 1, 3000, 6000, 7000, 2371),
(42, 20, 1,    0, 3000, 8600, 6254),
(43, 20, 1, 3000, 6000, 6900, 4238);

-- 8. Spectrogram clips (1 per detection -- begin_time_ms MUST match the result)
--    buffer_ms = 2000 (2 s context before the 3 s detection window).
INSERT INTO import.spectrograms
  (spectrogram_id, audio_file_id, begin_time_ms, result_id, buffer_ms, duration_ms,
   resolution_x, resolution_y, freq_min, freq_max, selection_mode) VALUES
( 1, 1,    0,  1, 2000, 7000, 700, 1000, 0, 12000, 'top'),
( 2, 1, 3000,  2, 2000, 7000, 700, 1000, 0, 12000, 'top'),
( 3, 1, 6000,  3, 2000, 7000, 700, 1000, 0, 12000, 'top'),
( 4, 2,    0,  4, 2000, 7000, 700, 1000, 0, 12000, 'top'),
( 5, 2, 3000,  5, 2000, 7000, 700, 1000, 0, 12000, 'random'),
( 6, 3,    0,  6, 2000, 7000, 700, 1000, 0, 12000, 'top'),
( 7, 3, 3000,  7, 2000, 7000, 700, 1000, 0, 12000, 'random'),
( 8, 4,    0,  8, 2000, 7000, 700, 1000, 0, 12000, 'top'),
( 9, 4, 3000,  9, 2000, 7000, 700, 1000, 0, 12000, 'stratified'),
(10, 5,    0, 10, 2000, 7000, 700, 1000, 0, 12000, 'top'),
(11, 5, 3000, 11, 2000, 7000, 700, 1000, 0, 12000, 'top'),
(12, 6,    0, 12, 2000, 7000, 700, 1000, 0, 12000, 'top'),
(13, 6, 3000, 13, 2000, 7000, 700, 1000, 0, 12000, 'random'),
(14, 7,    0, 14, 2000, 7000, 700, 1000, 0, 12000, 'stratified'),
(15, 7, 3000, 15, 2000, 7000, 700, 1000, 0, 12000, 'random'),
(16, 8,    0, 16, 2000, 7000, 700, 1000, 0, 12000, 'top'),
(17, 8, 3000, 17, 2000, 7000, 700, 1000, 0, 12000, 'stratified'),
(18, 9,    0, 18, 2000, 7000, 700, 1000, 0, 12000, 'top'),
(19, 9, 3000, 19, 2000, 7000, 700, 1000, 0, 12000, 'top'),
(20, 9, 6000, 20, 2000, 7000, 700, 1000, 0, 12000, 'random'),
(21, 10,   0, 21, 2000, 7000, 700, 1000, 0, 12000, 'top'),
(22, 10, 3000, 22, 2000, 7000, 700, 1000, 0, 12000, 'stratified'),
(23, 11,   0, 23, 2000, 7000, 700, 1000, 0, 12000, 'top'),
(24, 11, 3000, 24, 2000, 7000, 700, 1000, 0, 12000, 'random'),
(25, 12,   0, 25, 2000, 7000, 700, 1000, 0, 12000, 'top'),
(26, 12, 3000, 26, 2000, 7000, 700, 1000, 0, 12000, 'stratified'),
(27, 13,   0, 27, 2000, 7000, 700, 1000, 0, 12000, 'top'),
(28, 13, 3000, 28, 2000, 7000, 700, 1000, 0, 12000, 'top'),
(29, 14,   0, 29, 2000, 7000, 700, 1000, 0, 12000, 'top'),
(30, 14, 3000, 30, 2000, 7000, 700, 1000, 0, 12000, 'random'),
(31, 15,   0, 31, 2000, 7000, 700, 1000, 0, 12000, 'stratified'),
(32, 15, 3000, 32, 2000, 7000, 700, 1000, 0, 12000, 'random'),
(33, 16,   0, 33, 2000, 7000, 700, 1000, 0, 12000, 'top'),
(34, 16, 3000, 34, 2000, 7000, 700, 1000, 0, 12000, 'stratified'),
(35, 17,   0, 35, 2000, 7000, 700, 1000, 0, 12000, 'top'),
(36, 17, 3000, 36, 2000, 7000, 700, 1000, 0, 12000, 'top'),
(37, 17, 6000, 37, 2000, 7000, 700, 1000, 0, 12000, 'random'),
(38, 18,   0, 38, 2000, 7000, 700, 1000, 0, 12000, 'top'),
(39, 18, 3000, 39, 2000, 7000, 700, 1000, 0, 12000, 'stratified'),
(40, 19,   0, 40, 2000, 7000, 700, 1000, 0, 12000, 'top'),
(41, 19, 3000, 41, 2000, 7000, 700, 1000, 0, 12000, 'random'),
(42, 20,   0, 42, 2000, 7000, 700, 1000, 0, 12000, 'top'),
(43, 20, 3000, 43, 2000, 7000, 700, 1000, 0, 12000, 'stratified');

-- 9. Analysis log (every audio file processed) --------------------------------
INSERT INTO import.analysis_log (audio_file_id, settings_id, status) VALUES
(1, 1, 'success'), (2, 1, 'success'), (3, 1, 'success'), (4, 1, 'success'),
(5, 1, 'success'), (6, 1, 'success'), (7, 1, 'success'), (8, 1, 'success'),
(9, 1, 'success'), (10, 1, 'success'), (11, 1, 'success'), (12, 1, 'success'),
(13, 1, 'success'), (14, 1, 'success'), (15, 1, 'success'), (16, 1, 'success'),
(17, 1, 'success'), (18, 1, 'success'), (19, 1, 'success'), (20, 1, 'success');

-- 10. Occupancy groups (projects 2 and 3) -------------------------------------
--     target_count = number of confirmed-present clips needed to auto-stop.
INSERT INTO import.occupancy_groups (group_id, project_id, group_name, description, target_count) VALUES
-- Project 2 FOREST_24 (birder1)
(1, 2, 'auto_Turdus merula__Forest_A',    'Amsel @ Forest_A -- one away (2 of 3)',  3),
(2, 2, 'auto_Parus major__Forest_A',      'Kohlmeise @ Forest_A -- absent/unsure',  2),
(3, 2, 'auto_Turdus merula__Forest_B',    'Amsel @ Forest_B -- already done (2/2)', 2),
(4, 2, 'auto_Fringilla coelebs__Forest_B','Buchfink @ Forest_B -- one away (1 of 2)',2),
(5, 2, 'auto_Parus major__Forest_B',      'Kohlmeise @ Forest_B -- fresh (0 of 2)', 2),
-- Project 3 WETLAND_24 (admin) -- parallel copy of the same five scenarios
(6, 3, 'auto_Turdus merula__Marsh_A',     'Amsel @ Marsh_A -- one away (2 of 3)',  3),
(7, 3, 'auto_Parus major__Marsh_A',       'Kohlmeise @ Marsh_A -- absent/unsure',  2),
(8, 3, 'auto_Turdus merula__Marsh_B',     'Amsel @ Marsh_B -- already done (2/2)', 2),
(9, 3, 'auto_Fringilla coelebs__Marsh_B', 'Buchfink @ Marsh_B -- one away (1 of 2)',2),
(10,3, 'auto_Parus major__Marsh_B',       'Kohlmeise @ Marsh_B -- fresh (0 of 2)', 2);

-- 11. Group membership (spectrograms belonging to each occupancy group) --------
INSERT INTO import.spectrogram_groups (spectrogram_id, group_id) VALUES
-- Project 2 (Forest)
(10, 1), (12, 1), (14, 1), (16, 1), (17, 1),  -- G1 Amsel @ Forest_A (5)
(11, 2), (13, 2), (15, 2),                     -- G2 Kohlmeise @ Forest_A (3)
(18, 3), (21, 3), (23, 3), (25, 3),            -- G3 Amsel @ Forest_B (4)
(19, 4), (22, 4), (24, 4),                     -- G4 Buchfink @ Forest_B (3)
(20, 5), (26, 5),                              -- G5 Kohlmeise @ Forest_B (2)
-- Project 3 (Wetland) -- mirrors the above
(27, 6), (29, 6), (31, 6), (33, 6), (34, 6),  -- G6 Amsel @ Marsh_A (5)
(28, 7), (30, 7), (32, 7),                     -- G7 Kohlmeise @ Marsh_A (3)
(35, 8), (38, 8), (40, 8), (42, 8),            -- G8 Amsel @ Marsh_B (4)
(36, 9), (39, 9), (41, 9),                     -- G9 Buchfink @ Marsh_B (3)
(37, 10), (43, 10);                            -- G10 Kohlmeise @ Marsh_B (2)

-- 12. Annotation status (who verified which snippet) --------------------------
--     Full mode (project 1): target_species_id IS NULL.
--     Binary mode (projects 2/3): target_species_id = the queue/task species.
--     begin_time_ms MUST match the spectrogram/result window.
INSERT INTO import.annotation_status
  (audio_file_id, user_id, begin_time_ms, end_time_ms, annotation_type_id, target_species_id) VALUES
-- Project 1, admin (full segmentation) -- af1 fully worked
(1, 1,    0, 3000, 1, NULL),
(1, 1, 3000, 6000, 1, NULL),
(1, 1, 6000, 9000, 1, NULL),
-- Project 2, birder1 -- G1 Amsel @ Forest_A: 2 of 3 confirmed (spec 10,12)
(5, 2,    0, 3000, 3, 6254),
(6, 2,    0, 3000, 3, 6254),
-- Project 2, birder1 -- G2 Kohlmeise @ Forest_A: one ABSENT, one UNSURE
(5, 2, 3000, 6000, 3, 4238),
(6, 2, 3000, 6000, 3, 4238),
-- Project 2, birder1 -- G3 Amsel @ Forest_B: 2 of 2 confirmed (spec 18,21) -> done
(9,  2,   0, 3000, 3, 6254),
(10, 2,   0, 3000, 3, 6254),
-- Project 2, birder1 -- G4 Buchfink @ Forest_B: 1 of 2 confirmed (spec 19)
(9, 2, 3000, 6000, 3, 2371),
-- Project 3, admin -- G6 Amsel @ Marsh_A: 2 of 3 confirmed (spec 27,29)
(13, 1,   0, 3000, 3, 6254),
(14, 1,   0, 3000, 3, 6254),
-- Project 3, admin -- G7 Kohlmeise @ Marsh_A: one ABSENT, one UNSURE
(13, 1, 3000, 6000, 3, 4238),
(14, 1, 3000, 6000, 3, 4238),
-- Project 3, admin -- G8 Amsel @ Marsh_B: 2 of 2 confirmed (spec 35,38) -> done
(17, 1,   0, 3000, 3, 6254),
(18, 1,   0, 3000, 3, 6254),
-- Project 3, admin -- G9 Buchfink @ Marsh_B: 1 of 2 confirmed (spec 36)
(17, 1, 3000, 6000, 3, 2371);

-- 13. Ground-truth annotations (the confirmed truth) --------------------------
--     is_present + certainty_id = 1 make a clip count toward auto-stop.
INSERT INTO import.ground_truth_annotations
  (audio_file_id, user_id, species_id, abiotic_sound_id, begin_time_ms, end_time_ms,
   is_present, behavior_id, certainty_id) VALUES
-- Project 1, admin -- af1: Amsel singing, Kohlmeise calling, Buchfink present + traffic
(1, 1, 6254, NULL,    0, 3000, TRUE, 1, 1),
(1, 1, 4238, NULL, 3000, 6000, TRUE, 2, 1),
(1, 1, 2371, NULL, 6000, 9000, TRUE, 1, 2),
(1, 1, NULL, 1,       0, 3000, TRUE, NULL, 1),
-- Project 2 -- G1 Amsel @ Forest_A: 2 CERTAIN presences (spec 10,12) -> 2/3
(5, 2, 6254, NULL,    0, 3000, TRUE, 1, 1),
(6, 2, 6254, NULL,    0, 3000, TRUE, 1, 1),
-- Project 2 -- G2 Kohlmeise @ Forest_A: one ABSENT, one UNSURE -> 0 valid hits
(5, 2, 4238, NULL, 3000, 6000, FALSE, NULL, 1),
(6, 2, 4238, NULL, 3000, 6000, TRUE,  2, 3),
-- Project 2 -- G3 Amsel @ Forest_B: 2 CERTAIN presences (spec 18,21) -> done
(9,  2, 6254, NULL,   0, 3000, TRUE, 1, 1),
(10, 2, 6254, NULL,   0, 3000, TRUE, 1, 1),
-- Project 2 -- G4 Buchfink @ Forest_B: 1 CERTAIN presence (spec 19) -> 1/2
(9, 2, 2371, NULL, 3000, 6000, TRUE, 2, 1),
-- Project 3 -- G6 Amsel @ Marsh_A: 2 CERTAIN presences (spec 27,29) -> 2/3
(13, 1, 6254, NULL,   0, 3000, TRUE, 1, 1),
(14, 1, 6254, NULL,   0, 3000, TRUE, 1, 1),
-- Project 3 -- G7 Kohlmeise @ Marsh_A: one ABSENT, one UNSURE -> 0 valid hits
(13, 1, 4238, NULL, 3000, 6000, FALSE, NULL, 1),
(14, 1, 4238, NULL, 3000, 6000, TRUE,  2, 3),
-- Project 3 -- G8 Amsel @ Marsh_B: 2 CERTAIN presences (spec 35,38) -> done
(17, 1, 6254, NULL,   0, 3000, TRUE, 1, 1),
(18, 1, 6254, NULL,   0, 3000, TRUE, 1, 1),
-- Project 3 -- G9 Buchfink @ Marsh_B: 1 CERTAIN presence (spec 36) -> 1/2
(17, 1, 2371, NULL, 3000, 6000, TRUE, 2, 1);

-- 14. Reset sequences so the app's own inserts continue past the seeded ids ----
SELECT setval('import.projects_id_seq',                     (SELECT MAX(project_id)     FROM import.projects));
SELECT setval('import.deployments_id_seq',                  (SELECT MAX(deployment_id)  FROM import.deployments));
SELECT setval('import.settings_id_seq',                     (SELECT MAX(settings_id)    FROM import.settings));
SELECT setval('import.audio_file_id_seq',                   (SELECT MAX(audio_file_id)  FROM import.audio_files));
SELECT setval('import.results_data_id_seq',                 (SELECT MAX(result_id)      FROM import.results));
SELECT setval('import.annotation_status_data_id_seq',       (SELECT MAX(status_id)      FROM import.annotation_status));
SELECT setval('import.ground_truth_annotations_data_id_seq',(SELECT MAX(annotation_id)  FROM import.ground_truth_annotations));
SELECT setval(pg_get_serial_sequence('import.spectrograms',    'spectrogram_id'), (SELECT MAX(spectrogram_id) FROM import.spectrograms));
SELECT setval(pg_get_serial_sequence('import.occupancy_groups','group_id'),       (SELECT MAX(group_id)       FROM import.occupancy_groups));
SELECT setval(pg_get_serial_sequence('public.app_users',       'user_id'),        (SELECT MAX(user_id)        FROM public.app_users));
