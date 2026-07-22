-- ============================================================
-- 61_create_status_views.sql
--
-- Read-only views describing the state of each project:
--   * v_project_audio_summary      - deployments / audio files / inference runs
--   * v_project_spectrogram_summary- spectrograms and how many are classified
--   * v_project_species_confirmed  - species confirmed via ground truth
--   * v_user_activity_sessions     - annotation sessions (5 min buffer)
--   * v_user_activity_daily        - active minutes per user and date
--   * v_project_status             - one row per project, everything combined
--
-- Safe to re-run: every view is dropped and recreated.
-- Views live in the import schema, next to the tables they aggregate.
--
-- NOTE on time zones: created_at is timestamptz, so the calendar date in
-- v_user_activity_daily is derived in the session time zone of the connection
-- reading the view. Set TimeZone on the app connection if a fixed zone is
-- required.
-- ============================================================

DROP VIEW IF EXISTS import.v_project_status;
DROP VIEW IF EXISTS import.v_user_activity_daily;
DROP VIEW IF EXISTS import.v_user_activity_sessions;
DROP VIEW IF EXISTS import.v_project_species_confirmed;
DROP VIEW IF EXISTS import.v_project_spectrogram_summary;
DROP VIEW IF EXISTS import.v_project_audio_summary;


-- ------------------------------------------------------------
-- 1. Audio material per project
--    n_audio_files counts non-deleted files only. Deleted files are
--    reported separately so the difference stays visible.
--    n_audio_files_analysed counts files with at least one successful
--    inference run, n_audio_files_analysis_failed only files that failed
--    and never succeeded.
-- ------------------------------------------------------------
CREATE VIEW import.v_project_audio_summary AS
SELECT
    p.project_id,
    p.project_name_short,
    p.project_name_long,
    COUNT(DISTINCT d.deployment_id)                                          AS n_deployments,
    COUNT(DISTINCT d.deployment_id) FILTER (WHERE d.valid)                   AS n_deployments_valid,
    COUNT(af.audio_file_id) FILTER (WHERE COALESCE(af.deleted, FALSE) = FALSE) AS n_audio_files,
    COUNT(af.audio_file_id) FILTER (WHERE af.deleted)                        AS n_audio_files_deleted,
    COALESCE(SUM(af.duration_s) FILTER (WHERE COALESCE(af.deleted, FALSE) = FALSE), 0) AS total_duration_s,
    ROUND(
        COALESCE(SUM(af.duration_s) FILTER (WHERE COALESCE(af.deleted, FALSE) = FALSE), 0) / 3600.0,
        2
    )                                                                        AS total_duration_h,
    MIN(af.timestamp_start)                                                  AS first_recording_at,
    MAX(af.timestamp_start)                                                  AS last_recording_at,
    COUNT(af.audio_file_id) FILTER (WHERE alg.has_success)                   AS n_audio_files_analysed,
    COUNT(af.audio_file_id) FILTER (WHERE alg.has_failure AND NOT alg.has_success) AS n_audio_files_analysis_failed
FROM import.projects p
LEFT JOIN import.deployments d  ON d.project_id     = p.project_id
LEFT JOIN import.audio_files af ON af.deployment_id = d.deployment_id
-- aggregated per audio file: a file can be analysed with several settings,
-- a plain join would multiply the audio file rows
LEFT JOIN LATERAL (
    SELECT
        BOOL_OR(al.status =  'success') AS has_success,
        BOOL_OR(al.status <> 'success') AS has_failure
    FROM import.analysis_log al
    WHERE al.audio_file_id = af.audio_file_id
) alg ON TRUE
GROUP BY p.project_id, p.project_name_short, p.project_name_long;

COMMENT ON VIEW import.v_project_audio_summary IS
    'Per project: number of deployments, audio files, recorded hours and BirdNET inference coverage.';


-- ------------------------------------------------------------
-- 2. Spectrograms per project
--    A spectrogram counts as "classified" as soon as at least one
--    annotation_status row exists for its (audio_file_id, begin_time_ms)
--    slot -- that is the log of finished annotation work.
-- ------------------------------------------------------------
CREATE VIEW import.v_project_spectrogram_summary AS
SELECT
    p.project_id,
    p.project_name_short,
    COUNT(s.spectrogram_id)                                        AS n_spectrograms,
    COUNT(s.spectrogram_id) FILTER (WHERE st.n_status > 0)         AS n_classified,
    COUNT(s.spectrogram_id) FILTER (WHERE st.n_status = 0)         AS n_open,
    ROUND(
        100.0 * COUNT(s.spectrogram_id) FILTER (WHERE st.n_status > 0)
              / NULLIF(COUNT(s.spectrogram_id), 0),
        1
    )                                                              AS pct_classified,
    COUNT(s.spectrogram_id) FILTER (WHERE s.audio_data IS NOT NULL) AS n_with_audio_blob,
    COALESCE(SUM(st.n_status), 0)                                  AS n_annotation_status_rows,
    (
        SELECT COUNT(DISTINCT ast.user_id)
        FROM import.annotation_status ast
        JOIN import.audio_files af2 ON af2.audio_file_id = ast.audio_file_id
        JOIN import.deployments d2  ON d2.deployment_id  = af2.deployment_id
        WHERE d2.project_id = p.project_id
    )                                                              AS n_annotating_users
FROM import.projects p
LEFT JOIN import.deployments d  ON d.project_id      = p.project_id
LEFT JOIN import.audio_files af ON af.deployment_id  = d.deployment_id
LEFT JOIN import.spectrograms s ON s.audio_file_id   = af.audio_file_id
LEFT JOIN LATERAL (
    SELECT COUNT(*) AS n_status
    FROM import.annotation_status ast
    WHERE ast.audio_file_id = s.audio_file_id
      AND ast.begin_time_ms = s.begin_time_ms
) st ON TRUE
GROUP BY p.project_id, p.project_name_short;

COMMENT ON VIEW import.v_project_spectrogram_summary IS
    'Per project: number of spectrograms and how many of them have already been classified (have an annotation_status entry).';


-- ------------------------------------------------------------
-- 3. Species confirmed via ground truth annotations
--    One row per project and species. n_certain restricts to
--    certainty_id = 1 (certain), which is the same criterion the app
--    uses for occupancy counting.
-- ------------------------------------------------------------
CREATE VIEW import.v_project_species_confirmed AS
SELECT
    d.project_id,
    p.project_name_short,
    gt.species_id,
    sp.species_scientific,
    sp.species_long_de,
    sp.species_long_en,
    COUNT(*)                                                AS n_annotations,
    COUNT(*) FILTER (WHERE gt.certainty_id = 1)             AS n_certain,
    COUNT(DISTINCT gt.audio_file_id)                        AS n_audio_files,
    COUNT(DISTINCT d.deployment_id)                         AS n_deployments,
    COUNT(DISTINCT gt.user_id)                              AS n_users,
    MIN(gt.created_at)                                      AS first_confirmed_at,
    MAX(gt.created_at)                                      AS last_confirmed_at
FROM import.ground_truth_annotations gt
JOIN import.audio_files af      ON af.audio_file_id = gt.audio_file_id
JOIN import.deployments d       ON d.deployment_id  = af.deployment_id
JOIN import.projects p          ON p.project_id     = d.project_id
JOIN public.lut_species_code sp ON sp.species_id    = gt.species_id
WHERE gt.is_present
  AND gt.species_id IS NOT NULL
GROUP BY d.project_id, p.project_name_short, gt.species_id,
         sp.species_scientific, sp.species_long_de, sp.species_long_en;

COMMENT ON VIEW import.v_project_species_confirmed IS
    'Per project and species: species presence confirmed by ground truth annotations, including a count restricted to certain identifications.';


-- ------------------------------------------------------------
-- 4. Annotation sessions
--    Each ground truth annotation is treated as 5 minutes of activity
--    starting at its created_at timestamp. Consecutive annotations that
--    are at most 5 minutes apart are merged into one session
--    (gaps-and-islands), so overlapping buffers are never counted twice.
--    A single isolated annotation therefore yields exactly 5 minutes.
-- ------------------------------------------------------------
CREATE VIEW import.v_user_activity_sessions AS
WITH events AS (
    SELECT
        d.project_id,
        gt.user_id,
        gt.created_at
    FROM import.ground_truth_annotations gt
    JOIN import.audio_files af ON af.audio_file_id = gt.audio_file_id
    JOIN import.deployments d  ON d.deployment_id  = af.deployment_id
),
flagged AS (
    SELECT
        project_id,
        user_id,
        created_at,
        CASE
            WHEN created_at - LAG(created_at) OVER (
                     PARTITION BY project_id, user_id ORDER BY created_at
                 ) <= interval '5 minutes'
            THEN 0
            ELSE 1
        END AS is_new_session
    FROM events
),
numbered AS (
    SELECT
        project_id,
        user_id,
        created_at,
        SUM(is_new_session) OVER (
            PARTITION BY project_id, user_id ORDER BY created_at
            ROWS UNBOUNDED PRECEDING
        ) AS session_nr
    FROM flagged
)
SELECT
    project_id,
    user_id,
    session_nr,
    MIN(created_at)                          AS session_start,
    MAX(created_at) + interval '5 minutes'   AS session_end,
    COUNT(*)                                 AS n_annotations,
    ROUND(
        EXTRACT(EPOCH FROM (MAX(created_at) + interval '5 minutes' - MIN(created_at)))::numeric / 60.0,
        2
    )                                        AS active_minutes
FROM numbered
GROUP BY project_id, user_id, session_nr;

COMMENT ON VIEW import.v_user_activity_sessions IS
    'Annotation sessions per project and user: runs of ground truth annotations less than 5 minutes apart, each annotation buffered by 5 minutes of active time.';


-- ------------------------------------------------------------
-- 5. Active minutes per user and calendar date
--    Sessions crossing midnight are split at the day boundary, so the
--    minutes are attributed to the day on which they were actually spent.
-- ------------------------------------------------------------
CREATE VIEW import.v_user_activity_daily AS
WITH per_day AS (
    SELECT
        s.project_id,
        s.user_id,
        day::date AS activity_date,
        SUM(
            EXTRACT(EPOCH FROM (
                LEAST(s.session_end, day + interval '1 day')
                - GREATEST(s.session_start, day)
            ))::numeric / 60.0
        ) AS active_minutes,
        COUNT(*) AS n_sessions
    FROM import.v_user_activity_sessions s
    CROSS JOIN LATERAL generate_series(
        date_trunc('day', s.session_start),
        date_trunc('day', s.session_end),
        interval '1 day'
    ) AS day
    GROUP BY s.project_id, s.user_id, day::date
),
annotations_per_day AS (
    SELECT
        d.project_id,
        gt.user_id,
        gt.created_at::date AS activity_date,
        COUNT(*)                          AS n_annotations,
        COUNT(DISTINCT gt.audio_file_id)  AS n_audio_files,
        COUNT(DISTINCT gt.species_id)     AS n_species
    FROM import.ground_truth_annotations gt
    JOIN import.audio_files af ON af.audio_file_id = gt.audio_file_id
    JOIN import.deployments d  ON d.deployment_id  = af.deployment_id
    GROUP BY d.project_id, gt.user_id, gt.created_at::date
)
SELECT
    pd.project_id,
    p.project_name_short,
    pd.user_id,
    u.username,
    pd.activity_date,
    ROUND(pd.active_minutes, 2)             AS active_minutes,
    ROUND(pd.active_minutes / 60.0, 2)      AS active_hours,
    pd.n_sessions,
    COALESCE(apd.n_annotations, 0)          AS n_annotations,
    COALESCE(apd.n_audio_files, 0)          AS n_audio_files,
    COALESCE(apd.n_species, 0)              AS n_species
FROM per_day pd
JOIN import.projects p ON p.project_id = pd.project_id
JOIN public.app_users u ON u.user_id   = pd.user_id
LEFT JOIN annotations_per_day apd
       ON apd.project_id    = pd.project_id
      AND apd.user_id       = pd.user_id
      AND apd.activity_date = pd.activity_date;

COMMENT ON VIEW import.v_user_activity_daily IS
    'Active minutes per project, user and calendar date, derived from ground truth annotation timestamps with a 5 minute buffer per annotation.';


-- ------------------------------------------------------------
-- 6. One-row-per-project overview
-- ------------------------------------------------------------
CREATE VIEW import.v_project_status AS
SELECT
    a.project_id,
    a.project_name_short,
    a.project_name_long,
    a.n_deployments,
    a.n_audio_files,
    a.total_duration_h,
    a.n_audio_files_analysed,
    sg.n_spectrograms,
    sg.n_classified                       AS n_spectrograms_classified,
    sg.n_open                             AS n_spectrograms_open,
    sg.pct_classified                     AS pct_spectrograms_classified,
    COALESCE(sc.n_species_confirmed, 0)   AS n_species_confirmed,
    COALESCE(sc.n_species_certain, 0)     AS n_species_confirmed_certain,
    COALESCE(ua.n_users_active, 0)        AS n_users_active,
    COALESCE(ua.active_minutes_total, 0)  AS active_minutes_total,
    COALESCE(ua.active_hours_total, 0)    AS active_hours_total,
    ua.first_activity_date,
    ua.last_activity_date
FROM import.v_project_audio_summary a
LEFT JOIN import.v_project_spectrogram_summary sg ON sg.project_id = a.project_id
LEFT JOIN (
    SELECT
        project_id,
        COUNT(*)                                  AS n_species_confirmed,
        COUNT(*) FILTER (WHERE n_certain > 0)     AS n_species_certain
    FROM import.v_project_species_confirmed
    GROUP BY project_id
) sc ON sc.project_id = a.project_id
LEFT JOIN (
    SELECT
        project_id,
        COUNT(DISTINCT user_id)                AS n_users_active,
        ROUND(SUM(active_minutes), 2)          AS active_minutes_total,
        ROUND(SUM(active_minutes) / 60.0, 2)   AS active_hours_total,
        MIN(activity_date)                     AS first_activity_date,
        MAX(activity_date)                     AS last_activity_date
    FROM import.v_user_activity_daily
    GROUP BY project_id
) ua ON ua.project_id = a.project_id;

COMMENT ON VIEW import.v_project_status IS
    'One row per project: audio material, classification progress, confirmed species and annotator activity.';


-- ------------------------------------------------------------
-- 7. Grants
-- ------------------------------------------------------------
GRANT SELECT ON
    import.v_project_audio_summary,
    import.v_project_spectrogram_summary,
    import.v_project_species_confirmed,
    import.v_user_activity_sessions,
    import.v_user_activity_daily,
    import.v_project_status
TO evalpam_birder, evalpam_admin;
