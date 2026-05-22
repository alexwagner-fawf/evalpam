# evalpam — Technical Documentation

**Version**: 0.0.0.9001  
**Authors**: Gabriel Holz, Alexander Wagner  
**Date**: April 2026

---

## 1. Overview and Purpose

**evalpam** (Evaluate Passive Acoustic Monitoring) is an R package that combines a PostgreSQL-backed data pipeline with a Shiny web application for the expert verification of AI-generated bird call detections. It is designed for use in ecological research projects where BirdNET — a deep-learning species detector — processes large archives of field recordings, and domain experts are needed to validate or correct its output before it enters scientific analyses.

The core workflow has three stages:

1. **Ingestion**: audio files are discovered on disk, indexed, and their metadata written to the database.
2. **Inference**: BirdNET analyses every audio file, and its species predictions are stored alongside the original recordings.
3. **Verification**: experts open the Shiny app, listen to audio clips rendered as interactive spectrograms, and assign ground-truth labels (species, behaviour, certainty).

The application supports multi-user collaborative annotation with conflict detection, two annotation modes (binary and full), and a built-in logistic regression tool that lets users assess how well BirdNET's confidence score separates true from false detections for a given species.

### Technology stack

| Layer | Technology |
|---|---|
| Web framework | R Shiny + shinymanager (auth) |
| Audio visualisation | WaveSurfer.js 7 (Spectrogram, Regions, Timeline plugins) |
| Database | PostgreSQL with PostGIS |
| DB connectivity | pool + RPostgres + DBI/dbplyr |
| Audio processing | av, tuneR, seewave, exiftoolr |
| AI inference | birdnetR (Python BirdNET via reticulate) |
| Password security | bcrypt (user passwords) + keyring (DB credentials) |
| JS dependencies | WaveSurfer.js 7.8.17, vendored — no CDN at runtime |

---

## 2. Database Architecture

The database is a PostgreSQL instance with PostGIS enabled. Two primary schemas are used: `public` for shared lookup tables and user management, and `import` for project-specific data.

### 2.1 Lookup tables (`public` schema)

Lookup tables store controlled vocabularies that are shared across all projects. They are populated once during setup and rarely change.

| Table | Key columns | Purpose |
|---|---|---|
| `lut_species_code` | species_id, species_short, species_long_de/en, species_scientific | BirdNET-compatible species list |
| `lut_behavior_code` | behavior_id, behavior_short | Gesang, Ruf, etc. |
| `lut_certainty_code` | certainty_id, certainty_short | Annotation confidence levels |
| `lut_annotation_type_code` | annotation_type_id | Binary vs. full annotation |
| `lut_abiotic_sound_code` | abiotic_sound_id, abiotic_sound_short | Non-biotic sound categories (wind, traffic, etc.) |

### 2.2 User and project tables (`public` schema)

`app_users` stores login credentials (username, bcrypt password hash, name, email, expiry date, active flag). `app_user_roles` maps each user to one or more PostgreSQL roles (`evalpam_admin`, `evalpam_user`). `project_users` links users to the specific projects they are permitted to access.

### 2.3 Data tables (`import` schema)

These tables hold the project data from ingestion through to annotation.

| Table | Purpose |
|---|---|
| `projects` | Top-level project definitions |
| `deployments` | Recording locations; geometry stored as PostGIS point (SRID 4326) |
| `audio_files` | One row per audio file: deployment_id, sample_rate, duration_s, timestamp_start, relative_path |
| `settings` | BirdNET model configuration: model_name, min_confidence, overlap, model_params (JSONB) |
| `settings_species` | Junction: which species a given settings record targets |
| `results` | BirdNET output: audio_file_id, species_id, confidence (smallint ×10 000), begin/end_time_ms |
| `spectrograms` | One row per audio clip: audio_file_id, result_id, buffer_ms, duration_ms, freq_min/max, audio_data (BYTEA) |
| `annotation_status` | Per-snippet lock: records who is currently annotating which file/time window |
| `ground_truth_annotations` | Final labels: audio_file_id, user_id, species_id (nullable), abiotic_sound_id (nullable), behavior_id, certainty_id, begin/end_time_ms, is_present |
| `analysis_log` | Processing audit trail: audio_file_id, settings_id, analysed_at, status |

### 2.4 Initialisation

All schema objects are created by running the numbered SQL scripts in `inst/sql/` in order (00 through 51). The entry point `setup_app()` reads credentials, writes non-secret connection parameters to `golem-config.yml`, stores the database password securely in the OS keychain via `keyring`, and optionally executes the SQL scripts via `DBI::dbExecute()`. Dollar-quoted PL/pgSQL blocks (`$$…$$`) are parsed correctly before execution. A seed script (`99_seed_dummy_data.sql`) creates test data and is only run when `dummy = TRUE`.

---

## 3. Data Pipeline

The pipeline is fully scriptable and intended to be run from the R console or a scheduled job before users open the app.

### 3.1 Audio file discovery — `retrieve_local_file_info()`

The function walks a project folder to a configurable depth (default: 2 levels, where each leaf directory corresponds to one deployment). For each deployment it:

1. Lists audio files matching known extensions (`.wav`, `.mp3`, `.flac`, etc.) using a two-phase scan with retry logic to tolerate intermittent network-drive failures.
2. Reads EXIF metadata in batches of ~500 files via `exiftoolr` to extract sample rate and file duration.
3. Determines recording start timestamps either by parsing the filename (`YYYYMMDD_HHMMSS` pattern) or from EXIF fields (`MediaCreateDate` → `CreateDate` → `FileModifyDate`), with optional timezone coercion.
4. Writes an `.fst` index file per deployment to `<project_folder>/audio_file_indices/` for fast subsequent access.
5. Returns a deployment-level summary with start/end timestamps for the whole monitoring period.

The resulting data frame is passed to `upsert_audio_files_df()`, which writes records to `import.audio_files` using `ON CONFLICT (deployment_id, timestamp_start) DO UPDATE`.

### 3.2 BirdNET inference — `process_deployment_birdnet()`

`setup_birdnet_model()` initialises BirdNET with geographic coordinates and a calendar week, which causes the model to return only locally plausible species. Coordinates are rounded to one decimal place before model construction to avoid creating redundant `settings` rows for closely spaced deployments.

`apply_birdnet_model()` runs inference on one audio file. It validates the file (existence, readability, non-zero size) with exponential-backoff retries, distinguishing permanent errors (corrupt file → no retry) from transient I/O errors (network issues → up to three retries). The raw output is a data frame of (start_ms, end_ms, scientific_name, confidence) tuples. Confidence scores are stored as `smallint` multiplied by 10 000 to avoid floating-point precision issues.

`process_deployment_birdnet()` orchestrates a full deployment: it groups audio files by calendar week (when `temporal_filtering = TRUE`), runs per-week model setup, loops over files, joins predictions with the species lookup table to resolve `species_id`, deduplicates on `(audio_file_id, settings_id, begin_time_ms, end_time_ms, species_id)` keeping the highest confidence, and upserts everything via `upsert_results_df()`.

### 3.3 Spectrogram and clip generation — `build_audio_clips_db()`

`sample_results_table()` selects a manageable subset of BirdNET results for annotation. Three sampling modes are available: `"top"` (highest confidence), `"random"`, and `"stratified"` (one result per confidence bin, giving uniform coverage of the score distribution).

`build_audio_clips_db()` converts each selected result into an MP3 clip:

1. Queries `audio_files` + `deployments` to reconstruct the full file path.
2. Inserts a row into `import.spectrograms` with clip metadata (buffer_ms, duration_ms, freq_min, freq_max).
3. Extracts the clip from the source file using `av::av_audio_convert()` with a configurable padding window (default: 5 s before and after the detection window).
4. Saves the MP3 to `spectrograms/<spectrogram_id>.mp3` on disk. Optionally, the raw bytes are also written to `import.spectrograms.audio_data` for deployments where the app server cannot reach the audio storage directly.

The previous `build_spectrogram_db()` function generated `.mp4` spectrogram videos server-side using `av::av_spectrogram_video()` with FFmpeg drawbox overlays for detection markers. This approach has been superseded: spectrograms are now rendered entirely in the browser by WaveSurfer.js, eliminating the server-side video generation overhead and enabling interactive features such as user-drawn selections and frequency-filtered playback.

---

## 4. Shiny Application

### 4.1 Entry point and authentication

`run_app()` creates a `pool` connection object, registers `pool::poolClose()` on session stop, and passes the pool to `app_server()`. The UI is wrapped with `shinymanager::secure_app()`, which renders a login form before any application content is shown.

Authentication is handled by `check_credentials_db()`, which returns a closure. When a user submits credentials, the closure queries `app_users`, checks the bcrypt hash, verifies the account is active and not expired, and returns a `user_info` data frame containing `user_id` and `first_name`. These values are available in `res_auth` throughout the session.

### 4.2 User interface — `app_ui()`

The layout uses a `fluidPage` with a sidebar (width = 4) and a main panel (width = 8).

**Sidebar** (top to bottom):
- Logged-in user name and a gear-icon settings menu (language selector: German / English / Scientific; spectrogram frequency range in Hz).
- Project selector.
- In binary mode: a target species selector; in full mode: a sort-order selector.
- Score threshold slider (0–1) and a searchable file selector (`selectizeInput`).
- A multi-select species input and a dynamically generated container for per-species behaviour and certainty dropdowns.
- "Save & Continue" and "Model Check" action buttons.
- A compact DT table showing the top BirdNET detections for the current clip.

**Main panel**:
- Playback controls (play/pause button, elapsed/total time display).
- A `div#waveform` container and `div#timeline` container where WaveSurfer.js renders the spectrogram and timeline.
- A legend explaining the detection window (green overlay) and padding area (grey).

### 4.3 Server logic — `app_server()`

**Reactive data loading**: `project_data()` executes a multi-join SQL query: `results → spectrograms → audio_files → deployments`, filtered to the selected project. `filtered_files()` applies the score threshold and, in binary mode, the target species filter, then excludes snippets already recorded in `annotation_status` for other users.

**WaveSurfer.js integration**: when the file selection changes, the server sends a custom Shiny message (`ws_load`) containing the URL path to the MP3 clip, the detection window boundaries in seconds, and the display frequency range. The JS handler reinitialises WaveSurfer with the new audio source and redraws detection-window markers.

**Selection-based filtered playback**: users can drag a rectangle on the spectrogram canvas. The JS layer reports the selected time range and frequency bounds back to the server via `input$spec_selection`. The server reads the original MP3, applies a time cut (`seewave::cutw()`) and a bandpass filter (`tuneR::ffilter()`), re-encodes the result as a base64 MP3 data-URL, and sends it back via a `ws_play_selection` message. WaveSurfer.js plays this filtered clip in a loop.

**Dynamic annotation UI**: the `dynamic_behavior_ui` output renders one `selectInput` for behaviour and one for certainty for every species currently selected in the multi-select widget. If a ground-truth record already exists for this snippet and user, existing values are pre-populated. The default behaviour is "Gesang" (song).

**Concurrent-editing protection**: the server checks `annotation_status` for each clip. If the same user previously annotated it, the button label is "Update". If a different user holds the lock, the save button is disabled and labelled "Locked".

### 4.4 Client-side spectrogram — `wavesurfer_init.js`

The JS file (~850 lines) is embedded inline at startup to avoid browser caching stale versions during development. WaveSurfer.js and its three plugins (Spectrogram, Regions, Timeline) are served from vendored copies inside the package at `inst/app/www/wavesurfer/` and registered by `app_ui()` as a Shiny resource path (`/wslib/`). No external network request is made at runtime.

**Spectrogram rendering pipeline**:
1. WaveSurfer decodes the MP3 at the minimum sample rate required to cover `frequencyMax` (with 2.2× headroom), using a 1024-sample FFT window.
2. After the spectrogram canvas is painted, a `MutationObserver` detects the new canvas and triggers per-row normalisation:
   - Pass 1: subtract the local 5th-percentile intensity per frequency bin (noise-floor suppression).
   - Pass 2: scale globally to the 99th-percentile peak.
   - Pass 3: apply a custom magma-derived colormap (black point at 0.35, gamma 0.5) for perceptual contrast.
3. Detection-window boundaries are drawn as green vertical lines directly on the canvas.

**Audio controls**: Web Audio API biquad highpass and lowpass filters are chained into the WaveSurfer output node. Their cutoff frequencies are updated in real time when the user adjusts the frequency range selectors or plays back a user-drawn selection.

---

## 5. Annotation Workflow, Model Validation, and User Management

### 5.1 Save transaction

When the user clicks "Save & Continue", the server executes a single database transaction:

1. `DELETE FROM import.ground_truth_annotations WHERE audio_file_id = X AND user_id = Y AND begin_time_ms = Z` — removes any prior annotation by this user for this time window. In binary mode the delete is scoped to the target species; in full mode it removes all species.
2. `INSERT INTO import.ground_truth_annotations` — one row per selected species, carrying `species_id`, `behavior_id`, `certainty_id`, `begin_time_ms`, `end_time_ms`, and `is_present = TRUE`.
3. `DELETE / INSERT` on `import.annotation_status` — releases the old lock and records the new one.

After the commit the file selector automatically advances to the next unprocessed clip.

### 5.2 Annotation modes

**Binary mode**: the project targets one focal species per session. The verification question is simply "Is this species present in this clip?" The annotation status lock is scoped to `(audio_file_id, begin_time_ms, end_time_ms, target_species_id)`, allowing different users to independently annotate the same clip for different species.

**Full mode**: all species detected by BirdNET are presented simultaneously. The user may confirm, reject, or add species. The lock is scoped to the full time window, preventing two users from annotating the same clip simultaneously.

### 5.3 Model check — `get_glm_data()` / `plot_glm_check()`

The "Model Check" button opens a modal that assesses BirdNET's discrimination ability for the currently selected species. `get_glm_data()` joins `results` with `ground_truth_annotations` to build a binary outcome table (1 = true positive, 0 = false positive). It handles the asymmetry between annotation modes: full-mode annotations imply knowledge of all species in the clip, whereas binary-mode annotations only speak to the target species. Confidence scores are converted from stored smallints to [0, 1] and then to logit space, clamped to [0.001, 0.999].

`plot_glm_check()` fits a logistic regression (`outcome ~ logit_score`) and reports:
- **McFadden R²** as a colour-coded quality indicator (green ≥ 0.2, red < 0.05).
- **Sample size** with a warning if n < 30.
- The confidence threshold at which the model predicts p = 0.9, indicating the minimum score an annotator should trust for high-confidence automatic acceptance.

The tool guards against degenerate cases (fewer than five observations, all-positive or all-negative outcome vectors).

### 5.4 User management — `fct_manage_users.R`

Three functions cover the full user lifecycle:

- `add_users()`: validates username uniqueness and password length (≥ 8 characters), hashes the password with bcrypt, and in a single transaction inserts into `app_users`, `app_user_roles`, and `project_users`.
- `update_user()`: applies only the parameters that are explicitly provided — password rehash, role additions, or project additions/removals.
- `delete_user()`: requires `confirm = TRUE` and cascades automatically to `app_user_roles`.

`list_users()` returns a joined view of users and roles for administrative inspection.

### 5.5 Security architecture

evalpam uses two separate mechanisms for protecting credentials:

**User passwords** are hashed with bcrypt before being stored in `app_users`. The cost factor makes brute-force attacks computationally expensive. Passwords are never stored or logged in plaintext anywhere in the system.

**Database connection password** is stored in the OS keychain via the `keyring` R package and retrieved at runtime by `set_db_pool()`. This replaces the previous approach of base64-encoding the password into `.Renviron`, which offered no cryptographic protection. The `keyring` package delegates to the native secret store on each platform:

| Platform | Backend | Storage mechanism |
|---|---|---|
| Windows | Windows Credential Manager | DPAPI encryption, tied to the Windows user account |
| macOS | macOS Keychain | AES-256, protected by the login password |
| Linux desktop (GNOME) | libsecret / GNOME Keyring | AES-256, unlocked on login |
| Linux headless server | `keyring` file backend | AES-256, unlocked by `KEYRING_FILE_PASSWORD` env var |

**WaveSurfer.js** (version 7.8.17) and its three plugins are vendored inside the package at `inst/app/www/wavesurfer/` and served locally. No JavaScript is loaded from external CDNs at runtime, eliminating the supply-chain risk of a compromised CDN delivering arbitrary code into user sessions. The pinned version is auditable in git.

Non-secret connection parameters (host, port, database name, application username) are stored in `golem-config.yml` inside the installed package.

### 5.6 OS-specific setup for the keyring

The overall setup workflow is the same on all platforms — `setup_app()` stores the password and `set_db_pool()` retrieves it. What differs is whether the OS keychain needs any preparation beforehand.

#### Windows

No preparation required. Windows Credential Manager is available to all user accounts and works without any configuration. Run `setup_app()` as the user that will later run the Shiny app (or Shiny Server service account), and the credential will be stored under that account.

```r
setup_app(
  user             = "postgres",
  password         = "admin_pg_password",
  evalpam_username = "evalpam_user",
  evalpam_pw       = "strong_app_password",
  evalpam_dbname   = "evalpam_db",
  initialize_db    = TRUE
)
```

#### Linux — desktop (GNOME / KDE)

No preparation required if a keyring daemon is running (it is on any standard GNOME or KDE session). The `keyring` package uses `libsecret` to talk to the daemon over D-Bus. Run `setup_app()` from a normal user R session and the password will be stored in the login keyring, which is unlocked automatically when the user logs in.

```r
# Same call as on Windows — no extra steps needed
setup_app( ... )
```

If `libsecret-1-dev` is not installed, install it before installing the `keyring` package:

```bash
sudo apt install libsecret-1-dev   # Debian / Ubuntu
sudo dnf install libsecret-devel   # Fedora / RHEL
```

#### Linux — headless server (Shiny Server, no GUI)

A headless server has no keyring daemon running. The `keyring` file backend must be used instead. It stores the secret in an AES-encrypted file at `~/.local/share/r-keyring/` (owned by the user running the Shiny process), unlocked by a master password supplied via an environment variable.

**Step 1 — choose and protect a master password.** Create a file readable only by the Shiny service account:

```bash
sudo install -o shiny -g shiny -m 600 /dev/null /etc/evalpam-keyring.env
echo 'KEYRING_BACKEND=file' | sudo tee -a /etc/evalpam-keyring.env
echo 'KEYRING_FILE_PASSWORD=<strong-master-password>' | sudo tee -a /etc/evalpam-keyring.env
```

**Step 2 — tell the Shiny Server service to load these variables.** Create a systemd override:

```bash
sudo systemctl edit shiny-server
```

Add:

```ini
[Service]
EnvironmentFile=/etc/evalpam-keyring.env
```

Then reload: `sudo systemctl daemon-reload && sudo systemctl restart shiny-server`.

**Step 3 — run `setup_app()` as the shiny user** (so the keyring file is created in the right home directory):

```bash
sudo -u shiny Rscript -e "
  Sys.setenv(KEYRING_BACKEND = 'file',
             KEYRING_FILE_PASSWORD = '<strong-master-password>')
  library(evalpam)
  setup_app(
    user             = 'postgres',
    password         = 'admin_pg_password',
    evalpam_username = 'evalpam_user',
    evalpam_pw       = 'strong_app_password',
    evalpam_dbname   = 'evalpam_db',
    initialize_db    = TRUE
  )
"
```

After this, the encrypted keyring file lives at `/home/shiny/.local/share/r-keyring/evalpam.keyring`. When the Shiny Server starts the app, `KEYRING_BACKEND=file` and `KEYRING_FILE_PASSWORD` are injected from the env file, `set_db_pool()` calls `keyring::key_get("evalpam", ...)`, the file is decrypted in memory, and the password is passed to the connection pool — never written to disk in plaintext.

**Security properties of this arrangement**:
- The DB password on disk is AES-encrypted (not base64).
- The decryption key (`KEYRING_FILE_PASSWORD`) lives in a 600-permission file owned by the service account — the same protection level as a well-configured `.pgpass` or an SSH private key.
- Neither secret appears in any R source file, `.Renviron`, or `golem-config.yml`.

### 5.7 Deployment checklist

1. **Prepare keyring** (Linux server only): follow the steps in §5.6 before proceeding.
2. **Database setup**: `setup_app(user, password, ..., initialize_db = TRUE)` — stores DB password in keychain, writes `golem-config.yml`, runs all SQL scripts.
3. **Create users**: `add_users(pool, username, password, pg_role = "evalpam_user", project_ids = c(1))`.
4. **Ingest audio**: `retrieve_local_file_info()` → `upsert_deployments_sf()` → `upsert_audio_files_df()`.
5. **Run inference**: `process_deployment_birdnet()` → `upsert_results_df()` + `upsert_analysis_log_df()`.
6. **Generate clips**: `sample_results_table()` → `build_audio_clips_db()`.
7. **Launch app**: `evalpam::run_app()`.

BirdNET requires either a conda environment (`setup_birdnet_conda()`) or a reticulate-managed virtualenv (`check_birdnet_managed_env()`). Both helpers validate the Python version, install `birdnet==0.1.7` with `numpy<2.0.0`, and diagnose proxy issues for air-gapped networks.
