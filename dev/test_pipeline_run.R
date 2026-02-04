# ==============================================================================
# MASTER TEST PIPELINE: AUDIO INGEST -> BIRDNET ANALYSIS -> DB IMPORT -> SPECTROGRAMS
# ==============================================================================
# This script simulates the full workflow from raw audio files to a ready-to-use app.
# It handles database setup, file scanning, analysis, and spectrogram generation.

# ------------------------------------------------------------------------------
# 1. SETUP & CONFIGURATION
# ------------------------------------------------------------------------------

# Load necessary libraries
library(pool)
library(DBI)
library(dplyr)
library(readr)
library(fst)        # Fast storage for intermediate results
library(birdnetR)   # BirdNET wrapper

# Load the local package functions
devtools::load_all()

# --- Configuration Paths ---
# IMPORTANT: Adjust AUDIO_PATH to your local folder.
# Files must be inside a subfolder (Deployment), e.g., .../raw_test_audio/Test_Deployment_1/
AUDIO_PATH  <- "J:/Woe/Projekt/WSG/Projekte_Sachdaten/1418_FFK/4_Module/Modul_Audiomotten/Daten/raw_test_audio/"
SPECTRO_OUT <- "spectrograms_test"
FST_STORAGE <- "temp_birdnet_results"

# Create output directories if they don't exist
if(!dir.exists(SPECTRO_OUT)) dir.create(SPECTRO_OUT)
if(!dir.exists(FST_STORAGE)) dir.create(FST_STORAGE)

# ------------------------------------------------------------------------------
# 2. DATABASE CONNECTION & INITIALIZATION
# ------------------------------------------------------------------------------

# Close any existing pool connections to prevent locks/leaks
if(exists("pool") && pool::isPool(pool)) pool::poolClose(pool)

# Initialize the Application DB
# ACHTUNG: Passwörter hier anpassen!
evalpam:::setup_app(
  user = "xxxx",
  host = "fvafrsd-6v.forst.bwl.de",
  port = 5432,
  maintenance_dbname = "postgres",
  password = "xxxx",              # <--- PASSWORD REDACTED
  evalpam_username = "evalpam_user",
  evalpam_pw = "xxxx",            # <--- PASSWORD REDACTED
  evalpam_dbname = "evalpam_db",
  admin_mailaddress = "admin@test.com",
  dummy = TRUE,          # Set to TRUE for testing environment
  initialize_db = TRUE,  # Create tables
  renviron_dir = NULL
)

# Create the standard connection pool for the script
pool <- evalpam:::set_db_pool()

message("--- 2. Database Initialized & Connected ---")


# ------------------------------------------------------------------------------
# 3. PERMISSIONS & ADMIN USER SETUP
# ------------------------------------------------------------------------------

# A) Set Permissions (Requires Admin Connection)
# ------------------------------------------------------
admin_conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = "evalpam_db",
  host = "fvafrsd-6v.forst.bwl.de",
  port = 5432,
  user = "xxxx",
  password = "xxxx"               # <--- PASSWORD REDACTED
)

message("Setting permissions for 'evalpam_user'...")

# Grant full rights on PUBLIC schema
DBI::dbExecute(admin_conn, "GRANT USAGE ON SCHEMA public TO evalpam_user;")
DBI::dbExecute(admin_conn, "GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO evalpam_user;")
DBI::dbExecute(admin_conn, "GRANT USAGE, SELECT, UPDATE ON ALL SEQUENCES IN SCHEMA public TO evalpam_user;")

# Grant full rights on IMPORT schema
DBI::dbExecute(admin_conn, "GRANT USAGE ON SCHEMA import TO evalpam_user;")
DBI::dbExecute(admin_conn, "GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA import TO evalpam_user;")
DBI::dbExecute(admin_conn, "GRANT USAGE, SELECT, UPDATE ON ALL SEQUENCES IN SCHEMA import TO evalpam_user;")

DBI::dbDisconnect(admin_conn)
message("Permissions set successfully.")

# B) Create App User (if not exists)
# ------------------------------------------------------
tryCatch({
  evalpam:::add_users(
    pool = pool,
    username = "mein_admin",
    password = "xxxx",            # <--- PASSWORD REDACTED
    pg_role = "evalpam_admin",
    email = "admin@test.com",
    first_name = "Admin",
    last_name = "User",
    expire_date = NA
  )
  message("User 'mein_admin' created.")
}, error = function(e) {
  message("User creation skipped (likely already exists): ", e$message)
})


# ------------------------------------------------------------------------------
# 4. PROJECT SETUP
# ------------------------------------------------------------------------------
message("--- 4. Project Setup ---")

# Check if project exists, get ID if yes, create if no
existing_proj <- DBI::dbGetQuery(pool, "SELECT project_id FROM import.projects WHERE project_name_short = 'FFK_Test'")

if(nrow(existing_proj) > 0) {
  PROJECT_ID <- existing_proj$project_id[1]
  message("Using existing Project ID: ", PROJECT_ID)
} else {
  PROJECT_ID <- upsert_project(
    conn = pool,
    project_name_short = "FFK_Test",
    project_name_long = "FFK Test Project 2024",
    description = "Test Run for Pipeline Development",
    contact = NA,
    organisation = NA
  )
  message("Created new Project ID: ", PROJECT_ID)
}

# Assign User to Project
DBI::dbExecute(pool, "
  INSERT INTO public.project_users (project_id, user_id)
  SELECT $1, user_id FROM public.app_users WHERE username = 'mein_admin'
  ON CONFLICT DO NOTHING;",
               params = list(PROJECT_ID)
)


# ------------------------------------------------------------------------------
# 5. METADATA SETUP (Lookups & Settings)
# ------------------------------------------------------------------------------




# A) Analysis Settings
DBI::dbExecute(pool, "
  INSERT INTO import.settings (settings_id, model_name, min_conf, overlap, model_params)
  VALUES (1, 'BirdNET_v2.4', 0.1, 0.0, '{}')
  ON CONFLICT (settings_id) DO NOTHING;
")


# ------------------------------------------------------------------------------
# 6. AUDIO INGEST
# ------------------------------------------------------------------------------
message("--- 6. Audio Ingest ---")

# A) Scan Folder Structure
indexing <- retrieve_local_file_info(
  project_id = PROJECT_ID,
  project_folder = AUDIO_PATH,
  folder_depth = 1
)

# B) Import Deployments
if(file.exists(indexing$deployment_index)) {
  df_dep <- readr::read_csv(indexing$deployment_index, show_col_types = FALSE)
  for(k in 1:nrow(df_dep)) {
    DBI::dbExecute(pool, "
      INSERT INTO import.deployments (project_id, deployment_name, deployment_path, valid)
      VALUES ($1, $2, $3, TRUE)
      ON CONFLICT (deployment_name) DO UPDATE SET deployment_path = EXCLUDED.deployment_path",
                   params = list(PROJECT_ID, df_dep$deployment_name[k], df_dep$deployment_path[k])
    )
  }
}

# C) Import Audio Files
audio_csvs <- list.files(AUDIO_PATH, pattern = "_audio_file_index.csv", recursive = TRUE, full.names = TRUE)

# Definiere hier, welchen Typ du für den Test erzwingen willst!
# 1 = FULL_SEG, 2 = TARGET_SEG, etc. (siehe deine Tabelle lut_annotation_type_code)
TEST_ANNOTATION_TYPE_ID <- 1

for(csv in audio_csvs) {
  df_audio <- readr::read_csv(csv, show_col_types = FALSE)

  if(nrow(df_audio) > 0) {
    # Deployment ID auflösen
    dep_name <- as.character(unique(df_audio$deployment_id)[1])
    res_id <- DBI::dbGetQuery(pool, "SELECT deployment_id FROM import.deployments WHERE deployment_name = $1", params=list(dep_name))

    if(nrow(res_id) > 0) {
      df_audio$deployment_id <- res_id$deployment_id[1]
      # Wir setzen hart die ID für alle Files in diesem Import
      df_audio$required_annotation_type_id <- TEST_ANNOTATION_TYPE_ID
      # --------------------------

      upsert_audio_files_df(conn = pool, df_audio = df_audio)
    } else {
      warning("Deployment not found in DB: ", dep_name)
    }
  }
}

# ------------------------------------------------------------------------------
# 6b. POPULATE SETTINGS SPECIES (WHITELIST)
# ------------------------------------------------------------------------------
message("--- 6b. Populating Settings Species (Whitelist) ---")

meine_arten <- c("Amsel", "Buchfink", "Kohlmeise", "Rotkehlchen")

ids_df <- DBI::dbGetQuery(pool,
                          "SELECT species_id, species_long_de FROM public.lut_species_code WHERE species_long_de IN ($1, $2, $3, $4)",
                          params = as.list(meine_arten)
)
print(ids_df)

settings_id <- 1
for(sid in ids_df$species_id) {
  DBI::dbExecute(pool,
                 "INSERT INTO import.settings_species (settings_id, species_id) VALUES ($1, $2) ON CONFLICT DO NOTHING",
                 params = list(settings_id, sid)
  )
}


# ------------------------------------------------------------------------------
# 7. BIRDNET ANALYSIS
# ------------------------------------------------------------------------------
message("--- 7. Running BirdNET Analysis ---")

my_setup <- setup_birdnet_model(
  version = "v2.4",
  min_confidence = 0.5,
  tflite_num_threads = 1,
  latitude = 48.0,
  longitude = 7.8,
  week = 20
)

# Fetch files to process
query_files <- "
  SELECT af.audio_file_id, af.relative_path, d.deployment_path
  FROM import.audio_files af
  JOIN import.deployments d ON af.deployment_id = d.deployment_id
"
files_to_run <- DBI::dbGetQuery(pool, query_files)
message("Files to analyze: ", nrow(files_to_run))

# Processing Loop
count <- 0
if(nrow(files_to_run) > 0) {
  for(i in 1:nrow(files_to_run)) {
    row <- files_to_run[i, ]
    clean_rel <- sub("^/+|\\\\+", "", row$relative_path)
    full_path <- file.path(row$deployment_path, clean_rel)

    if(file.exists(full_path)) {
      message("Analyzing: ", clean_rel)
      try({
        res_obj <- apply_birdnet_model(
          audio_file = full_path,
          birdnet_setup = my_setup,
          birdnet_params = list(min_confidence = 0.1, apply_sigmoid = TRUE)
        )
        res_df <- res_obj$prediction_raw

        if(!is.null(res_df) && nrow(res_df) > 0) {
          res_df$audio_file_id <- row$audio_file_id
          fst::write_fst(res_df, file.path(FST_STORAGE, paste0("result_", row$audio_file_id, ".fst")))
        }
        count <- count + 1
      })
    } else {
      message("Skipping (File not found): ", full_path)
    }
  }
}
message("Analysis complete. Processed files: ", count)


# ------------------------------------------------------------------------------
# 8. IMPORT RESULTS TO DB
# ------------------------------------------------------------------------------
message("--- 8. Importing Results (Confidence >= 0.9) ---")

fst_files <- list.files(FST_STORAGE, pattern = "\\.fst$", full.names = TRUE)

if(length(fst_files) > 0) {
  all_res <- do.call(rbind, lapply(fst_files, fst::read_fst))

  top_res <- all_res %>%
    filter(confidence >= 0.9) %>%
    mutate(settings_id = 1,
           begin_time_ms = as.integer(start * 1000),
           end_time_ms = as.integer(end * 1000))

  lut_sp <- dbGetQuery(pool, "SELECT species_id, species_scientific FROM public.lut_species_code")

  final_db <- top_res %>%
    inner_join(lut_sp, by = c("scientific_name" = "species_scientific")) %>%
    select(audio_file_id, settings_id, begin_time_ms, end_time_ms, confidence, species_id)

  if(nrow(final_db) > 0) {
    upsert_results_df(pool, final_db)
    message("Imported ", nrow(final_db), " detections into DB.")
  } else {
    message("No detections above threshold.")
  }
}


# ------------------------------------------------------------------------------
# 9. SPECTROGRAM GENERATION
# ------------------------------------------------------------------------------
message("--- 9. Generating Spectrograms ---")

q_spec <- "
  SELECT r.result_id, r.audio_file_id, r.species_id, r.confidence, r.begin_time_ms
  FROM import.results r
  WHERE r.confidence >= 0.9
  LIMIT 5
"
to_render <- DBI::dbGetQuery(pool, q_spec)

if(nrow(to_render) > 0) {
  build_spectrogram_db(data = to_render, pool = pool, padding_s = 5, output_dir = SPECTRO_OUT)
  message("Spectrograms generated in: ", SPECTRO_OUT)
} else {
  message("No detections found to render.")
}


# ------------------------------------------------------------------------------
# 10. LAUNCH APP
# ------------------------------------------------------------------------------
message("--- Pipeline Finished. Launching App... ---")

# Set folder variable for App
Sys.setenv(spectrogram_folder = file.path(getwd(), SPECTRO_OUT))

devtools::load_all()
evalpam::run_app()

# ==============================================================================
# DEBUGGING / MANUAL CHECKS (Execute line by line manually)
# ==============================================================================
#
# # 1. Check User ID
# users <- DBI::dbGetQuery(pool, "SELECT user_id, username FROM public.app_users")
# print(users)
#
# # 2. Check Annotations for User 1
# meine_echte_id <- 1
# check <- DBI::dbGetQuery(pool, "SELECT count(*) as anzahl FROM import.ground_truth_annotations WHERE user_id = $1", params = list(meine_echte_id))
# print(paste("Gespeicherte Einträge:", check$anzahl))
#
# # 3. Test Manual Insert
# tryCatch({
#   DBI::dbExecute(pool, "
#     INSERT INTO import.ground_truth_annotations
#     (audio_file_id, user_id, species_id, begin_time_ms, end_time_ms, is_present)
#     VALUES (1, 1, 1, 0, 1000, TRUE)
#   ")
#   print("INSERT erfolgreich!")
# }, error = function(e) {
#   print(paste("FEHLER:", e$message))
# })
#
# # 4. Cleanup / Reset
# aid <- 1       # Audio File ID
# user <- 1      # Deine User ID
# start <- 0     # Startzeit in ms
#
# # Vögel weg
# DBI::dbExecute(pool, "DELETE FROM import.ground_truth_annotations WHERE user_id = $1 AND begin_time_ms = $2", list(user, start))
# # Status weg
# DBI::dbExecute(pool, "DELETE FROM import.annotation_status WHERE user_id = $1", list(user))
