#' Upsert a project into the import.projects table
#'
#' @param conn A valid DBI pool connection
#' @param project_name_short Short project name (max 30 chars)
#' @param project_name_long Long project name (max 150 chars)
#' @param description Text description
#' @param contact Contact info
#' @param organisation Organisation name
#' @param project_id Optional. If provided, will update that project; if NULL, will insert new
#'
#' @return The project_id of the inserted or updated project
upsert_project <- function(conn,
                           project_name_short,
                           project_name_long,
                           description = NULL,
                           contact = NULL,
                           organisation = NULL,
                           project_id = NULL) {

  if (!is.null(project_id)) {
    # UPDATE existing project
    sql <- glue::glue_sql(
      "UPDATE import.projects
       SET project_name_short = {project_name_short},
           project_name_long = {project_name_long},
           description = {description},
           contact = {contact},
           organisation = {organisation}
       WHERE project_id = {project_id}
       RETURNING project_id;",
      .con = conn
    )
    res <- DBI::dbGetQuery(conn, sql)

    if (nrow(res) == 0) {
      stop("No project found with project_id = ", project_id)
    }
    return(res$project_id)

  } else {
    # INSERT new project
    sql <- glue::glue_sql(
      "INSERT INTO import.projects
       (project_name_short, project_name_long, description, contact, organisation)
       VALUES ({project_name_short}, {project_name_long}, {description}, {contact}, {organisation})
       RETURNING project_id;",
      .con = conn
    )

    res <- DBI::dbGetQuery(conn, sql)
    return(res$project_id)
  }
}


#' Upsert deployments from an sf object
#'
#' @param conn A valid DBI pool connection
#' @param sf_deployments An sf object with columns matching import.deployments:
#'        project_id, deployment_name, deployment_path, start_datetime, end_datetime,
#'        device_manufacturer, device_modelname, valid, notes, deployment_id (optional)
#'        and a geometry column.
#' @param update_if_exists Logical. If TRUE, will update rows where deployment_id exists.
#'
#' @return A vector of deployment_ids for inserted/updated rows
upsert_deployments_sf <- function(conn, sf_deployments, update_if_exists = TRUE) {
  stopifnot("sf" %in% class(sf_deployments))

  # Convert geometry column to WKT upfront
  sf_deployments$geom_wkt <- sf::st_as_text(sf_deployments$geometry)

  # Drop geometry column and convert to data frame
  df <- sf::st_drop_geometry(sf_deployments)

  # Write to temporary table
  DBI::dbWriteTable(conn, "temp_deployments",
                    df, temporary = TRUE, overwrite = TRUE)

  if (update_if_exists) {
    # UPSERT: insert new, update existing based on deployment_name
    sql <- "
      INSERT INTO import.deployments
        (project_id, deployment_name, deployment_path, start_datetime,
         end_datetime, device_manufacturer, device_modelname, valid, notes, geometry)
      SELECT
        project_id, deployment_name, deployment_path, start_datetime,
        end_datetime, device_manufacturer, device_modelname, valid, notes,
        ST_GeomFromText(geom_wkt, 4326)
      FROM temp_deployments
      ON CONFLICT (deployment_name)
      DO UPDATE SET
        project_id = EXCLUDED.project_id,
        deployment_path = EXCLUDED.deployment_path,
        start_datetime = EXCLUDED.start_datetime,
        end_datetime = EXCLUDED.end_datetime,
        device_manufacturer = EXCLUDED.device_manufacturer,
        device_modelname = EXCLUDED.device_modelname,
        valid = EXCLUDED.valid,
        notes = EXCLUDED.notes,
        geometry = EXCLUDED.geometry
      RETURNING deployment_id;"
  } else {
    # INSERT only (will fail if deployment_name already exists)
    sql <- "
      INSERT INTO import.deployments
        (project_id, deployment_name, deployment_path, start_datetime,
         end_datetime, device_manufacturer, device_modelname, valid, notes, geometry)
      SELECT
        project_id, deployment_name, deployment_path, start_datetime,
        end_datetime, device_manufacturer, device_modelname, valid, notes,
        ST_GeomFromText(geom_wkt, 4326)
      FROM temp_deployments
      RETURNING deployment_id;"
  }

  result <- DBI::dbGetQuery(conn, sql)

  return(result$deployment_id)
}


#' Upsert settings into import.settings from a data.frame
#'
#' @param conn A valid DBI pool connection
#' @param df_settings data.frame with columns:
#'        model_name, model_version (optional), min_conf, overlap,
#'        locale (optional), model_params (list or character/JSON string)
#' @param update_if_exists Logical. If TRUE, will update existing rows on conflict.
#'        Note: Since there's no UNIQUE constraint, this will always insert.
#'
#' @return A vector of settings_id for inserted rows
upsert_settings_df <- function(conn, df_settings, update_if_exists = TRUE) {
  stopifnot(is.data.frame(df_settings))

  # Ensure required columns exist
  required_cols <- c("model_name", "min_conf", "overlap", "model_params")
  missing_cols <- setdiff(required_cols, names(df_settings))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Select only the columns we need
  cols_to_use <- c("model_name", "min_conf", "overlap", "model_params")

  # Add optional columns if they exist
  if ("model_version" %in% names(df_settings)) {
    cols_to_use <- c(cols_to_use, "model_version")
  } else {
    df_settings$model_version <- NA_character_
    cols_to_use <- c(cols_to_use, "model_version")
  }

  if ("locale" %in% names(df_settings)) {
    cols_to_use <- c(cols_to_use, "locale")
  } else {
    df_settings$locale <- NA_character_
    cols_to_use <- c(cols_to_use, "locale")
  }

  # Convert model_params to JSON string if it's a list
  if ("model_params" %in% names(df_settings)) {
    df_settings$model_params <- vapply(df_settings$model_params, function(x) {
      if (is.list(x) || is.data.frame(x)) {
        jsonlite::toJSON(x, auto_unbox = TRUE)
      } else if (is.character(x)) {
        x
      } else {
        stop("model_params must be a list, data.frame, or JSON string")
      }
    }, character(1))
  }

  # Select only the columns we want to write
  df_temp <- df_settings[, cols_to_use, drop = FALSE]

  # Write to temporary table
  DBI::dbWriteTable(conn, "temp_settings",
                    df_temp, temporary = TRUE, overwrite = TRUE)

  # Since there's no UNIQUE constraint, we always INSERT
  # (duplicates are allowed per your requirement)
  sql <- "
    INSERT INTO import.settings
      (model_name, model_version, min_conf, overlap, locale, model_params)
    SELECT
      model_name, model_version, min_conf, overlap, locale,
      model_params::jsonb
    FROM temp_settings
    RETURNING settings_id;"

  result <- DBI::dbGetQuery(conn, sql)
  return(result$settings_id)
}

#' Replace species associations for a settings_id
#'
#' Deletes all existing species associations for the given settings_id,
#' then inserts new associations. This ensures clean replacement of the
#' many-to-many relationship.
#'
#' @param conn A valid DBI pool connection
#' @param settings_id Integer. The settings_id to update
#' @param species_ids Integer vector of species_id values to associate
#'
#' @return Invisible NULL (operation succeeds or throws error)
#' @export
#'
#' @examples
#' # Replace all species for settings_id = 5
#' replace_settings_species(conn = pool, settings_id = 5, species_ids = c(10, 20, 30))
#'
#' # Clear all species for settings_id = 5 (pass empty vector)
#' replace_settings_species(conn = pool, settings_id = 5, species_ids = integer(0))
replace_settings_species <- function(conn, settings_id, species_ids) {
  stopifnot(is.numeric(settings_id) && length(settings_id) == 1)
  stopifnot(is.numeric(species_ids) || is.integer(species_ids))

  # Use poolWithTransaction for pool objects
  pool::poolWithTransaction(conn, function(conn_tx) {
    # Step 1: Delete all existing associations for this settings_id
    delete_sql <- "DELETE FROM import.settings_species WHERE settings_id = $1"
    DBI::dbExecute(conn_tx, delete_sql, params = list(settings_id))

    # Step 2: Insert new associations (if any)
    if (length(species_ids) > 0) {
      # Create data frame for new associations
      df_new <- data.frame(
        settings_id = rep(settings_id, length(species_ids)),
        species_id = species_ids
      )

      # Write to temporary table
      DBI::dbWriteTable(conn_tx, "temp_settings_species",
                        df_new, temporary = TRUE, overwrite = TRUE)

      # Insert from temp table
      insert_sql <- "
        INSERT INTO import.settings_species (settings_id, species_id)
        SELECT settings_id, species_id
        FROM temp_settings_species"

      DBI::dbExecute(conn_tx, insert_sql)
    }
  })

  invisible(NULL)
}

#' Upsert audio files into import.audio_files from a data.frame
#'
#' @param conn A valid DBI pool connection
#' @param df_audio data.frame with columns:
#'        deployment_id, sample_rate, relative_path, file_deleted (logical),
#'        timestamp_start (POSIXct or character), duration_s (integer),
#'        audio_file_id (optional, for updates)
#' @param update_if_exists Logical. If TRUE, will update rows with audio_file_id.
#'
#' @return A vector of audio_file_id for inserted/updated rows
upsert_audio_files_df <- function(conn, df_audio, update_if_exists = TRUE) {
  stopifnot(is.data.frame(df_audio))

  # Ensure required columns exist
  required_cols <- c("deployment_id", "sample_rate", "relative_path",
                     "timestamp_start", "duration_s")
  missing_cols <- setdiff(required_cols, names(df_audio))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Select only the columns we need (avoid extra columns like audio_file_id causing issues)
  cols_to_use <- c("deployment_id", "sample_rate", "relative_path",
                   "timestamp_start", "duration_s")

  # Add optional columns if they exist
  if ("deleted" %in% names(df_audio)) {
    cols_to_use <- c(cols_to_use, "deleted")
  } else {
    df_audio$deleted <- FALSE
    cols_to_use <- c(cols_to_use, "deleted")
  }

  if ("required_annotation_type_id" %in% names(df_audio)) {
    cols_to_use <- c(cols_to_use, "required_annotation_type_id")
  } else {
    df_audio$required_annotation_type_id <- NA_integer_
    cols_to_use <- c(cols_to_use, "required_annotation_type_id")
  }

  # Select only the columns we want to write
  df_temp <- df_audio[, cols_to_use, drop = FALSE]

  # Write to temporary table
  DBI::dbWriteTable(conn, "temp_audio_files",
                    df_temp, temporary = TRUE, overwrite = TRUE)

  if (update_if_exists) {
    # UPSERT: insert new, update existing based on (deployment_id, timestamp_start)
    sql <- "
      INSERT INTO import.audio_files
        (deployment_id, sample_rate, relative_path, deleted,
         timestamp_start, duration_s, required_annotation_type_id)
      SELECT
        deployment_id, sample_rate, relative_path, deleted,
        timestamp_start, duration_s, required_annotation_type_id
      FROM temp_audio_files
      ON CONFLICT (deployment_id, timestamp_start)
      DO UPDATE SET
        sample_rate = EXCLUDED.sample_rate,
        relative_path = EXCLUDED.relative_path,
        deleted = EXCLUDED.deleted,
        duration_s = EXCLUDED.duration_s,
        required_annotation_type_id = EXCLUDED.required_annotation_type_id
      RETURNING audio_file_id;"
  } else {
    # INSERT only (will fail if (deployment_id, timestamp_start) already exists)
    sql <- "
      INSERT INTO import.audio_files
        (deployment_id, sample_rate, relative_path, deleted,
         timestamp_start, duration_s, required_annotation_type_id)
      SELECT
        deployment_id, sample_rate, relative_path, deleted,
        timestamp_start, duration_s, required_annotation_type_id
      FROM temp_audio_files
      RETURNING audio_file_id;"
  }

  result <- DBI::dbGetQuery(conn, sql)
  return(result$audio_file_id)
}


#' Upsert results into import.results from a data.frame
#'
#' @param conn A valid DBI pool connection
#' @param df_results data.frame with columns:
#'        audio_file_id, settings_id, begin_time_ms, end_time_ms,
#'        confidence, species_id, behavior_id
#' @param update_if_exists Logical. If TRUE, will update rows matching unique constraint
#'
#' @return A vector of result_id for inserted/updated rows
upsert_results_df <- function(conn, df_results, update_if_exists = TRUE) {
  stopifnot(is.data.frame(df_results))

  # Required columns
  required_cols <- c("audio_file_id", "settings_id", "begin_time_ms",
                     "end_time_ms", "confidence", "species_id")
  missing_cols <- setdiff(required_cols, names(df_results))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Add optional column behavior_id if missing
  if (!"behavior_id" %in% names(df_results)) {
    df_results$behavior_id <- NA_integer_
  }

  # Select only columns we care about
  cols_to_use <- c("audio_file_id", "settings_id", "begin_time_ms",
                   "end_time_ms", "confidence", "species_id", "behavior_id")
  df_temp <- df_results[, cols_to_use, drop = FALSE]

  # Write to temporary table
  DBI::dbWriteTable(conn, "temp_results",
                    df_temp, temporary = TRUE, overwrite = TRUE)

  if (update_if_exists) {
    # UPSERT: insert new, update existing based on unique constraint
    sql <- "
      INSERT INTO import.results
        (audio_file_id, settings_id, begin_time_ms, end_time_ms,
         confidence, species_id, behavior_id)
      SELECT
        audio_file_id, settings_id, begin_time_ms, end_time_ms,
        confidence, species_id, behavior_id
      FROM temp_results
      ON CONFLICT (audio_file_id, settings_id, begin_time_ms, end_time_ms, species_id)
      DO UPDATE SET
        begin_time_ms = EXCLUDED.begin_time_ms,
        end_time_ms = EXCLUDED.end_time_ms,
        confidence = EXCLUDED.confidence,
        behavior_id = EXCLUDED.behavior_id
      RETURNING result_id;"
  } else {
    # INSERT only
    sql <- "
      INSERT INTO import.results
        (audio_file_id, settings_id, begin_time_ms, end_time_ms,
         confidence, species_id, behavior_id)
      SELECT
        audio_file_id, settings_id, begin_time_ms, end_time_ms,
        confidence, species_id, behavior_id
      FROM temp_results
      RETURNING result_id;"
  }

  result <- DBI::dbGetQuery(conn, sql)
  return(result$result_id)
}

