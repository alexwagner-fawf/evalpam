#' Helpers to create and populate import.occupancy_groups
#'
#' The primary, lowest-level function is `create_occupancy_group()`, which takes
#' a vector of `spectrogram_id`s. Higher-level convenience wrappers build that
#' vector from different selection criteria (audio file, deployment+time,
#' top-N detections, embedding similarity, ...).
#'
#' Design rationale: a Spektrogramm is the natural unit of observation for
#' Occupancy work — not the audio file. A 7-minute recording may contain
#' dozens of 3-second Schnipsel, and the right ones may be characterised by
#' time window, BirdNET score, or acoustic similarity rather than file
#' identity. Pinning the API to `spectrogram_ids` keeps the abstraction clean.
#'
#' @name occupancy_groups
NULL


# ---------------------------------------------------------------------------
# Internal: turn an R integer vector into a Postgres bigint[] literal.
# RPostgres does not reliably send length-1 vectors as arrays.
# ---------------------------------------------------------------------------
.bigint_array <- function(x) {
  paste0("{", paste(as.integer(x), collapse = ","), "}")
}


#' Create or update an occupancy group from explicit spectrogram IDs
#'
#' Primary, lowest-level interface. All wrappers funnel into this.
#'
#' Idempotent: re-running with the same `project_id` + `group_name` updates
#' `target_count` and `description`, and appends any new spectrograms to the
#' group (existing memberships are kept; `ON CONFLICT DO NOTHING`).
#'
#' @param pool A DBI pool.
#' @param project_id Integer. Owning project.
#' @param group_name Character. Unique within project.
#' @param target_count Integer >= 1. Stop threshold (number of certain
#'   detections of the queue/target species required to mark group as done).
#' @param spectrogram_ids Integer vector. Spectrograms to assign to the group.
#'   Must reference rows that exist in `import.spectrograms`.
#' @param description Optional character. Free-form note.
#'
#' @return The integer `group_id`.
#' @export
create_occupancy_group <- function(pool, project_id, group_name,
                                   target_count, spectrogram_ids,
                                   description = NULL) {
  stopifnot(
    length(group_name) == 1L, nchar(group_name) > 0L,
    target_count >= 1L,
    length(spectrogram_ids) > 0L,
    all(!is.na(spectrogram_ids))
  )

  pool::poolWithTransaction(pool, function(conn) {
    gid <- DBI::dbGetQuery(conn,
                           "INSERT INTO import.occupancy_groups
         (project_id, group_name, target_count, description)
       VALUES ($1, $2, $3, $4)
       ON CONFLICT (project_id, group_name) DO UPDATE
         SET target_count = EXCLUDED.target_count,
             description  = COALESCE(EXCLUDED.description, occupancy_groups.description)
       RETURNING group_id",
                           params = list(as.integer(project_id),
                                         group_name,
                                         as.integer(target_count),
                                         description)
    )$group_id

    DBI::dbExecute(conn,
                   "INSERT INTO import.spectrogram_groups (spectrogram_id, group_id)
       SELECT spectrogram_id, $1
       FROM import.spectrograms
       WHERE spectrogram_id = ANY($2::bigint[])
       ON CONFLICT DO NOTHING",
                   params = list(gid, .bigint_array(spectrogram_ids))
    )

    as.integer(gid)
  })
}


#' Wrapper: group containing all spectrograms of one or more audio files
#'
#' Convenience for the classical plot-level case where each deployment has a
#' single recording and you want every 3-second Schnipsel of those recordings
#' in the group.
#'
#' @inheritParams create_occupancy_group
#' @param audio_file_ids Integer vector of `audio_file_id`s.
#' @export
group_from_audio_files <- function(pool, project_id, group_name, target_count,
                                   audio_file_ids, description = NULL) {
  stopifnot(length(audio_file_ids) > 0L)

  spec_ids <- DBI::dbGetQuery(pool,
                              "SELECT spectrogram_id
     FROM import.spectrograms
     WHERE audio_file_id = ANY($1::bigint[])",
                              params = list(.bigint_array(audio_file_ids))
  )$spectrogram_id

  if (length(spec_ids) == 0L) {
    stop("No spectrograms found for the given audio_file_ids.")
  }

  create_occupancy_group(pool, project_id, group_name, target_count,
                         spec_ids, description)
}


#' Wrapper: group containing spectrograms of a deployment in a daily time window
#'
#' Filters spectrograms by their absolute timestamp's hour-of-day, derived from
#' the audio file's `timestamp_start` plus the Schnipsel's `begin_time_ms`.
#' Useful for stratified Occupancy designs ("morning vs. midday").
#'
#' @inheritParams create_occupancy_group
#' @param deployment_name Character. Must match `deployments.deployment_name`.
#' @param hour_from,hour_to Integer 0–23. Inclusive bounds on hour-of-day.
#' @export
group_from_deployment_time <- function(pool, project_id, group_name, target_count,
                                       deployment_name, hour_from, hour_to,
                                       description = NULL) {
  stopifnot(hour_from >= 0L, hour_to <= 23L, hour_from <= hour_to)

  spec_ids <- DBI::dbGetQuery(pool,
                              "SELECT s.spectrogram_id
     FROM import.spectrograms s
     JOIN import.audio_files af USING (audio_file_id)
     JOIN import.deployments d  USING (deployment_id)
     WHERE d.deployment_name = $1
       AND EXTRACT(HOUR FROM af.timestamp_start
                   + (s.begin_time_ms || ' milliseconds')::interval)
           BETWEEN $2 AND $3",
                              params = list(deployment_name, as.integer(hour_from), as.integer(hour_to))
  )$spectrogram_id

  if (length(spec_ids) == 0L) {
    stop(sprintf("No spectrograms found for deployment '%s' between hours %d and %d.",
                 deployment_name, hour_from, hour_to))
  }

  create_occupancy_group(pool, project_id, group_name, target_count,
                         spec_ids, description)
}


#' Wrapper: group of the top-N highest-confidence BirdNET detections
#'
#' Selects the N spectrograms whose associated `import.results` row has the
#' highest `confidence` for the given species in the given project.
#'
#' @inheritParams create_occupancy_group
#' @param species_id Integer. Restrict to detections of this species.
#' @param n_top Integer. How many top-confidence detections to keep.
#' @export
group_from_top_detections <- function(pool, project_id, group_name, target_count,
                                      species_id, n_top, description = NULL) {
  stopifnot(n_top >= 1L)

  spec_ids <- DBI::dbGetQuery(pool,
                              "SELECT s.spectrogram_id
     FROM import.spectrograms s
     JOIN import.results r ON r.result_id = s.result_id
     JOIN import.audio_files af USING (audio_file_id)
     JOIN import.deployments d  USING (deployment_id)
     WHERE d.project_id = $1
       AND r.species_id = $2
     ORDER BY r.confidence DESC
     LIMIT $3",
                              params = list(as.integer(project_id),
                                            as.integer(species_id),
                                            as.integer(n_top))
  )$spectrogram_id

  if (length(spec_ids) == 0L) {
    stop(sprintf("No detections found for species_id %d in project %d.",
                 species_id, project_id))
  }

  create_occupancy_group(pool, project_id, group_name, target_count,
                         spec_ids, description)
}


#' Wrapper: group of the K spectrograms most similar to a reference (embedding-based)
#'
#' Requires the `vector` extension and an `embedding` column on
#' `import.spectrograms`. Returns the K spectrograms with the smallest
#' cosine distance to the reference spectrogram's embedding.
#'
#' Disabled-friendly: throws a clear error if the embedding column is missing,
#' so the rest of the codebase can run without pgvector installed.
#'
#' @inheritParams create_occupancy_group
#' @param reference_spec_id Integer. The "seed" spectrogram whose embedding
#'   defines the similarity centre.
#' @param top_k Integer. Number of nearest neighbours to retrieve (includes
#'   the reference itself).
#' @export
group_from_embedding_neighbours <- function(pool, project_id, group_name, target_count,
                                            reference_spec_id, top_k,
                                            description = NULL) {
  stopifnot(top_k >= 1L)

  has_embedding <- DBI::dbGetQuery(pool,
                                   "SELECT 1 FROM information_schema.columns
     WHERE table_schema = 'import'
       AND table_name   = 'spectrograms'
       AND column_name  = 'embedding'"
  )
  if (nrow(has_embedding) == 0L) {
    stop("Column import.spectrograms.embedding does not exist. ",
         "Install the pgvector extension and run the embedding migration first.")
  }

  spec_ids <- DBI::dbGetQuery(pool,
                              "SELECT spectrogram_id
     FROM import.spectrograms
     WHERE embedding IS NOT NULL
     ORDER BY embedding <=> (
       SELECT embedding FROM import.spectrograms WHERE spectrogram_id = $1
     )
     LIMIT $2",
                              params = list(as.integer(reference_spec_id), as.integer(top_k))
  )$spectrogram_id

  if (length(spec_ids) == 0L) {
    stop("Reference spectrogram has no embedding, or no neighbours available.")
  }

  create_occupancy_group(pool, project_id, group_name, target_count,
                         spec_ids, description)
}
