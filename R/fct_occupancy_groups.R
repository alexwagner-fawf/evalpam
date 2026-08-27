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


# ---------------------------------------------------------------------------
# Internal: enriched per-spectrogram metadata for a project, with convenience
# time columns pre-derived so callers can group on them directly.
# ---------------------------------------------------------------------------
.spectrogram_group_metadata <- function(pool, project_id,
                                        species_ids = NULL,
                                        deployment_ids = NULL) {
  where <- "d.project_id = $1"
  params <- list(as.integer(project_id))
  if (!is.null(species_ids)) {
    params[[length(params) + 1L]] <- .bigint_array(species_ids)
    where <- paste0(where, " AND r.species_id = ANY($", length(params), "::bigint[])")
  }
  if (!is.null(deployment_ids)) {
    params[[length(params) + 1L]] <- .bigint_array(deployment_ids)
    where <- paste0(where, " AND af.deployment_id = ANY($", length(params), "::bigint[])")
  }

  meta <- DBI::dbGetQuery(pool, paste0(
    "SELECT s.spectrogram_id, s.audio_file_id, s.begin_time_ms, s.result_id,
            r.species_id, ls.species_scientific,
            af.deployment_id, d.deployment_name,
            ST_AsText(d.geometry) AS site,
            af.timestamp_start + (s.begin_time_ms || ' milliseconds')::interval
              AS detection_time
     FROM import.spectrograms s
     JOIN import.audio_files af ON af.audio_file_id = s.audio_file_id
     JOIN import.deployments  d ON d.deployment_id  = af.deployment_id
     LEFT JOIN import.results r  ON r.result_id = s.result_id
     LEFT JOIN public.lut_species_code ls ON ls.species_id = r.species_id
     WHERE ", where), params = params)

  if (nrow(meta) == 0L) return(meta)

  t   <- meta$detection_time
  iso_year <- as.integer(format(t, "%G"))
  iso_week <- as.integer(format(t, "%V"))
  # Convenience partitioning columns (all character so they read well in names).
  meta$date    <- as.character(as.Date(t))
  meta$year    <- format(t, "%Y")
  meta$month   <- format(t, "%Y-%m")
  meta$week    <- sprintf("%d-W%02d", iso_year, iso_week)
  meta$biweek  <- sprintf("%d-BW%02d", iso_year, (iso_week + 1L) %/% 2L)
  meta
}


#' Create occupancy groups for a project by a flexible, user-defined partition
#'
#' Standalone batch wrapper around \code{\link{create_occupancy_group}}: it
#' pulls every spectrogram of a project (with convenience metadata), splits them
#' into partitions defined entirely by the caller, and creates (or idempotently
#' updates) one occupancy group per partition. Each group carries the same
#' \code{target_count}, so the app's occupancy auto-stop skips a group's
#' remaining clips once \code{target_count} confirmations of the queue's target
#' species accrue.
#'
#' The partition is whatever \code{group_by} says, which keeps the grouping
#' policy in the R user's hands: group by species and month, by several
#' deployments lumped into a site, bi-weekly, etc.
#'
#' @param pool A DBI pool.
#' @param project_id Integer. Owning project.
#' @param group_by How to partition the spectrograms. Either:
#'   \itemize{
#'     \item a character vector of metadata column names to cross — available
#'       columns are \code{spectrogram_id}, \code{audio_file_id},
#'       \code{result_id}, \code{species_id}, \code{species_scientific},
#'       \code{deployment_id}, \code{deployment_name}, \code{site} (geometry
#'       WKT), \code{detection_time}, and the pre-derived \code{date},
#'       \code{year}, \code{month}, \code{week} (ISO year-week) and
#'       \code{biweek}; or
#'     \item a function taking the metadata data frame and returning a vector of
#'       group labels (one per row) — use this for anything the columns do not
#'       express directly, e.g. lumping several deployments together or custom
#'       time bins.
#'   }
#' @param target_count Integer >= 1. Confirmation threshold applied to every
#'   created group. Default 2.
#' @param name_prefix Character. Prefix for auto-generated group names. Default
#'   \code{"auto"}. Final names are \code{<name_prefix>_<partition key>},
#'   sanitised and truncated to fit \code{varchar(100)} (with a hash suffix if
#'   truncation would collide).
#' @param species_ids,deployment_ids Optional integer vectors to restrict the
#'   spectrograms considered before partitioning. \code{NULL} (default) = all.
#' @param dry_run Logical. When \code{TRUE}, build and return the partition plan
#'   without writing anything to the database. Default \code{FALSE}.
#' @param verbose Logical. Print a one-line summary. Default \code{TRUE}.
#'
#' @return A tibble with one row per group: \code{group_name}, \code{group_id}
#'   (\code{NA} on \code{dry_run}), \code{n_spectrograms}, \code{target_count}
#'   and \code{group_key}. Returned invisibly unless \code{dry_run}.
#'
#' @examples
#' \dontrun{
#' pool <- set_db_pool()
#' # species x deployment x month, confirmed after 2 verifications
#' groups_from_spectrograms(pool, project_id = 1,
#'                          group_by = c("species_id", "deployment_id", "month"))
#' # bi-weekly per species, preview only
#' groups_from_spectrograms(pool, 1, group_by = c("species_id", "biweek"),
#'                          dry_run = TRUE)
#' # lump several deployments into one "plot" via a function
#' plot_of <- function(df) paste0(df$species_id, "_plot",
#'                                ifelse(df$deployment_id %in% c(2, 3), "A", "B"))
#' groups_from_spectrograms(pool, 1, group_by = plot_of, target_count = 3)
#' }
#'
#' @seealso \code{\link{create_occupancy_group}}
#' @export
groups_from_spectrograms <- function(pool, project_id, group_by,
                                     target_count   = 2L,
                                     name_prefix    = "auto",
                                     species_ids    = NULL,
                                     deployment_ids = NULL,
                                     dry_run        = FALSE,
                                     verbose        = TRUE) {
  stopifnot(length(project_id) == 1L, target_count >= 1L,
            length(name_prefix) == 1L, nzchar(name_prefix))
  species_ids    <- .normalise_id_filter(species_ids)
  deployment_ids <- .normalise_id_filter(deployment_ids)

  meta <- .spectrogram_group_metadata(pool, project_id, species_ids, deployment_ids)
  if (nrow(meta) == 0L) {
    stop("No spectrograms found for project_id=", project_id,
         " (after any species/deployment filter).")
  }

  # ── Resolve the per-row partition key ───────────────────────────────────────
  # `bad` marks rows to drop for an incomplete key. For column-based grouping we
  # can detect that from NA in the source columns; for a user function we trust
  # its output and only drop literal NA labels.
  if (is.function(group_by)) {
    keys <- group_by(meta)
    if (length(keys) != nrow(meta)) {
      stop("`group_by` function must return one label per row (got ",
           length(keys), " for ", nrow(meta), " spectrograms).")
    }
    keys <- as.character(keys)
    bad  <- is.na(keys)
  } else if (is.character(group_by) && length(group_by) >= 1L) {
    missing_cols <- setdiff(group_by, names(meta))
    if (length(missing_cols)) {
      stop("`group_by` columns not available: ", paste(missing_cols, collapse = ", "),
           ". Available: ", paste(names(meta), collapse = ", "), ".")
    }
    key_parts <- lapply(group_by, function(cc) as.character(meta[[cc]]))
    keys <- do.call(paste, c(key_parts, sep = "__"))
    bad  <- Reduce(`|`, lapply(group_by, function(cc) is.na(meta[[cc]])))
  } else {
    stop("`group_by` must be a non-empty character vector of column names ",
         "or a function(meta_df) returning one label per row.")
  }

  # Drop rows whose key is incomplete (e.g. grouping by species where result_id,
  # hence species_id, is NULL for some spectrograms).
  if (any(bad)) {
    if (verbose) message(sum(bad), " spectrogram(s) dropped: incomplete grouping key.")
    meta <- meta[!bad, , drop = FALSE]
    keys <- keys[!bad]
  }
  if (length(keys) == 0L) stop("No spectrograms left after dropping incomplete keys.")

  # ── Build unique, DB-safe group names per partition ─────────────────────────
  split_ids <- split(meta$spectrogram_id, keys)
  raw_keys  <- names(split_ids)
  safe      <- gsub("[^A-Za-z0-9_.-]+", "_", paste0(name_prefix, "_", raw_keys))
  # Truncate to varchar(100); disambiguate any collisions with a short hash of
  # the full key so distinct partitions never merge into one group.
  trunc <- substr(safe, 1L, 100L)
  dup   <- trunc %in% trunc[duplicated(trunc)]
  if (any(dup)) {
    h <- vapply(raw_keys[dup],
                function(k) digest::digest(k, algo = "xxhash64"),
                character(1))
    trunc[dup] <- paste0(substr(trunc[dup], 1L, 91L), "_", substr(h, 1L, 8L))
  }

  plan <- dplyr::tibble(
    group_name     = trunc,
    group_key      = raw_keys,
    n_spectrograms = unname(lengths(split_ids)),
    target_count   = as.integer(target_count),
    group_id       = NA_integer_
  )

  short <- plan$n_spectrograms < target_count
  if (any(short) && verbose) {
    message(sum(short), " group(s) have fewer than target_count=", target_count,
            " spectrograms and can never auto-confirm.")
  }

  if (dry_run) {
    if (verbose) message("dry_run: ", nrow(plan), " group(s) planned, nothing written.")
    return(plan)
  }

  for (i in seq_len(nrow(plan))) {
    plan$group_id[i] <- create_occupancy_group(
      pool, project_id,
      group_name      = plan$group_name[i],
      target_count    = target_count,
      spectrogram_ids = split_ids[[plan$group_key[i]]],
      description     = paste0("auto: group_key=", plan$group_key[i])
    )
  }

  if (verbose) {
    message(nrow(plan), " occupancy group(s) created/updated for project ",
            project_id, " (target_count=", target_count, ").")
  }
  invisible(plan)
}
