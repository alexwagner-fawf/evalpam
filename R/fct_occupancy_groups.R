#' Create or update an occupancy group and assign spectrograms to it
#'
#' @param pool DBI pool
#' @param project_id integer
#' @param group_name character, unique per project
#' @param target_count integer, number of certain detections to stop at
#' @param audio_file_ids integer vector of audio_file_ids whose spectrograms
#'   should be assigned to this group
#' @param description optional character
#' @return integer group_id
#' @export
create_occupancy_group <- function(pool, project_id, group_name,
                                   target_count, audio_file_ids,
                                   description = NULL) {
  stopifnot(
    length(group_name) == 1, nchar(group_name) > 0,
    target_count >= 1,
    length(audio_file_ids) > 0
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
                           list(project_id, group_name, as.integer(target_count), description)
    )$group_id

    DBI::dbExecute(conn,
                   "INSERT INTO import.spectrogram_groups (spectrogram_id, group_id)
       SELECT s.spectrogram_id, $1
       FROM import.spectrograms s
       WHERE s.audio_file_id = ANY($2::bigint[])
       ON CONFLICT DO NOTHING",
                   list(gid, as.integer(audio_file_ids))
    )

    gid
  })
}
