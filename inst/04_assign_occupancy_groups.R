# =============================================================================
# Assign occupancy groups (the "stop criterion")
# =============================================================================
# An occupancy group bundles spectrograms and a target_count: once a group has
# target_count certain-present verifications of its target species, the app's
# occupancy auto-stop drops the group's remaining clips from the queue and
# offers to skip to the next group.
#
# For the full screening -> calibration pipeline see
#   inst/05_two_phase_occupancy_workflow.R
#
# Prerequisite: migration inst/sql/44_add_occupancy_groups.sql applied.
# =============================================================================

library(evalpam)

project_id <- 1
pool <- set_db_pool()
on.exit(pool::poolClose(pool), add = TRUE)

# -----------------------------------------------------------------------------
# A) Recommended: partition the project's spectrograms automatically.
# -----------------------------------------------------------------------------
# `groups_from_spectrograms()` creates one group per partition. `group_by` is
# either metadata column names to cross, or a function(meta_df) -> group labels.
# Available columns: species_id, deployment_id, site (geometry), deployment_name,
# date, year, month, week, biweek, ...

# One group per species x location, confirmed after 2 verifications:
groups_from_spectrograms(pool, project_id,
                         group_by     = c("species_id", "deployment_id"),
                         target_count = 2L)

# Preview only (write nothing) with dry_run = TRUE:
# groups_from_spectrograms(pool, project_id,
#                          group_by = c("species_id", "biweek"),
#                          dry_run = TRUE)

# Custom partition via a function (e.g. lump deployments 2 & 3 into one plot):
# plot_of <- function(df) paste0(df$species_id, "_plot",
#                                ifelse(df$deployment_id %in% c(2, 3), "A", "B"))
# groups_from_spectrograms(pool, project_id, group_by = plot_of, target_count = 3L)


# -----------------------------------------------------------------------------
# B) Manual: build a single group from an explicit set of clips.
# -----------------------------------------------------------------------------
# All spectrograms of one plot's audio files:
# plot01_ids <- DBI::dbGetQuery(pool,
#   "SELECT af.audio_file_id FROM import.audio_files af
#      JOIN import.deployments d USING (deployment_id)
#     WHERE d.deployment_name = 'Plot_01'")$audio_file_id
# group_from_audio_files(pool, project_id,
#                        group_name = "Plot_01", target_count = 3L,
#                        audio_file_ids = plot01_ids)

# See also: group_from_deployment_time(), group_from_top_detections(),
#           group_from_embedding_neighbours(), create_occupancy_group().
