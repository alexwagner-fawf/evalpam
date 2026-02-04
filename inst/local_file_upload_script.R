pool <- set_db_pool()

project_id <- upsert_project(pool,
                             "test",
                             "test123",
                             description = "123",
                             contact = "23",
                             organisation = "123")

out <- retrieve_local_file_info(project_id,
                                project_folder = "~/Dokumente/sound_db/project_1/",
                                folder_depth = 1)

# WARNING!!! THIS WILL INJECT RANDOM COORDINATES ATM

deployment_ids <- out$deployment_index |>
  readr::read_csv() |>
  dplyr::mutate(geometry_x_4326 = runif(dplyr::n(), 7, 8)) |>
  dplyr::mutate(geometry_y_4326 = runif(dplyr::n(), 48, 50)) |>
  dplyr::mutate(valid = TRUE) |>
  sf::st_as_sf(coords = c("geometry_x_4326", "geometry_y_4326")) |>
  sf::st_set_crs(4326) |>
  upsert_deployments_sf(conn = pool)

# check the uploaded data
deployments <- dplyr::tbl(pool, DBI::Id("import", "deployments")) |>
  dplyr::filter(deployment_id %in% deployment_ids) |>
  dplyr::collect()


# if there are new deployments (aka audio_file_indices csvs) decide whether to upload
# only this information or all available audio file data. this will not introduce
# duplicates to the database as audio files are checked for duplicates by deployment_id
# and timestamp but may take more time. if you are unsure whether all previous audio file
# indices have been uploaded, just use out$all_audio_file_indices

if(length(out$new_audio_file_indices) == 0){
  audio_file_indices <- out$all_audio_file_indices
}else{
  audio_file_indices <- out$new_audio_file_indices
}

# convert audio file indices so it matches the
df <- out$all_audio_file_indices |>
  lapply(readr::read_csv) |>
  dplyr::bind_rows() |>
  dplyr::rename(deployment_name = deployment_id) |>
  dplyr::left_join(deployments |> dplyr::select(deployment_id, deployment_name)) |>
  dplyr::relocate(deployment_id, .before = "deployment_name") |>
  dplyr::select(-deployment_name)


# upload audio file indices, retrieve ids of upload
audio_file_ids <- upsert_audio_files_df(conn = pool,
                                        df_audio = df,
                                        update_if_exists = TRUE)

# check the uploaded data (glimpse to avoid large download)
audio_files <- dplyr::tbl(pool, DBI::Id("import", "audio_files")) |>
  dplyr::filter(audio_file_id %in% audio_file_ids) |>
  dplyr::glimpse() |>
  dplyr::collect()

pool::poolClose(pool)

