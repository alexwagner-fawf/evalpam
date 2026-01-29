pool <- set_db_pool()

project_id <- upsert_project(pool, "test", "test123", description = "123", contact = "23", organisation = "123")

out <- retrieve_local_file_info(project_id,
                                project_folder = "~/Dokumente/sound_db/project_1/",
                                folder_depth = 1)

out$deployment_index |>
  readr::read_csv() |>
  dplyr::mutate(geometry_x_4326 = runif(dplyr::n(), 0, 10)) |>
  dplyr::mutate(geometry_y_4326 = runif(dplyr::n(), 40, 50)) |>
  dplyr::mutate(valid = TRUE) |>
  sf::st_as_sf(coords = c("geometry_x_4326", "geometry_y_4326")) |>
  sf::st_set_crs(4326) |>
  upsert_deployments_sf(conn = pool)

deployments <- DBI::dbReadTable(pool, DBI::Id("import", "deployments"))

df <- out$audio_file_indices |>
  lapply(readr::read_csv) |>
  dplyr::bind_rows() |>
  dplyr::rename(deployment_name = deployment_id) |>
  dplyr::left_join(deployments |> dplyr::select(deployment_id, deployment_name)) |>
  dplyr::relocate(deployment_id, .before = "deployment_name") |>
  dplyr::select(-deployment_name)

upsert_audio_files_df(conn = pool, df_audio = df)

pool::poolClose(pool)

