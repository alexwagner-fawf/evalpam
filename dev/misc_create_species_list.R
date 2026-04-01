

birds_en <- birdnetR::predict_species_at_location_and_time(birdnetR::birdnet_model_meta("v2.4", language = "en_us"), latitude =0, longitude = 0, min_confidence = 0.0) |>
  dplyr::mutate(common_name_en = stringr::str_split(label, "_", simplify = TRUE)[,2]) |>
  dplyr::mutate(latin_name = stringr::str_split(label, "_", simplify = TRUE)[,1]) |>
  dplyr::select(-label, -confidence)


birds_de <- birdnetR::predict_species_at_location_and_time(birdnetR::birdnet_model_meta("v2.4", language = "de"), latitude =0, longitude = 0, min_confidence = 0.0) |>
  dplyr::mutate(common_name_de = stringr::str_split(label, "_", simplify = TRUE)[,2]) |>
  dplyr::mutate(latin_name = stringr::str_split(label, "_", simplify = TRUE)[,1]) |>
  dplyr::select(-label, -confidence)


birds_table <- birds_en |>
  dplyr::left_join(birds_de, by = "latin_name") |>
  dplyr::arrange(latin_name) |>
  dplyr::rename(species_scientific = latin_name) |>
  dplyr::rename(species_de = common_name_de) |>
  dplyr::rename(species_en = common_name_en) |>
  readr::write_csv("inst/app/data/long_species_list.csv")


#
# gbif_ids <- sapply(birds_en$latin_name, rgbif::name_backbone)
#
#
#
# gbif_table <- gbif_ids |>
#   lapply(function(x){
#     x[1,]
#   }) |>
#   do.call(what = dplyr::bind_rows)
