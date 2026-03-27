# ==============================================================
# 1. Mode-Mixing: Full zuerst, dann Binary → Fehler
# ==============================================================
test_that("Trigger verhindert Binary nach Full auf demselben Snippet", {
  pool <- get_test_pool()
  on.exit(pool::poolClose(pool), add = TRUE)
  con <- with_test_transaction(pool)

  af <- DBI::dbGetQuery(con, "SELECT audio_file_id FROM import.audio_files LIMIT 1")
  skip_if(nrow(af) == 0, "No audio files in DB")
  usr <- DBI::dbGetQuery(con, "SELECT user_id FROM public.app_users LIMIT 1")
  skip_if(nrow(usr) == 0, "No users in DB")
  sp <- DBI::dbGetQuery(con, "SELECT species_id FROM public.lut_species_code LIMIT 1")
  skip_if(nrow(sp) == 0, "No species in DB")

  test_begin <- 999000L
  test_end   <- 999999L

  # Full-Eintrag
  DBI::dbExecute(con,
                 "INSERT INTO import.annotation_status (audio_file_id, user_id, begin_time_ms, end_time_ms, annotation_type_id, target_species_id)
     VALUES ($1, $2, $3, $4, 1, NULL)",
                 list(af$audio_file_id[1], usr$user_id[1], test_begin, test_end))

  # Binary muss fehlschlagen
  expect_error(
    DBI::dbExecute(con,
                   "INSERT INTO import.annotation_status (audio_file_id, user_id, begin_time_ms, end_time_ms, annotation_type_id, target_species_id)
       VALUES ($1, $2, $3, $4, 1, $5)",
                   list(af$audio_file_id[1], usr$user_id[1], test_begin, test_end, sp$species_id[1])),
    regexp = "Mode-Konflikt|mode.konflikt|conflict",
    ignore.case = TRUE
  )
})

# ==============================================================
# 2. Mode-Mixing: Binary zuerst, dann Full → Fehler
# ==============================================================
test_that("Trigger verhindert Full nach Binary auf demselben Snippet", {
  pool <- get_test_pool()
  on.exit(pool::poolClose(pool), add = TRUE)
  con <- with_test_transaction(pool)

  af <- DBI::dbGetQuery(con, "SELECT audio_file_id FROM import.audio_files LIMIT 1")
  skip_if(nrow(af) == 0)
  usr <- DBI::dbGetQuery(con, "SELECT user_id FROM public.app_users LIMIT 1")
  skip_if(nrow(usr) == 0)
  sp <- DBI::dbGetQuery(con, "SELECT species_id FROM public.lut_species_code LIMIT 1")
  skip_if(nrow(sp) == 0)

  test_begin <- 998000L
  test_end   <- 998999L

  # Binary-Eintrag zuerst
  DBI::dbExecute(con,
                 "INSERT INTO import.annotation_status (audio_file_id, user_id, begin_time_ms, end_time_ms, annotation_type_id, target_species_id)
     VALUES ($1, $2, $3, $4, 1, $5)",
                 list(af$audio_file_id[1], usr$user_id[1], test_begin, test_end, sp$species_id[1]))

  # Full muss fehlschlagen
  expect_error(
    DBI::dbExecute(con,
                   "INSERT INTO import.annotation_status (audio_file_id, user_id, begin_time_ms, end_time_ms, annotation_type_id, target_species_id)
       VALUES ($1, $2, $3, $4, 1, NULL)",
                   list(af$audio_file_id[1], usr$user_id[1], test_begin, test_end)),
    regexp = "Mode-Konflikt|mode.konflikt|conflict",
    ignore.case = TRUE
  )
})

# ==============================================================
# 3. Binary: Zwei verschiedene Arten auf gleichem Snippet → OK
# ==============================================================
test_that("Binary erlaubt verschiedene Arten auf gleichem Snippet", {
  pool <- get_test_pool()
  on.exit(pool::poolClose(pool), add = TRUE)
  con <- with_test_transaction(pool)

  af <- DBI::dbGetQuery(con, "SELECT audio_file_id FROM import.audio_files LIMIT 1")
  skip_if(nrow(af) == 0)
  usr <- DBI::dbGetQuery(con, "SELECT user_id FROM public.app_users LIMIT 1")
  skip_if(nrow(usr) == 0)
  sps <- DBI::dbGetQuery(con, "SELECT species_id FROM public.lut_species_code LIMIT 2")
  skip_if(nrow(sps) < 2, "Need at least 2 species")

  test_begin <- 997000L
  test_end   <- 997999L

  expect_no_error(
    DBI::dbExecute(con,
                   "INSERT INTO import.annotation_status (audio_file_id, user_id, begin_time_ms, end_time_ms, annotation_type_id, target_species_id)
       VALUES ($1, $2, $3, $4, 1, $5)",
                   list(af$audio_file_id[1], usr$user_id[1], test_begin, test_end, sps$species_id[1]))
  )

  expect_no_error(
    DBI::dbExecute(con,
                   "INSERT INTO import.annotation_status (audio_file_id, user_id, begin_time_ms, end_time_ms, annotation_type_id, target_species_id)
       VALUES ($1, $2, $3, $4, 1, $5)",
                   list(af$audio_file_id[1], usr$user_id[1], test_begin, test_end, sps$species_id[2]))
  )
})

# ==============================================================
# 4. Binary: Gleiche Art doppelt auf gleichem Snippet → Fehler (EXCLUDE)
# ==============================================================
test_that("EXCLUDE verhindert doppelte Art im Binary-Mode", {
  pool <- get_test_pool()
  on.exit(pool::poolClose(pool), add = TRUE)
  con <- with_test_transaction(pool)

  af <- DBI::dbGetQuery(con, "SELECT audio_file_id FROM import.audio_files LIMIT 1")
  skip_if(nrow(af) == 0)
  usr <- DBI::dbGetQuery(con, "SELECT user_id FROM public.app_users LIMIT 1")
  skip_if(nrow(usr) == 0)
  sp <- DBI::dbGetQuery(con, "SELECT species_id FROM public.lut_species_code LIMIT 1")
  skip_if(nrow(sp) == 0)

  test_begin <- 996000L
  test_end   <- 996999L

  DBI::dbExecute(con,
                 "INSERT INTO import.annotation_status (audio_file_id, user_id, begin_time_ms, end_time_ms, annotation_type_id, target_species_id)
     VALUES ($1, $2, $3, $4, 1, $5)",
                 list(af$audio_file_id[1], usr$user_id[1], test_begin, test_end, sp$species_id[1]))

  expect_error(
    DBI::dbExecute(con,
                   "INSERT INTO import.annotation_status (audio_file_id, user_id, begin_time_ms, end_time_ms, annotation_type_id, target_species_id)
       VALUES ($1, $2, $3, $4, 1, $5)",
                   list(af$audio_file_id[1], usr$user_id[1], test_begin, test_end, sp$species_id[1])),
    regexp = "exclude|overlap|duplicate|conflict",
    ignore.case = TRUE
  )
})

# ==============================================================
# 5. Certainty: Default-Wert und FK funktionieren
# ==============================================================
test_that("ground_truth_annotations akzeptiert certainty_id", {
  pool <- get_test_pool()
  on.exit(pool::poolClose(pool), add = TRUE)
  con <- with_test_transaction(pool)

  af <- DBI::dbGetQuery(con, "SELECT audio_file_id FROM import.audio_files LIMIT 1")
  skip_if(nrow(af) == 0)
  usr <- DBI::dbGetQuery(con, "SELECT user_id FROM public.app_users LIMIT 1")
  skip_if(nrow(usr) == 0)
  sp <- DBI::dbGetQuery(con, "SELECT species_id FROM public.lut_species_code LIMIT 1")
  skip_if(nrow(sp) == 0)

  # Insert mit expliziter certainty_id
  expect_no_error(
    DBI::dbExecute(con,
                   "INSERT INTO import.ground_truth_annotations
       (audio_file_id, user_id, species_id, begin_time_ms, end_time_ms, is_present, certainty_id)
       VALUES ($1, $2, $3, 990000, 990999, TRUE, 3)",
                   list(af$audio_file_id[1], usr$user_id[1], sp$species_id[1]))
  )

  # Insert ohne certainty_id → Default 1
  expect_no_error(
    DBI::dbExecute(con,
                   "INSERT INTO import.ground_truth_annotations
       (audio_file_id, user_id, species_id, begin_time_ms, end_time_ms, is_present)
       VALUES ($1, $2, $3, 991000, 991999, TRUE)",
                   list(af$audio_file_id[1], usr$user_id[1], sp$species_id[1]))
  )

  row <- DBI::dbGetQuery(con,
                         "SELECT certainty_id FROM import.ground_truth_annotations WHERE audio_file_id = $1 AND begin_time_ms = 991000",
                         list(af$audio_file_id[1]))
  expect_equal(row$certainty_id[1], 1L)

  # Ungültige certainty_id → FK-Fehler
  expect_error(
    DBI::dbExecute(con,
                   "INSERT INTO import.ground_truth_annotations
       (audio_file_id, user_id, species_id, begin_time_ms, end_time_ms, is_present, certainty_id)
       VALUES ($1, $2, $3, 992000, 992999, TRUE, 99)",
                   list(af$audio_file_id[1], usr$user_id[1], sp$species_id[1])),
    regexp = "foreign key|fkey|violates",
    ignore.case = TRUE
  )
})


