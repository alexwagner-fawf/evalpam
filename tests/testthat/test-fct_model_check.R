# ==============================================================
# 6. get_glm_data: Korrekte Spalten und Wertebereiche
# ==============================================================
test_that("get_glm_data liefert korrekte Spalten", {
  pool <- get_test_pool()
  on.exit(pool::poolClose(pool), add = TRUE)

  proj <- DBI::dbGetQuery(pool, "SELECT project_id FROM import.projects LIMIT 1")
  skip_if(nrow(proj) == 0, "No projects in DB")
  sp <- DBI::dbGetQuery(pool, "SELECT species_id FROM public.lut_species_code LIMIT 1")
  skip_if(nrow(sp) == 0)

  df <- get_glm_data(pool, proj$project_id[1], sp$species_id[1])

  if (nrow(df) > 0) {
    expect_true(all(c("confidence", "logit_score", "outcome") %in% names(df)))
    expect_true(all(df$outcome %in% c(0L, 1L)))
    expect_true(all(df$confidence >= 0 & df$confidence <= 1))
  }
})
