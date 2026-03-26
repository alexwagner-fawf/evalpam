#' Build GLM validation data for a species from annotated results
#'
#' Joins BirdNET predictions with ground truth annotations,
#' respecting annotation mode (full vs binary).
#'
#' @param pool DBI pool connection
#' @param project_id Integer. Project ID.
#' @param species_id Integer. Species to evaluate.
#' @return A data.frame with columns: confidence, logit_score, outcome (0/1)
#' @noRd
get_glm_data <- function(pool, project_id, species_id) {

  # Alle annotierten Snippets im Projekt holen (mit Modus-Info)
  # Full-Mode (target_species_id IS NULL): Wir wissen über ALLE Arten Bescheid
  # Binary-Mode (target_species_id = X): Wir wissen nur über Art X Bescheid
  query <- "
    WITH annotated_snippets AS (
      SELECT DISTINCT
        ast.audio_file_id,
        ast.begin_time_ms,
        ast.target_species_id
      FROM import.annotation_status ast
      JOIN import.audio_files af ON ast.audio_file_id = af.audio_file_id
      JOIN import.deployments d ON af.deployment_id = d.deployment_id
      WHERE d.project_id = $1
    ),
    -- BirdNET Predictions für die Zielart in annotierten Snippets
    predictions AS (
      SELECT
        r.audio_file_id,
        r.begin_time_ms,
        r.confidence,
        a.target_species_id
      FROM import.results r
      JOIN annotated_snippets a
        ON r.audio_file_id = a.audio_file_id
        AND r.begin_time_ms = a.begin_time_ms
      WHERE r.species_id = $2
        -- Nur verwenden wenn: Full-Mode ODER Binary mit passender Art
        AND (a.target_species_id IS NULL OR a.target_species_id = $2)
    ),
    -- Ground Truth: Wurde die Art bestätigt?
    ground_truth AS (
      SELECT DISTINCT audio_file_id, begin_time_ms
      FROM import.ground_truth_annotations
      WHERE species_id = $2
        AND is_present = TRUE
    )
    SELECT
      p.confidence,
      CASE WHEN gt.audio_file_id IS NOT NULL THEN 1 ELSE 0 END AS outcome
    FROM predictions p
    LEFT JOIN ground_truth gt
      ON p.audio_file_id = gt.audio_file_id
      AND p.begin_time_ms = gt.begin_time_ms
    ORDER BY p.confidence
  "

  df <- DBI::dbGetQuery(pool, query, params = list(project_id, species_id))

  if(nrow(df) == 0) return(df)

  # Confidence umrechnen: DB speichert als smallint (x10000)
  df$confidence <- df$confidence / 10000

  # Logit-Score (wie Wood & Kahl empfehlen)
  # Clamp um log(0) und log(Inf) zu vermeiden
  df$confidence_clamped <- pmin(pmax(df$confidence, 0.001), 0.999)
  df$logit_score <- log(df$confidence_clamped / (1 - df$confidence_clamped))

  df
}


#' Fit GLM and generate diagnostic plot
#'
#' @param df Data.frame from get_glm_data()
#' @param species_name Character. Name for plot title.
#' @return NULL (plots directly)
#' @noRd
plot_glm_check <- function(df, species_name = "Species") {

  n <- nrow(df)

  # --- Spezialfälle abfangen ---
  if(n < 5) {
    plot.new()
    text(0.5, 0.5, paste0("Zu wenig Daten (n = ", n, ")\nMindestens 5 Annotationen nötig."),
         cex = 1.4, col = "red")
    return(invisible(NULL))
  }

  if(length(unique(df$outcome)) < 2) {
    plot.new()
    text(0.5, 0.5, paste0("Nur ", ifelse(all(df$outcome == 1), "positive", "negative"),
                          " Annotationen (n = ", n, ")\nBeidse Outcomes nötig für GLM."),
         cex = 1.4, col = "orange")
    return(invisible(NULL))
  }

  # --- GLM fitten ---
  model <- glm(outcome ~ logit_score, data = df, family = binomial)
  null_model <- glm(outcome ~ 1, data = df, family = binomial)

  # McFadden R²
  mcfadden_r2 <- 1 - as.numeric(logLik(model)) / as.numeric(logLik(null_model))

  # Farbe für R²
  r2_col <- if(mcfadden_r2 < 0.1) "#d9534f"      # rot
  else if(mcfadden_r2 < 0.2) "#f0ad4e"  # orange
  else if(mcfadden_r2 < 0.4) "#5cb85c"  # grün
  else "#2e7d32"                          # dunkelgrün

  r2_label <- if(mcfadden_r2 < 0.1) "schlecht"
  else if(mcfadden_r2 < 0.2) "mäßig"
  else if(mcfadden_r2 < 0.4) "gut"
  else "exzellent"

  # Farbe für n
  n_col <- if(n < 30) "#d9534f"
  else if(n < 100) "#f0ad4e"
  else "#5cb85c"

  # --- Prediction Curve ---
  logit_range <- seq(min(df$logit_score) - 0.5, max(df$logit_score) + 0.5, length.out = 200)
  pred_df <- data.frame(logit_score = logit_range)
  pred_df$p <- predict(model, newdata = pred_df, type = "response")

  # Confidence-Achse (für zweite X-Achse)
  conf_from_logit <- function(x) 1 / (1 + exp(-x))

  # p=0.9 Threshold
  threshold_logit <- tryCatch({
    (log(0.9 / 0.1) - coef(model)[1]) / coef(model)[2]
  }, error = function(e) NA)

  threshold_conf <- if(!is.na(threshold_logit)) conf_from_logit(threshold_logit) else NA

  # --- Plot ---
  par(mar = c(5, 4, 4, 2) + 0.1)

  plot(df$logit_score, jitter(df$outcome, amount = 0.03),
       pch = 16, col = adjustcolor(ifelse(df$outcome == 1, "#5cb85c", "#d9534f"), 0.5),
       cex = 0.8,
       xlab = "BirdNET Logit Score", ylab = "P(True Positive)",
       main = species_name,
       ylim = c(-0.05, 1.05), las = 1)

  # Fitted Kurve
  lines(pred_df$logit_score, pred_df$p, col = "#337ab7", lwd = 2.5)

  # p=0.9 Linie
  abline(h = 0.9, lty = 2, col = "grey50")
  text(min(df$logit_score), 0.93, "p = 0.9", col = "grey50", adj = 0, cex = 0.8)

  # Threshold Markierung
  if(!is.na(threshold_logit) &&
     threshold_logit >= min(df$logit_score) &&
     threshold_logit <= max(df$logit_score)) {
    abline(v = threshold_logit, lty = 3, col = "#337ab7")
    text(threshold_logit, 0.05,
         paste0("Score ≥ ", round(threshold_conf, 3)),
         col = "#337ab7", adj = -0.1, cex = 0.8)
  }

  # Info Box
  legend("topleft",
         legend = c(
           paste0("McFadden R² = ", round(mcfadden_r2, 3), " (", r2_label, ")"),
           paste0("n = ", n),
           if(!is.na(threshold_conf)) paste0("Threshold (p=0.9): ", round(threshold_conf, 3)) else "Threshold: nicht berechenbar"
         ),
         text.col = c(r2_col, n_col, "#337ab7"),
         bty = "n", cex = 0.9)

  # Legende Punkte
  legend("bottomright",
         legend = c("True Positive", "False Positive"),
         pch = 16, col = c("#5cb85c", "#d9534f"),
         bty = "n", cex = 0.8)

  invisible(list(model = model, mcfadden_r2 = mcfadden_r2, n = n,
                 threshold_conf = threshold_conf))
}
