#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # ============================================================================
  # Bird Sound Verification App - Server Logic
  # ===========================================================================
  this_table <- reactiveValues(data = meta_data)

    # Authentication ----
    res_auth <- secure_server(
      check_credentials = check_credentials(credentials)
    )

    # Video Control ----
    observeEvent(input$seq, {
      changeVideo("video", input$seq)
      seekVideo("video", seek = 5)
      playVideo("video")
    })

    # Filter Data for Current Sequence ----
    df_filter <- reactive({
      this_table$data %>%
        filter(path == input$seq) %>%
        arrange(desc(score))
    })

    # Warning Text for Previously Edited Sequences ----
    warning_text <- reactive({
      req(input$seq)

      check_temp <- this_table$data %>%
        filter(path == input$seq)

      if (nrow(check_temp) > 0 && !is.null(check_temp$timestamp)) {
        check_temp2 <- check_temp %>%
          distinct(score, timestamp) %>%
          slice(1)

        if (is.na(check_temp2$score)) {
          return(paste("Du hast diese Sequenz bereits bearbeitet am", check_temp2$timestamp))
        }
      }

      return("")
    })

    # Update Species Selection ----
    observeEvent(input$seq, {
      x <- df_filter()

      if (!is.null(x) && nrow(x) > 0) {
        selected_species <- if (is.na(x$species_to_check[1])) {
          x$prediction[1]
        } else {
          x$species_to_check[1]
        }

        updateMultiInput(
          session,
          "inSelect",
          label = "Vogelarten auswählen:",
          choices = arten,
          selected = selected_species
        )
      }
    })

    # Save Verification ----
    observeEvent(input$add_btn, {
      req(input$seq)

      # Prepare data to save
      old_df <- this_table$data %>%
        filter(path != input$seq)

      add_df <- this_table$data %>%
        filter(path == input$seq) %>%
        distinct(path, .keep_all = TRUE) %>%
        mutate(species_to_check = NA)

      # Set prediction based on input
      if (is.null(input$inSelect) || length(input$inSelect) == 0) {
        add_df$prediction <- "falsch"
      } else {
        add_df$prediction <- list(input$inSelect)
        add_df <- add_df %>%
          tidyr::unnest(cols = c(prediction))
      }

      # Update reactive data
      this_table$data <- rbind(add_df, old_df)

      # Export to CSV
      export_df <- add_df %>%
        mutate(test_file = NA) %>%
        select(
          verification = prediction,
          id_seq,
          start,
          end,
          test_file,
          path,
          plot_info
        ) %>%
        mutate(
          timestamp = lubridate::now(),
          verification_by = res_auth$user
        )

      # Append to CSV
      write_csv(
        export_df,
        EXPORT_FILE,
        append = TRUE
      )

      # Navigate to next sequence
      current_idx <- which(audio_files$path == input$seq)
      next_idx <- current_idx + 1

      if (next_idx > nrow(audio_files)) {
        showModal(modalDialog(
          title = "Fertig!",
          "Alle Sequenzen wurden angesehen!",
          easyClose = TRUE
        ))
      } else {
        next_path <- audio_files$path[next_idx]

        changeVideo("video", next_path)
        updateSelectizeInput(
          session,
          "seq",
          choices = mylist,
          selected = next_path
        )
        seekVideo("video", seek = 5)
        playVideo("video")
      }
    })

    # Outputs ----

    output$user_info <- renderUI({
      tags$h3(paste0("Hallo ", res_auth$user, "!"))
    })

    output$text1 <- renderText({
      warning_text()
    })

    output$table_bnet <- renderDataTable({
      req(df_filter())

      df <- df_filter()

      if (nrow(df) > 0 && !is.na(df$species_to_check[1])) {
        highlight_row <- which(df$species_to_check == df$prediction)[1]

        datatable(
          df %>% select(species_to_check, prediction, score),
          options = list(
            dom = 't',
            columnDefs = list(list(visible = FALSE, targets = "species_to_check"))
          ),
          rownames = TRUE
        ) %>%
          formatRound(columns = 'score', digits = 3) %>%
          formatStyle(
            0,
            target = "row",
            backgroundColor = styleEqual(highlight_row, "lightgrey")
          )
      }
    })

    output$plot_info <- renderUI({
      req(df_filter())

      tags$h5(
        df_filter() %>%
          distinct(plot_info) %>%
          pull(plot_info)
      )
    })

    output$map <- renderUI({
      req(df_filter())

      url_temp <- df_filter() %>%
        distinct(plot_info) %>%
        tidyr::separate(plot_info, into = c("id", "id_2", "id_3"), sep = " ") %>%
        left_join(coords, by = "id") %>%
        pull(url)

      tagList(
        "Aufnahmeort: ",
        a("Karte öffnen", href = url_temp, target = "_blank")
      )
    })
}
