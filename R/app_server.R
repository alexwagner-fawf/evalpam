app_server <- function(input, output, session) {

  pool <- set_db_pool()

  # ---- Authentication ----
  res_auth <- shinymanager::secure_server(
    check_credentials = check_credentials_db(pool)
  )

  # ---- Reactive Table ----
  this_table <- reactiveValues(data = getOption("evalpam.data")$data)

  # ---- Video Control ----
  observeEvent(input$seq, {
    video::changeVideo("video", input$seq)
    video::seekVideo("video", seek = 5)
    video::playVideo("video")
  })


  # ---- Filtered Data for Current Sequence ----
  df_filter <- reactive({
    this_table$data |>
      dplyr::filter(path == input$seq) |>
      dplyr::arrange(dplyr::desc(score))
  })


  # ---- Previously edited warning ----
  warning_text <- reactive({
    req(input$seq)

    check_temp <- this_table$data |>
      dplyr::filter(path == input$seq)

    if (nrow(check_temp) > 0 && !is.null(check_temp$timestamp)) {
      check_temp2 <- check_temp |>
        dplyr::distinct(score, timestamp) |>
        dplyr::slice(1)

      if (is.na(check_temp2$score)) {
        return(paste("Du hast diese Sequenz bereits bearbeitet am", check_temp2$timestamp))
      }
    }

    return("")
  })


  # ---- Update species selection ----
  observeEvent(input$seq, {
    x <- df_filter()

    if (!is.null(x) && nrow(x) > 0) {
      selected_species <- if (is.na(x$species_to_check[1])) {
        x$prediction[1]
      } else {
        x$species_to_check[1]
      }

      shinyWidgets::updateMultiInput(
        session,
        "inSelect",
        label = "Vogelarten auswählen:",
        selected = selected_species
      )
    }
  })


  # ---- Save verification ----
  observeEvent(input$add_btn, {
    req(input$seq)

    old_df <- this_table$data |>
      dplyr::filter(path != input$seq)

    add_df <- this_table$data |>
      dplyr::filter(path == input$seq) |>
      dplyr::distinct(path, .keep_all = TRUE) |>
      dplyr::mutate(species_to_check = NA)

    # Set prediction(s)
    if (is.null(input$inSelect) || length(input$inSelect) == 0) {
      add_df$prediction <- "falsch"
    } else {
      add_df$prediction <- list(input$inSelect)
      add_df <- add_df |>
        tidyr::unnest(cols = c(prediction))
    }

    # Update reactive table
    this_table$data <- rbind(add_df, old_df)

    # Prepare export row
    export_df <- add_df |>
      dplyr::mutate(test_file = NA) |>
      dplyr::select(
        verification = prediction,
        id_seq,
        start,
        end,
        test_file,
        path,
        plot_info
      ) |>
      dplyr::mutate(
        timestamp = lubridate::now(),
        verification_by = res_auth$user
      )

    # Append to EXPORT_FILE
    readr::write_csv(
      export_df,
      EXPORT_FILE,
      append = TRUE
    )


    # ---- Move to next audio sequence ----
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

      video::changeVideo("video", next_path)

      updateSelectizeInput(
        session,
        "seq",
        choices = mylist,
        selected = next_path
      )

      video::seekVideo("video", seek = 5)
      video::playVideo("video")
    }
  })


  # ---- Output: Username ----
  output$user_info <- renderUI({
    tags$h3(paste0("Hallo ", res_auth$user, "!"))
  })

  # ---- Output: Warning ----
  output$text1 <- renderText({
    warning_text()
  })


  # ---- Output: Table ----
  output$table_bnet <-DT::renderDataTable({
    req(df_filter())

    df <- df_filter()

    if (nrow(df) > 0 && !is.na(df$species_to_check[1])) {
      highlight_row <- which(df$species_to_check == df$prediction)[1]

      DT::datatable(
        df |> dplyr::select(species_to_check, prediction, score),
        options = list(
          dom = "t",
          columnDefs = list(list(visible = FALSE, targets = "species_to_check"))
        )
      ) |>
        DT::formatRound("score", digits = 3) |>
        DT::formatStyle(
          0,
          target = "row",
          backgroundColor = DT::styleEqual(highlight_row, "lightgrey")
        )
    }
  })


  # ---- Output: Plot info ----
  output$plot_info <- renderUI({
    req(df_filter())

    tags$h5(
      df_filter() |> dplyr::distinct(plot_info) |> dplyr::pull(plot_info)
    )
  })


  # ---- Output: Map ----
  output$map <- renderUI({
    req(df_filter())

    url_temp <- df_filter() |>
      dplyr::distinct(plot_info) |>
      tidyr::separate(plot_info, into = c("id", "id_2", "id_3"), sep = " ") |>
      dplyr::left_join(getOption("evalpam.data")$coords, by = "id") |>
      dplyr::pull(url)

    tagList(
      "Aufnahmeort: ",
      a("Karte öffnen", href = url_temp, target = "_blank")
    )
  })

}
