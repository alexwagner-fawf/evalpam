#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {

  data <- getOption("evalpam.data")
  mylist <- data$mylist
  arten  <- data$arten

  ui <- tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(

      titlePanel("Vogelstimmen-Verifikation"),

      sidebarLayout(

        # Sidebar ----
        sidebarPanel(
          width = 4,
          # User Info
          uiOutput("user_info"),

          # Sequence Selection
          selectizeInput(
            "seq",

            "Sequenz:",
            choices = mylist,
            options = list(maxOptions = 10000)
          ),
          # Warning Text
          textOutput('text1'),
          tags$head(tags$style("#text1{
        color: red;
        font-size: 16px;
        font-style: italic;
      }")),

          br(),
          # Species Selection
          shinyWidgets::multiInput(
            inputId = "inSelect",
            label = "Vogelarten:",
            choices = arten,
            selected = NULL,
            width = "100%",
            options = list(
              enable_search = TRUE,
              non_selected_header = "Verfügbare Arten:",
              selected_header = "Ausgewählt:"
            )
          ),

          # BirdNET Predictions Table
          h4("BirdNET Vorhersagen:"),
          DT::DTOutput('table_bnet'),

          br(),

          # Plot Info and Map
          fluidRow(
            column(6, uiOutput("plot_info")),
            column(6, uiOutput("map"))
          ),

          br(),

          # Save Button
          actionButton(
            "add_btn",
            "Speichern",
            icon = icon("paper-plane"),
            style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 100%;"
          )
        ),

        # Main Panel ----
        mainPanel(
          width = 8,

          h2("Spektrogramm"),

          video::video(
            elementId = "video",
            files = NULL
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path( "www", app_sys("app/www") )
  add_resource_path("spectograms", Sys.getenv("spectogram_folder"))

    tags$head(
      favicon(),
      bundle_resources(
        path = app_sys("app/www"),
        app_title = "evalpam"
      )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
