#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @import shiny
#' @noRd
app_ui <- function(request) {

  # HINWEIS: Wir laden hier KEINE Daten mehr über getOption!
  # Die Dropdowns werden leer initialisiert und vom Server befüllt.

  ui <- tagList(
    golem_add_external_resources(),

    fluidPage(
      titlePanel("Vogelstimmen-Verifikation"),

      sidebarLayout(

        # Sidebar ----
        sidebarPanel(
          width = 4,

          # 1. User Info & Projekt Auswahl
          uiOutput("user_info"),
          uiOutput("project_selector_ui"),
          hr(),

          # 2. Sequence Selection
          selectizeInput(
            "seq",
            "Sequenz:",
            choices = character(0), # WICHTIG: Leerer Vektor statt NULL
            options = list(maxOptions = 10000)
          ),

          textOutput('text1'),
          tags$head(tags$style("#text1{ color: red; font-size: 16px; font-style: italic; }")),

          br(),

          # 3. Species Selection
          shinyWidgets::multiInput(
            inputId = "inSelect",
            label = "Vogelarten:",
            choices = character(0), # WICHTIG: Leerer Vektor statt NULL
            width = "100%",
            options = list(
              enable_search = TRUE,
              non_selected_header = "Verfügbare Arten:",
              selected_header = "Ausgewählt:"
            )
          ),

          h4("BirdNET Vorhersagen:"),
          DT::DTOutput('table_bnet'),

          br(),

          fluidRow(
            column(6, uiOutput("plot_info")),
            column(6, uiOutput("map"))
          ),

          br(),

          actionButton(
            "add_btn",
            "Speichern & Weiter",
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
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {

  add_resource_path("www", app_sys("app/www"))

  # Audio-Ordner einbinden
  spec_path <- Sys.getenv("spectogram_folder")
  if(spec_path != "" && dir.exists(spec_path)) {
    add_resource_path("spectograms", spec_path)
  }

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "evalpam"
    )
  )
}
