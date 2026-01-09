#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @import shiny
#' @import shinyjs
#' @noRd
app_ui <- function(request) {
  ui <- tagList(
    golem_add_external_resources(),
    fluidPage(
      shinyjs::useShinyjs(),
      titlePanel("Vogelstimmen-Verifikation"),

      sidebarLayout(
        # ---- Sidebar ----
        sidebarPanel(
          width = 4,

          # --- OBERE LEISTE: User Info & Einstellungen ---
          fluidRow(
            column(8, uiOutput("user_info")), # Name des Users
            column(4,
                   # Das Zahnrad für "globale" Einstellungen (versteckt die Sprache)
                   div(style = "float: right;",
                       shinyWidgets::dropdownButton(
                         tags$h4("Einstellungen"),
                         selectInput(
                           "species_lang",
                           "Sprache / Language:",
                           choices = c("Deutsch" = "de", "English" = "en", "Scientific" = "sci"),
                           selected = "de"
                         ),
                         circle = TRUE,
                         status = "default",
                         icon = icon("gear"),
                         width = "300px",
                         tooltip = shinyWidgets::tooltipOptions(title = "Sprache ändern")
                       )
                   )
            )
          ),
          hr(),

          # --- NAVIGATION ---
          uiOutput("project_selector_ui"),
          selectizeInput("seq", "Sequenz / File:", choices = character(0), options = list(maxOptions = 10000)),

          # Info-Boxen (Task & Warnungen)
          div(style = "margin-top: -10px; margin-bottom: 10px;",
              uiOutput("task_info"),
              textOutput('text1'),
              tags$head(tags$style("#text1{ color: red; font-size: 14px; font-style: italic; }"))
          ),

          hr(),

          # --- ANNOTATION INPUTS ---

          # 1. Arten-Auswahl (Bleibt wie vorher)
          shinyWidgets::multiInput(
            inputId = "inSelect",
            label = "Vogelarten / Species:",
            choices = character(0),
            width = "100%",
            options = list(
              enable_search = TRUE,
              non_selected_header = "Verfügbar:",
              selected_header = "Ausgewählt:"
            )
          ),

          hr(),

          # 2. Dynamische Verhaltens-Liste (NEU)
          # Hier erscheinen automatisch die Eingabefelder für jede gewählte Art
          tags$div(
            id = "behavior_container",
            style = "max-height: 400px; overflow-y: auto; padding-right: 5px;", # Scrollbar falls viele Arten
            uiOutput("dynamic_behavior_ui")
          ),

          br(),

          # --- BUTTON ---
          actionButton(
            "add_btn", "Speichern & Weiter",
            icon = icon("paper-plane"),
            style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 100%; font-weight: bold;"
          ),

          br(), br(),

          # --- INFO TABELLE ---
          h5("BirdNET Detektionen:"),
          DT::DTOutput('table_bnet'),

          br(),
          uiOutput("plot_info")
        ),

        # ---- Main Panel ----
        mainPanel(
          width = 8,
          h2("Spektrogramm"),
          video::video(elementId = "video", files = NULL)
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
