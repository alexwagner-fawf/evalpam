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
          uiOutput("target_species_ui"),
          uiOutput("mode_info_ui"),
          numericInput("score_start", "Scores anzeigen ab / Show scores from:",
                       value = 1.0, min = 0, max = 1, step = 0.05),
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
          hr(),
          actionButton(
            "model_check_btn", "Modell-Check",
            icon = icon("chart-line"),
            style = "width: 100%; background-color: #5cb85c; color: white; border-color: #4cae4c;"
          ),

          br(),
          uiOutput("plot_info")
        ),

        # ---- Main Panel ----
        mainPanel(
          width = 8,
          h2("Spektrogramm"),

          # Controls (Play, Zeit)
          div(style = "display: flex; align-items: center; gap: 12px; margin-bottom: 8px;
                        background: #2c3e50; padding: 8px 14px; border-radius: 4px;",
              tags$button(id = "ws-play-btn",
                          style = "font-size: 22px; background: none; border: none; color: white;
                                   cursor: pointer; padding: 2px 8px;",
                          "\u25B6"),
              tags$span(id = "ws-current-time", style = "color: #eee; font-family: monospace; font-size: 14px;", "0:00.0"),
              tags$span(style = "color: #888;", "/"),
              tags$span(id = "ws-total-time", style = "color: #aaa; font-family: monospace; font-size: 14px;", "0:00.0")
          ),

          # Waveform
          div(id = "waveform",
              style = "background: #1a1a2e; border-radius: 4px 4px 0 0; padding: 4px;"),

          # Timeline
          div(id = "timeline",
              style = "background: #1a1a2e; padding: 0 4px;"),

          # Spektrogramm
          div(id = "spectrogram",
              style = "background: #1a1a2e; border-radius: 0 0 4px 4px;"),

          # Legende
          div(style = "font-size: 12px; color: #888; margin-top: 6px;",
              tags$span(style = "display:inline-block; width:12px; height:12px;
                                  background:rgba(46,204,113,0.3); border:1px solid #2ecc71;
                                  vertical-align:middle; margin-right:4px;"),
              "Detektionsfenster",
              tags$span(style = "margin-left: 15px;"),
              tags$span(style = "display:inline-block; width:12px; height:12px;
                                  background:rgba(0,0,0,0.4); border:1px solid #555;
                                  vertical-align:middle; margin-right:4px;"),
              "Kontext (Padding)",
              tags$span(style = "margin-left: 15px; color: #aaa;",
                        "Klick auf Waveform = Play/Pause")
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

  # --- FIX: Pfad sicherstellen ---
  # 1. Versuche Umgebungsvariable
  spec_path <- Sys.getenv("spectrogram_folder")

  # 2. Wenn leer, nimm den lokalen Ordner im Projekt
  if (spec_path == "") {
    spec_path <- "spectrograms" # Oder der absolute Pfad: "C:/MeinProjekt/spectrograms"
  }

  # 3. Prüfen und freigeben
  if (dir.exists(spec_path)) {
    add_resource_path("spectrograms", spec_path)
  } else {
    warning(paste("ACHTUNG: Spektrogramm-Ordner nicht gefunden unter:", spec_path))
  }
  # =====================================================================
  # ALTERNATIVE: Interactive JS Spectrogram (Wavesurfer)
  # Currently deactivated in favor of the more stable MP4 video version.
  # Uncomment if needed and ensure the paths are updated to .mp3 files.
  # =====================================================================
  # tagList(
  #   tags$head(
  #     favicon(),
  #     bundle_resources(path = app_sys("app/www"), app_title = "evalpam"),
  #     tags$script(type = "module", src = "www/wavesurfer_init.js")
  #   )
  # )

  tagList(
    tags$head(
      favicon(),
      bundle_resources(path = app_sys("app/www"), app_title = "evalpam"),
      tags$script(type = "module", src = "www/wavesurfer_init.js")
    )
  )
}
