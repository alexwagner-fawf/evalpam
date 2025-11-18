# ============================================================================
# Bird Sound Verification App - User Interface
# ============================================================================

app_ui <- function(request) {

  # --- your UI goes here ---
  ui <- fluidPage(

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
        multiInput(
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
        dataTableOutput('table_bnet'),

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

        video(
          elementId = "video",
          files = NULL
        )
      )
    )
  )

  # --- wrap UI in shinymanager secure_app ---
  shinymanager::secure_app(
    ui = ui,
    # optional options:
    theme = bslib::bs_theme(),
    enable_admin = TRUE
  )
}
