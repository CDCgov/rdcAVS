ui_monitoring <- function() {
  tabPanel(
    "Surveillance",
    fluidRow(column(
      8,
      h4("S\u00e9lection de Campagne"),
      uiOutput("campaign_surveillance"),
      selectInput("data_quality_sheet_selection",
                  "Sections",
                  list(
                    "Donnees de base" = 1,
                    "J(-3)" = 2,
                    "J(-2)" = 3,
                    "J(-1)" = 4,
                    "Jour1" = 5,
                    "Jour2" = 6,
                    "Jour3" = 7,
                    "Jour4" = 8),
                  multiple = TRUE),
      actionButton("monitor_campaign_btn", "Sélectionner une campagne", class = "btn-primary"),
      br(),
      br()
    )),
    fluidRow(
      h5("Completeness information"),
      verbatimTextOutput("refresh_date"),
      DT::DTOutput("campaign_info_table"),
      column(width = 4,
             downloadButton("download_data_quality_monitoring",
                            "T\u00e9l\u00e9charger",
                            style = "display: none;"))
    ),
    fluidRow(
      h5("Campaign progress report"),
      DT::DTOutput("campaign_progress_table"),
      column(width = 4,
             downloadButton("download_campaign_quality_monitoring",
                            "T\u00e9l\u00e9charger",
                            style = "display: none;")),
      selectInput("prov_selector_campaign_completeness",
                  "Select province below:",
                  choices = NULL),
      selectInput("zs_selector_campaign_completeness",
                  "Select zone de sante below:",
                  choices = NULL),

      plotOutput("campaign_completeness_plot")
    )
  )
}
