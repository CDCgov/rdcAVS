ui_monitoring <- function() {
  nav_panel(
    "Surveillance",
    h4("Surveillance de Campagne"),
    layout_columns(uiOutput("campaign_surveillance"),
                   selectInput("data_quality_sheet_selection",
                               h6("Sections"),
                               list(
                                 "Donnees de base" = 1,
                                 "J(-3)" = 2,
                                 "J(-2)" = 3,
                                 "J(-1)" = 4,
                                 "Jour1" = 5,
                                 "Jour2" = 6,
                                 "Jour3" = 7,
                                 "Jour4" = 8),
                               multiple = TRUE), br(), br(), br(), br()),

    layout_columns(actionButton("monitor_campaign_btn",
                                "Sélectionner une campagne",
                                class = "btn-primary"), col_widths = 2),
    layout_columns(verbatimTextOutput("refresh_date"), col_widths = 3),
    accordion(
      open = "Graphiques",
      accordion_panel(
        title = "Completeness information",
        icon = bsicons::bs_icon("card-checklist"),
        DT::DTOutput("campaign_info_table"),
        layout_columns(downloadButton("download_data_quality_monitoring",
                       "T\u00e9l\u00e9charger",
                       style = "display: none;"), col_widths = 2)
      ),
      accordion_panel(
        title = "Campaign progress report",
        icon = bsicons::bs_icon("activity"),
        DT::DTOutput("campaign_progress_table"),
        layout_columns(downloadButton("download_campaign_quality_monitoring",
                              "T\u00e9l\u00e9charger",
                              style = "display: none;"), col_widths = 2)
      ),
      accordion_panel(
        title = "Graphiques",
        icon = bsicons::bs_icon("bar-chart"),
        layout_columns(
          selectInput("prov_selector_campaign_completeness",
                      "Select province below:",
                      choices = NULL),
          selectInput("zs_selector_campaign_completeness",
                      "Select zone de sante below:",
                      choices = NULL),
          col_widths = c(3, 3)
        ),
        plotOutput("campaign_completeness_plot")
      )
    )
  )
}
