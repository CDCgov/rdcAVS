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

    layout_columns(input_task_button("monitor_campaign_btn",
                                     "Sélectionner une campagne",
                                     label_busy = "Traitement..."), col_widths = 2),
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
                      "Sélectionner une province:",
                      choices = NULL),
          selectInput("zs_selector_campaign_completeness",
                      "Sélectionner une zone de sante:",
                      choices = NULL),
          col_widths = c(3, 3)
        ),
        card(navset_pill(
          nav_panel(
            "Couverture Campagne",
            plotOutput("campaign_completeness_plot", height = "700px", width = "40%")
            ),
          nav_panel("Nb moyen d'enfants vaccinés/équipe",
                    plotOutput("campaign_urban_rural_plot", height = "700px")
                    ),
          nav_panel("Récupérations",
                    plotOutput("campaign_recovery_plot", height = "700px"))
        ))
      )
    )
  )
}
