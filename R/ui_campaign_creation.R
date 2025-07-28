ui_campaign_creation <- function() {

  checkbox_title_style <- "border: 1px solid #ccc; background-color: white; padding: 1px; margin-bottom: 10px;"
  checkbox_style <- "height: 100px; overflow-y: scroll; background-color: white; padding-top: 3px;"

  tabPanel(
    "Cr\u00e9er Une Campagne",
    fluidRow(column(
      8,
      h4("Authentifiez-vous avec Google Drive"),
      actionButton("auth_drive", "Authentifier", class = "btn-success"),
      verbatimTextOutput("auth_status"),
      br()
    )),
    sidebarPanel(
      textInput("campaign_name", "Nom de la Campagne"),
      dateInput("start_date", "D\u00e9but", language = "fr", format = "dd/mm/yyyy"),
      dateInput("end_date", "Fin", language = "fr", format = "dd/mm/yyyy"),
      br(),
      fluidRow(
        column(
          8,
          actionLink("select_all_prov", "S\u00e9lectionner Tout / D\u00e9s\u00e9lectionner Tout"),
          h4("Provinces"),
          tags$div(style = checkbox_title_style, tags$div(
            style = checkbox_style,
            checkboxGroupInput("selected_provinces", NULL, choices = NULL)
          ))
        ),
        column(
          8,
          actionLink("select_all_ant", "S\u00e9lectionner Tout / D\u00e9s\u00e9lectionner Tout"),
          h4("Antennes"),
          tags$div(style = checkbox_title_style, tags$div(
            style = checkbox_style,
            checkboxGroupInput("selected_ant", NULL, choices = NULL)
          ))
        ),
        column(
          8,
          actionLink("select_all_zs", "S\u00e9lectionner Tout / D\u00e9s\u00e9lectionner Tout"),
          h4("Zones de Sante"),
          tags$div(style = checkbox_title_style, tags$div(
            style = checkbox_style, checkboxGroupInput("selected_zs", NULL, choices = NULL)
          ))
        ),
        column(8, textInput("zs_template_url", "Adresse URL de mod\u00e8le de masque"))),
      br(),
      actionButton("create_campaign", "Cr\u00e9er une Campagne", class = "btn-primary"),
      width = 20
    ),
  )
}
