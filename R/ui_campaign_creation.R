ui_campaign_creation <- function() {

  checkbox_title_style <- "border: 1px solid #ccc; background-color: white; padding: 1px; margin-bottom: 10px;"
  checkbox_style <- "height: 100px; overflow-y: scroll; background-color: white; padding-top: 3px;"

  nav_panel(
    "Cr\u00e9er Une Campagne",
      h4("Authentifiez-vous avec Google Drive"),
      actionButton("auth_drive", "Authentifier", class = "btn-success"),
      layout_columns(verbatimTextOutput("auth_status"), br(), br()),
      h4("Informations sur la Campagne"),
      layout_columns(card(textInput("campaign_name", h6("Nom de la Campagne"))),
                      card(dateInput("start_date", h6("D\u00e9but"),
                                language = "fr", format = "dd/mm/yyyy")),
                      card(dateInput("end_date", h6("Fin"),
                                language = "fr", format = "dd/mm/yyyy")),
                      br()),
      layout_columns(
        card(
          h6("Provinces"),
          actionLink("select_all_prov", "S\u00e9lectionner Tout / D\u00e9s\u00e9lectionner Tout"),
          tags$div(style = checkbox_title_style, tags$div(
            style = checkbox_style,
            checkboxGroupInput("selected_provinces", NULL, choices = NULL)
          ))
        ),
        card(
          h6("Antennes"),
          actionLink("select_all_ant", "S\u00e9lectionner Tout / D\u00e9s\u00e9lectionner Tout"),
          tags$div(style = checkbox_title_style, tags$div(
            style = checkbox_style,
            checkboxGroupInput("selected_ant", NULL, choices = NULL)
          ))
        ),
        card(
          h6("Zones de Sante"),
          actionLink("select_all_zs", "S\u00e9lectionner Tout / D\u00e9s\u00e9lectionner Tout"),
          tags$div(style = checkbox_title_style, tags$div(
            style = checkbox_style, checkboxGroupInput("selected_zs", NULL, choices = NULL)
          ))
        ),
        br()),
      textInput("zs_template_url", h6("Adresse URL de mod\u00e8le de masque")),
      actionButton("create_campaign", "Cr\u00e9er une Campagne", class = "btn-primary"),
    )
}
