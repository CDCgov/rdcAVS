ui_campaign_creation <- function() {
  nav_panel(
    "Créer Une Campagne",
    Stack(
      tokens = list(childrenGap = 14),
      br(),      
      Text(variant = "xLarge", "Informations sur la Campagne"),
      
      fluent_card(
        Stack(
          horizontal = TRUE,
          wrap = TRUE,
          tokens = list(childrenGap = 12),          
          div(
            style = "flex:2; min-width:280px;",
            TextField.shinyInput(
              "campaign_name",
              label = "Nom de la campagne",
              placeholder = "Inserer le nom de la campagne ici",
              required = TRUE
            )
          ),          
          div(
            style = "flex:1; min-width:220px;",
            DatePicker.shinyInput(
              "start_date",
              label = "Début",
              isRequired = TRUE
            )
          ),          
          div(
            style = "flex:1; min-width:220px;",
            DatePicker.shinyInput(
              "end_date",
              label = "Fin",
              isRequired = TRUE
            )
          )
        ),
        
        div(style = "height:14px;"),
        
        Stack(
          horizontal = TRUE,
          wrap = TRUE,
          tokens = list(childrenGap = 12),
          
          # Provinces
          div(
            style = "flex:1; min-width:260px;",
            Stack(tokens = list(childrenGap = 6),
                  Text(variant = "mediumPlus", "Provinces"),
                  DefaultButton.shinyInput(
                    "select_all_prov",
                    text = "Tout sélectionner / désélectionner",
                    iconProps = list(iconName = "CheckList")
                  ),
                  div(
                    style = "
                  max-height:170px; overflow:auto; padding:8px;
                  border:1px solid #edebe9; border-radius:10px; background:#faf9f8;
                ",
                    checkboxGroupInput("selected_provinces", NULL, choices = NULL)
                  )
            )
          ),
          
          # Antennes
          div(
            style = "flex:1; min-width:260px;",
            Stack(tokens = list(childrenGap = 6),
                  Text(variant = "mediumPlus", "Antennes"),
                  DefaultButton.shinyInput(
                    "select_all_ant",
                    text = "Tout sélectionner / désélectionner",
                    iconProps = list(iconName = "CheckList")
                  ),
                  div(
                    style = "
                  max-height:170px; overflow:auto; padding:8px;
                  border:1px solid #edebe9; border-radius:10px; background:#faf9f8;
                ",
                    checkboxGroupInput("selected_ant", NULL, choices = NULL)
                  )
            )
          ),
          
          # Zones de sante
          div(
            style = "flex:1; min-width:260px;",
            Stack(tokens = list(childrenGap = 6),
                  Text(variant = "mediumPlus", "Zones de santé"),
                  DefaultButton.shinyInput(
                    "select_all_zs",
                    text = "Tout sélectionner / désélectionner",
                    iconProps = list(iconName = "CheckList")
                  ),
                  div(
                    style = "
                  max-height:170px; overflow:auto; padding:8px;
                  border:1px solid #edebe9; border-radius:10px; background:#faf9f8;
                ",
                    checkboxGroupInput("selected_zs", NULL, choices = NULL)
                  )
            )
          )
        ),
        
        div(style = "height:14px;"),        
        Stack(
          horizontal = TRUE,
          wrap = TRUE,
          tokens = list(childrenGap = 12),
          verticalAlign = "end",
          
          div(
            style = "flex:2; min-width:300px;",
            TextField.shinyInput(
              "zs_template_url",
              label = "Adresse URL du modèle de masque",
              placeholder = "https://…",
              iconProps = list(iconName = "Link")
            )
          ),
          
          div(
            style = "flex:1; min-width:240px;",
            PrimaryButton.shinyInput(
              "create_campaign",
              text = "Créer une campagne",
              iconProps = list(iconName = "Rocket")
            )
          )
        )
      ),
      uiOutput("campagne_folder_url")
    )
  )
}

