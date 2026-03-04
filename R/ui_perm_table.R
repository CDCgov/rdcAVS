
ui_perm_table <- function() {
  nav_panel(
    "G\u00e9rer les Autorisations",br(),

    Stack(
      tokens = list(childrenGap = 14),

      Text(variant = "xLarge", "G\u00e9rer les Niveaux d'Autorisation"),

      fluent_card(
        Stack(
          tokens = list(childrenGap = 10),

          Text(variant = "large", "Importer un fichier d'autorisations (CSV)"),

          fileInput(
            "upload_permissions",
            label = NULL,
            accept = ".csv",
            buttonLabel = "Parcourir...",
            placeholder = "Aucun fichier s\u00e9lectionn\u00e9"
          ),

          MessageBar(
            messageBarType = 5,   # tip
            isMultiline = TRUE,
            styles = list(text = list(fontSize = 14, lineHeight = "1.55")),
            Stack(tokens = list(childrenGap = 6),
              Text('Si le niveau est "national", les zones g\u00e9ographiques sont facultatives.'),
              Text("Une zone g\u00e9ographique valide doit \u00eatre incluse dans les autres niveaux."),
              Text('Exemple : si le niveau est "zone de sant\u00e9", incluez une valeur pour "province", "antenne" et "zone de sant\u00e9".')
            ),br(),
            Stack(
            horizontal = TRUE,
            verticalAlign = "center",
            tokens = list(childrenGap = 6),
            Text("Vous pouvez t\u00e9l\u00e9charger au besoin un template"),
            actionLink(
              "download_template_link_permissions",
              "ici",
              style = "font-weight:600; color:#0F6CBD; text-decoration:underline;"
            )
          )
          )
        )
      ),
      div(id = "show_permissions",
          style = "display:none;",
      fluent_card(
        Stack(
          tokens = list(childrenGap = 12),

          Text(variant = "large", "Table des autorisations"),
          Stack(
            horizontal = TRUE,
            wrap = TRUE,
            verticalAlign = "center",
            tokens = list(childrenGap = 8),

            icon_btn("delete_permission", "Delete", "Supprimer la s\u00e9lection", type = "danger"),
            icon_btn("undo_perm", "Undo", "Annuler", type = "primary"),
            icon_btn("redo_perm", "Redo", "R\u00e9tablir", type = "primary"),
            icon_btn("clear_perm", "DeleteRows", "Tout effacer", type = "danger"),
            icon_btn("press_download_permissions", "Download", "T\u00e9l\u00e9charger", type = "success"),
            icon_btn("add_row_question_perm", "Add", "Ajouter une entr\u00e9e", type = "success")
          ),
          br(),
          div(
      id = "add_permissions",
      style = "display:none;",
      div(
        style = "border:1px dashed #c8c6c4;
                border-radius:12px;
                padding:12px;
                background:#faf9f8;",
        Stack(
          tokens = list(childrenGap = 12),

          Text(variant = "large", "Ajouter une autorisation"),

          # Row 1
          Stack(
            horizontal = TRUE,
            wrap = TRUE,
            tokens = list(childrenGap = 12),

            div(
              style = "flex:1; min-width:240px;",
              TextField.shinyInput("perm_name", label = "Name", placeholder = "Nom")
            ),
            div(
              style = "flex:1; min-width:240px;",
              TextField.shinyInput("perm_phone", label = "Phone", placeholder = "+243...")
            ),
            div(
              style = "flex:2; min-width:280px;",
              TextField.shinyInput("perm_notes", label = "Notes", placeholder = "Affiliation, Poste, etc...")
            )
          ),

          # Row 2
          Stack(
            horizontal = TRUE,
            wrap = TRUE,
            tokens = list(childrenGap = 12),

            div(
              style = "flex:1; min-width:260px;",
              TextField.shinyInput("perm_email", label = "Email", placeholder = "johndoe@gmail.com", iconProps = list(iconName = "Mail"),required  = TRUE)
            ),

            div(
              style = "flex:1; min-width:240px;",
              Dropdown.shinyInput(
                "perm_level",
                label = "Level",
                options = lapply(c("national", "province", "antenne", "zone de sante"), function(x) list(key = x, text = x)),
                value =  "national"
              )
            ),

            div(
              style = "flex:1; min-width:240px;",
              Dropdown.shinyInput(
                "perm_role",
                label = "Role",
                options = lapply(c("writer", "reader", "commenter"), function(x) list(key = x, text = x)),
                value = "reader"
              )
            )
          ),

          # Geographic selectors 
          Stack(
            horizontal = TRUE,
            wrap = TRUE,
            tokens = list(childrenGap = 12),

            conditionalPanel(
              condition = "input.perm_level == 'province' || input.perm_level == 'antenne' || input.perm_level == 'zone de sante'",
              div(
                style = "flex:1; min-width:240px;",
                Dropdown.shinyInput(
                  "perm_province",
                  label = "Province",
                  placeholder = "S\u00e9lectionnez une province",
                  options = list(),
                  required = TRUE
                )
              )
            ),
            conditionalPanel(
              condition = "input.perm_level == 'antenne' || input.perm_level == 'zone de sante'",
              div(
                style = "flex:1; min-width:240px;",
                Dropdown.shinyInput(
                  "perm_antenne",
                  label = "Antenne",
                  placeholder = "S\u00e9lectionnez une antenne",
                  required = TRUE,
                  options = list()
                )
              )
            ),

            conditionalPanel(
              condition = "input.perm_level == 'zone de sante'",
              div(
                style = "flex:1; min-width:240px;",
                Dropdown.shinyInput(
                  "perm_zs",
                  label = "Zones de Sant\u00e9",
                  placeholder = "S\u00e9lectionnez une zone de sant\u00e9",
                  options = list(),
                  required = TRUE
                )
              )
            )
          ),

          Stack(
            horizontal = TRUE,
            tokens = list(childrenGap = 10),

            PrimaryButton.shinyInput(
              "add_permission",
              text = "Ajouter une entr\u00e9e",
              iconProps = list(iconName = "Add")
            )
          )
        )      
    )
    ),
          br(),
          div(
            style = "border:1px solid #edebe9; border-radius:12px; padding:10px; background:#fff;",
            DTOutput("permissions_table", width = "100%")
          )         
        )
      ),br(),
      fluent_card(
        Stack(
          tokens = list(childrenGap = 12),

          Text(variant = "xLarge", "S\u00e9lection de Campagne"),
          uiOutput("campaign_drive_picker"),
          Stack(
            horizontal = TRUE,
            wrap = TRUE,
            tokens = list(childrenGap = 10),
            taskButton(
              "set_permissions_btn",
              label = "D\u00e9finir l'autorisation",
              iconProps = list(iconName = "Permissions")
            ),
            DefaultButton.shinyInput(
              "refresh_drive",
              text = "Actualiser",
              iconProps = list(iconName = "Refresh"),
              styles = list(root = list(display = "none"))
            )
          )
        )
      ),br(),br(),br()
       )
    )
  )
}