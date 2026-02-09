
ui_perm_table <- function() {
  nav_panel(
    "Gérer les Autorisations",br(),

    Stack(
      tokens = list(childrenGap = 14),

      Text(variant = "xLarge", "Gérer les Niveaux d'Autorisation"),

      fluent_card(
        Stack(
          tokens = list(childrenGap = 10),

          Text(variant = "large", "Importer un fichier d'autorisations (CSV)"),

          fileInput(
            "upload_permissions",
            label = NULL,
            accept = ".csv",
            buttonLabel = "Parcourir…",
            placeholder = "Aucun fichier sélectionné"
          ),

          MessageBar(
            messageBarType = 5,   # tip
            isMultiline = TRUE,
            styles = list(text = list(fontSize = 14, lineHeight = "1.55")),
            Stack(tokens = list(childrenGap = 6),
              Text('Si le niveau est "national", les zones géographiques sont facultatives.'),
              Text("Une zone géographique valide doit être incluse dans les autres niveaux."),
              Text('Exemple : si le niveau est "zone de santé", incluez une valeur pour "province", "antenne" et "zone de santé".')
            ),br(),
            Stack(
            horizontal = TRUE,
            verticalAlign = "center",
            tokens = list(childrenGap = 6),
            Text("Vous pouvez télécharger au besoin un template"),
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

            icon_btn("delete_permission", "Delete", "Supprimer la sélection", type = "danger"),
            icon_btn("undo_perm", "Undo", "Annuler", type = "primary"),
            icon_btn("redo_perm", "Redo", "Rétablir", type = "primary"),
            icon_btn("clear_perm", "DeleteRows", "Tout effacer", type = "danger"),
            icon_btn("press_download_permissions", "Download", "Télécharger", type = "success"),
            icon_btn("add_row_question_perm", "Add", "Ajouter une entrée", type = "success")
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
              TextField.shinyInput("perm_phone", label = "Phone", placeholder = "+243…")
            ),
            div(
              style = "flex:2; min-width:280px;",
              TextField.shinyInput("perm_notes", label = "Notes", placeholder = "Affiliation, Poste, etc…")
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
                  placeholder = "Sélectionnez une province",
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
                  placeholder = "Sélectionnez une antenne",
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
                  label = "Zones de Santé",
                  placeholder = "Sélectionnez une zone de santé",
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
              text = "Ajouter une entrée",
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

          Text(variant = "xLarge", "Sélection de Campagne"),

          uiOutput("campaign_drive_picker"),

          Stack(
            horizontal = TRUE,
            wrap = TRUE,
            tokens = list(childrenGap = 10),

            PrimaryButton.shinyInput(
              "set_permissions_btn",
              text = "Définir l'autorisation",
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