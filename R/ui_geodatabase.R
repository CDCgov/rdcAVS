
ui_geodatabase <- function() {
  nav_panel(
    "Informations Géographiques",br(),
    Stack(
      tokens = list(childrenGap = 14),
      MessageBar(
        messageBarType = 5,          
        isMultiline = TRUE,
        styles = list(
          root = list(marginBottom = 6),
          text = list(fontSize = 14, lineHeight = "1.55")
        ),
        Stack(
          tokens = list(childrenGap = 6),
          Text(
            "Veuillez inclure les colonnes suivantes dans votre fichier CSV lors de l'importation :",
            variant = "mediumPlus"
          ),
          Text("provinces, antennes, zone_de_sante, aires_de_sante, population_totale."),
          Stack(
            horizontal = TRUE,
            verticalAlign = "center",
            tokens = list(childrenGap = 6),
            Text("Vous pouvez télécharger au besoin un template"),
            actionLink(
              "download_template_link",
              "ici",
              style = "font-weight:600; color:#0F6CBD; text-decoration:underline;"
            )
          )
        )
      ),
      fluent_card(
        Stack(
          tokens = list(childrenGap = 10),

          Text(variant = "large", "Charger un fichier CSV"),
          Text("Veuillez charger un fichier CSV avec les informations géographiques."),
          fileInput(
            "upload_geo",
            label = NULL,
            accept = ".csv",
            buttonLabel = "Parcourir…",
            placeholder = "Aucun fichier choisi"
          )
        )
      ),
      fluent_card(
        Stack(
          tokens = list(childrenGap = 12),
          Stack(
            horizontal = TRUE,
            wrap = TRUE,
            verticalAlign = "center",
            tokens = list(childrenGap = 8),

            icon_btn("delete_row", "Delete", "Supprimer la sélection", type = "danger"),
            icon_btn("undo_geo", "Undo", "Annuler", type = "primary"),
            icon_btn("redo_geo", "Redo", "Rétablir", type = "primary"),
            icon_btn("clear_geo", "DeleteRows", "Tout effacer", type = "danger"),
            icon_btn("download_geo", "Download", "Télécharger", type = "success"),
            icon_btn("add_row_question", "Add", "Ajouter une entrée", type = "success")
          ),
          div(
            id = "add_new_row",
            style = "display:none;",
            div(
              style = "
                border:1px dashed #c8c6c4;
                border-radius:12px;
                padding:12px;
                background:#faf9f8;
              ",

              Stack(
                tokens = list(childrenGap = 12),

                Text(variant = "mediumPlus", "Nouvelle entrée"),

                Stack(
                  horizontal = TRUE,
                  wrap = TRUE,
                  tokens = list(childrenGap = 12),

                  div(
                    style = "flex:1; min-width:240px;",
                    TextField.shinyInput(
                      "new_province",
                      label = "Provinces",
                      placeholder = "Veuillez entrer le nom de la province ici"
                    )
                  ),
                  div(
                    style = "flex:1; min-width:240px;",
                    TextField.shinyInput(
                      "new_antenne",
                      label = "Antennes",
                      placeholder = "Veuillez entrer le nom de l'antenne ici"
                    )
                  ),
                  div(
                    style = "flex:1; min-width:240px;",
                    TextField.shinyInput(
                      "new_zs",
                      label = "Zones de santé",
                      placeholder = "Veuillez entrer le nom de la Zones de santé ici"
                    )
                  ),
                  div(
                    style = "flex:1; min-width:240px;",
                    TextField.shinyInput(
                      "new_as",
                      label = "Aires de santé",
                      placeholder = "Veuillez entrer le nom de l'aire de santé ici"
                    )
                  ),
                  div(
                    style = "flex:1; min-width:240px;",
                    TextField.shinyInput(
                      "new_pop",
                      label = "Population Totale",
                      type = "number",
                      value = 0,
                      min = 0
                    )
                  )
                ),

                Stack(
                  horizontal = TRUE,
                  tokens = list(childrenGap = 10),

                  PrimaryButton.shinyInput(
                    "add_row",
                    text = "Ajouter",
                    iconProps = list(iconName = "Add")
                  ),
                )
              )
            )
          ),

          # Table output
          div(
            style = "border:1px solid #edebe9; border-radius:12px; padding:10px; background:#fff;",
            DTOutput("geo_table", width = "100%")
          )
        )
      )
    )
  )
}