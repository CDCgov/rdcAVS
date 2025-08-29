ui_perm_table <- function() {
  nav_panel(
    "G\u00e9rer les Autorisations",
      h4("G\u00e9rer les Niveaux d'Autorisation"),
      fileInput(
        "upload_permissions",
        "Autorisations de T\u00e9l\u00e9chargement CSV",
        accept = ".csv"
      ),
    helpText(paste0(
        'Si le niveau est "national", les zones g\u00e9ographiques sont facultatives.',
        " Une zone g\u00e9ographique valide doit \u00eatre incluse dans les autres niveaux.\n\n",
        'Par exemple, si le niveau est "zone de sante",',
        ' incluez une valeur pour les colonnes "province", "antenne" et "zone de sante".'
      )),
      DT::DTOutput("permissions_table"),
      layout_columns(
        actionButton("delete_permission", "Supprimer la S\u00e9lection", class = "btn-danger"),
        actionButton("undo_perm", "Annuler", icon = icon("undo")),
        actionButton("redo_perm", "R\u00e9tablir", icon = icon("redo")),
        actionButton("clear_perm", "Tout Effacer", icon = icon("trash"), class = "btn-danger"),
        downloadButton("download_permissions", "T\u00e9l\u00e9charger")
      ),
      layout_columns(
        textInput("perm_name", h6("Name")),
        textInput("perm_phone", h6("Phone")),
        textInput("perm_notes", h6("Notes"), placeholder = "affiliation, job title, etc..."),
        textInput("perm_email", h6("Email")),
        selectInput(
          "perm_level",
          h6("Level"),
          choices = c("national", "province", "antenne", "zone de sante")
        ),
        selectInput(
          "perm_role", h6("Role"),
          choices = c("writer", "reader", "commenter")
        )),
      layout_columns(
        # Province options
        conditionalPanel(condition = "input.perm_level == 'province' || input.perm_level == 'antenne' || input.perm_level == 'zone de sante'",
          selectizeInput(
            "perm_province",
            h6("Province"),
            choices = NULL,
            options = list(placeholder = "S\u00e9lectionnez une province")
          )
        ),

        # Antenne options
        conditionalPanel(condition = "input.perm_level == 'antenne' || input.perm_level == 'zone de sante'",
        selectizeInput(
            "perm_antenne",
            h6("Antenne"),
            choices = NULL,
            options = list(placeholder = "S\u00e9lectionnez une antenne")
          )
        ),

        # Zone de sante options
        conditionalPanel(condition = "input.perm_level == 'zone de sante'",
          selectizeInput(
            "perm_zs",
            h6("Zones de Sante"),
            choices = NULL,
            options = list(placeholder = "S\u00e9lectionnez une zone de sant\u00e9")
          )
        ),
        br(), br(), br()),
    layout_columns(actionButton("add_permission", "Ajouter Une Entr\u00e9e", class = "btn-success"), col_widths = 2),
    layout_columns(h4("S\u00e9lection de Campagne")),
    layout_columns(
      uiOutput("campaign_drive_picker"),
      input_task_button("set_permissions_btn", "Définir l'Autorisation", class = "btn-primary"),
      input_task_button(
        "refresh_drive",
        "Actualiser",
        class = "btn-secondary",
        style = "display: none;",
        label_busy = "Traitement..."
      ),
      br(), br(), br(), br()
    )
  )
}
