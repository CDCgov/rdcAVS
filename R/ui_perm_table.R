ui_perm_table <- function() {
  nav_panel(
    "G\u00e9rer les Autorisations",
    fluidRow(
      h4("G\u00e9rer les Niveaux d'Autorisation"),
      fluidRow(column(
        8,
        fileInput(
          "upload_permissions",
          "Autorisations de T\u00e9l\u00e9chargement CSV",
          accept = ".csv"
        )
      ),
      helpText(paste0(
        'Si le niveau est "global", les zones g\u00e9ographiques sont facultatives.',
        " Une zone g\u00e9ographique valide doit \u00eatre incluse dans les autres niveaux.\n\n",
        'Par exemple, si le niveau est "zone de sante",',
        ' incluez une valeur pour les colonnes "province", "antenne" et "zone de sante".'
      )),
      DT::DTOutput("permissions_table"),
      br(),
      fluidRow(
        column(
          8, actionButton("delete_permission", "Supprimer la S\u00e9lection", class = "btn-danger"),
          actionButton("undo_perm", "Annuler", icon = icon("undo")),
          actionButton("redo_perm", "R\u00e9tablir", icon = icon("redo"))
        )),
      br(), br()
      ), column(
        8,
        actionButton(
          "clear_perm",
          "Tout Effacer",
          icon = icon("trash"),
          class = "btn-danger"
        ),
        downloadButton(
          "download_permissions",
          "T\u00e9l\u00e9charger les Autorisations Actuelles"
        )
      ),
      br(),
      br(),
      fluidRow(
        column(3, textInput("perm_name", "Name")),
        column(3, textInput("perm_phone", "Phone")),
        column(3, textInput("perm_notes", "Notes", placeholder = "affiliation, job title, etc...")),
        column(3, textInput("perm_email", "Email")),
        column(3, selectInput(
          "perm_level",
          "Level",
          choices = c("global", "province", "antenne", "zone de sante")
        )),
        column(3, selectInput(
          "perm_role", "Role",
          choices = c("writer", "reader", "commenter")
        )),
      ),
      fluidRow(
        # Province options
        conditionalPanel(condition = "input.perm_level == 'province' || input.perm_level == 'antenne' || input.perm_level == 'zone de sante'", column(
          4,
          selectizeInput(
            "perm_province",
            "Province",
            choices = NULL,
            options = list(placeholder = "S\u00e9lectionnez une province")
          )
        )),

        # Antenne options
        conditionalPanel(condition = "input.perm_level == 'antenne' || input.perm_level == 'zone de sante'", column(
          4,
          selectizeInput(
            "perm_antenne",
            "Antenne",
            choices = NULL,
            options = list(placeholder = "S\u00e9lectionnez une antenne")
          )
        )),

        # Zone de sante options
        conditionalPanel(condition = "input.perm_level == 'zone de sante'", column(
          4,
          selectizeInput(
            "perm_zs",
            "Zones de Sante",
            choices = NULL,
            options = list(placeholder = "S\u00e9lectionnez une zone de sant\u00e9")
          )
        ))
      ),
      fluidRow(column(
        3,
        actionButton("add_permission", "Add Entry", class = "btn-success")
      )),
      br(),
      br()
    ),
    fluidRow(column(
      8,
      h4("S\u00e9lection de Campagne"),
      uiOutput("campaign_drive_picker"),
      actionButton("set_permissions_btn", "Set Permissions", class = "btn-primary"),
      actionButton(
        "refresh_drive",
        "Actualiser",
        class = "btn-secondary",
        style = "display: none;"
      ),
      br(),
      br()
    ))
  )
}
