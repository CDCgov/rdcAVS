ui_geodatabase <- function() {
  tabPanel(
    "G\u00e9ographiques",
    fluidRow(
      h4("Donn\u00e9es G\u00e9ographiques"),
      fluidRow(
        column(8, fileInput("upload_geo", "T\u00e9l\u00e9charger Un Fichier CSV G\u00e9ographique",
                            accept = ".csv"
        )),
        helpText("Veuillez inclure les colonnes suivantes lors du t\u00e9l\u00e9chargement : provinces, antennes, zones_de_sante, aires_de_sante, population_totale")
      ),
      DTOutput("geo_table"),
      br(),
      fluidRow(column(
        9, actionButton("delete_row", "Supprimer la s\u00e9lection", class = "btn-danger"),
        actionButton("undo_geo", "Annuler", icon = icon("undo")),
        actionButton("redo_geo", "R\u00e9tablir", icon = icon("redo"))
      ), br(), br()),
      column(
        8,
        actionButton("clear_geo", "Tout Effacer", icon = icon("trash"), class = "btn-danger"),
        downloadButton("download_geo", "T\u00e9l\u00e9charger la base de donn\u00e9es g\u00e9ographique actuelle")
      ),
      fluidRow(
        br(),
        column(3, textInput("new_province", "Provinces")),
        column(3, textInput("new_antenne", "Antennes")),
        column(3, textInput("new_zs", "Zones de sant\u00e9")),
        column(3, textInput("new_as", "Aires de sant\u00e9")),
        column(3, numericInput("new_pop", "Population Totale", value = 0))
      ),
      fluidRow(column(3, actionButton("add_row", "Ajouter Une Entr\u00e9e", class = "btn-success"))),
      br(),
      br()
    )
  )
}
