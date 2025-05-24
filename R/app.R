#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# UI ----

# Styles


#' Main function
#'
#' @description
#' This function gets called to run the app.
#'
#' @param ... Optional parameters. Practically speaking, not used.
#'
#' @returns None.
#' @export
#'
#' @examples
#' \dontrun{
#' campagneApp()
#' }
campagneApp <- function(...) {
  runApp(
    {
      checkbox_title_style <- "border: 1px solid #ccc; background-color: white; padding: 1px; margin-bottom: 10px;"
      checkbox_style <- "height: 100px; overflow-y: scroll; background-color: white; padding-top: 3px;"
      gui <- fluidPage(
        theme = bslib::bs_theme(
          version = 5,
          bootswatch = "yeti"
        ),
        useShinyjs(),
        tags$div(
          style = "display: flex; justify-content: space-between; align-items: center; padding: 10px 20px;",
          tags$div(
            style = "display: flex; gap: 15px;",
            imageOutput("logo", height = "60px"),
          ),
          tags$div(style = "flex-grow: 40;"),
          tags$div(
            style = "display: flex; gap: 15px;",
            imageOutput("drc_cdc_logo", height = "60px"),
          ),
        ),
        tags$hr(),
        fluidRow(
          column(12,
            align = "right",
            actionButton("end_session", label = tagList(icon("sign-out-alt"), "Quitter l'application"), class = "btn btn-danger")
          )
        ),
        tabsetPanel(
          type = "tabs",
          ## Campaign creation panel ----
          tabPanel(
            "Cr\u00e9er Une Campagne",
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
                )
              ),
              shinyDirButton(
                "dir",
                "S\u00e9lectionner le R\u00e9pertoire Cible",
                "S\u00e9lectionner un Dossier"
              ),
              verbatimTextOutput("selected_dir"),
              br(),
              actionButton("create_campaign", "Cr\u00e9er une Campagne", class = "btn-primary"),
              width = 20
            ),
          ),

          ## Geo panel ----
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
          ),

          ## Permission panel ----
          tabPanel(
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
              )),
              DT::DTOutput("permissions_table"),
              br(),
              fluidRow(
                column(
                  8, actionButton("delete_permission", "Supprimer la S\u00e9lection", class = "btn-danger"),
                  actionButton("undo_perm", "Annuler", icon = icon("undo")),
                  actionButton("redo_perm", "R\u00e9tablir", icon = icon("redo"))
                ),
                helpText(paste0(
                  'Si le niveau est "global", les zones g\u00e9ographiques sont facultatives.',
                  " Une zone g\u00e9ographique valide doit \u00eatre incluse dans les autres niveaux.\n\n",
                  'Par exemple, si le niveau est "zone de sante",',
                  ' incluez une valeur pour les colonnes "province", "antenne" et "zone de sante".'
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
              br(),
              fluidRow(column(
                8,
                h4("Authentifiez-vous avec Google Drive"),
                actionButton("auth_drive", "Authentifier", class = "btn-success"),
                actionButton(
                  "refresh_drive",
                  "Actualiser",
                  class = "btn-primary",
                  style = "display: none;"
                ),
                verbatimTextOutput("auth_status"),
                br()
              ))
            ),
            fluidRow(column(
              8,
              h4("S\u00e9lection de Campagne"),
              uiOutput("campaign_drive_picker"),
              actionButton("set_permissions_btn", "Set Permissions", class = "btn-primary"),
              br(),
              br()
            ))
          )
        ),
        tags$footer(
          style = "
    text-align: center;
    padding: 1em;
    font-size: 0.9em;
    color: #888;
    border-top: 1px solid #ddd;
    margin-top: 40px;
  ",
          "Developed by the CDC Polio Eradication Branch Surveillance, Innovation, and Research Team (2025)"
        )
      )

      # Server ----
      server <- function(input, output, session) {
        ## Loading assets (pics) ----
        showModal(modalDialog(
          title = "Chargement",
          "Veuillez patienter pendant que l'application se charge...",
          footer = NULL,
          size = "s",
          easyClose = FALSE
        ))

        ## Loading data ----

        ### Creating local data cache ----
        cache_dir <- user_data_dir("rdcAVS") # OS-specific user data dir

        if (!dir.exists(cache_dir)) {
          # First-time user: prompt for setup
          showModal(modalDialog(
            title = "Initialisation des Donn\u00e9es",
            paste0(
              "Ceci est la premi\u00e8re fois que vous ex\u00e9cutez l'application. Les donn\u00e9es g\u00e9ographiques et les autorisations seront initialis\u00e9es.",
              "\n Vous pouvez trouver les dossiers ici: ", cache_dir
            ),
            easyClose = TRUE,
            footer = modalButton("OK")
          ))

          dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
        }

        geo_cache_path <- file.path(cache_dir, "geo_data.rda")
        perm_cache_path <- file.path(cache_dir, "perm_data.rda")
        template_path <- file.path(cache_dir, "zs_masque_template.xlsx")

        ### Geographic data ----

        if (!file.exists(geo_cache_path)) {
          geo_data <- tibble(
            provinces = character(),
            antennes = character(),
            zones_de_sante = character(),
            aires_de_sante = character(),
            population_totale = character()
          )
          save(geo_data, file = geo_cache_path)
        } else {
          load(geo_cache_path)
        }

        geo_data <- arrange(geo_data)
        geo_values <- reactiveValues(data = geo_data)
        geo_data_reactive <- reactive({
          arrange(geo_values$data)
        })

        ### Permissions data ----
        if (!file.exists(perm_cache_path)) {
          perm_data <- tibble(
            name = character(),
            email = character(),
            level = character(),
            role = character(),
            province = character(),
            antenne = character(),
            zone_de_sante = character(),
          )
          save(perm_data, file = perm_cache_path)
        } else {
          load(perm_cache_path)
        }
        perm_data <- arrange(perm_data)
        perm_values <- reactiveValues(data = perm_data)
        perm_data_reactive <- reactive({
          arrange(perm_values$data)
        })

        ### Data stacks for undo/redo ----
        geo_stack <- reactiveValues(undo = list(), redo = list())
        perm_stack <- reactiveValues(undo = list(), redo = list())

        ## Campaign creation tab ----

        ### Geographic check box settings ----
        updateGeoCheckboxes <- function() {
          updateCheckboxGroupInput(
            session,
            "selected_provinces",
            choices = sort(unique(geo_values$data$provinces)),
            selected = sort(unique(geo_values$data$provinces))
          )

          updateCheckboxGroupInput(
            session,
            "selected_ant",
            choices = sort(unique(geo_values$data$antennes)),
            selected = sort(unique(geo_values$data$antennes))
          )

          updateCheckboxGroupInput(
            session,
            "selected_zs",
            choices = sort(unique(geo_values$data$zones_de_sante)),
            selected = sort(unique(geo_values$data$zones_de_sante))
          )
        }

        ### Folder selection settings ----
        volumes <- c(
          Home = fs::path_home(),
          "R Installation" = R.home(),
          getVolumes()()
        )
        shinyDirChoose(input, "dir", roots = volumes, session = session)

        selected_dir <- reactiveVal(NULL)

        observeEvent(input$dir, {
          dir_path <- tryCatch(
            {
              parseDirPath(volumes, input$dir)
            },
            error = function(e) {
              message("parseDirPath error: ", e$message)
              return(NULL)
            }
          )

          print(paste("Parsed path:", dir_path))

          if (!is.null(dir_path) &&
            length(dir_path) > 0 && !any(is.na(dir_path))) {
            selected_dir(dir_path)
          }
        })


        output$selected_dir <- renderText({
          selected_dir()
        })

        ### Observer ----
        observe({
          #### Render images ----
          output$drc_cdc_logo <- renderImage(
            {
              list(
                src = system.file("www", "drc_cdc_logo.svg", package = "rdcAVS"),
                width = 200,
                height = 80
              )
            },
            deleteFile = FALSE
          )
          output$logo <- renderImage(
            {
              list(
                src = system.file("www", "logo.svg", package = "rdcAVS"),
                width = 80,
                height = 80
              )
            },
            deleteFile = FALSE
          )

          removeModal()


          #### Geographic selection observers ----
          current_data <- geo_data_reactive()

          ##### Campaign creation tab ----

          # Provinces
          updateCheckboxGroupInput(
            session,
            "selected_provinces",
            choices = sort(unique(current_data$provinces)),
            selected = input$selected_provinces
          )

          # Antennes
          selected_prov <- input$selected_provinces
          filtered_antennes <- if (!is.null(selected_prov) &&
            length(selected_prov) > 0) {
            current_data |>
              filter(provinces %in% selected_prov) |>
              pull(antennes) |>
              unique()
          } else {
            unique(current_data |>
              filter(provinces %in% selected_prov) |>
              pull(antennes))
          }

          updateCheckboxGroupInput(
            session,
            "selected_ant",
            choices = sort(filtered_antennes),
            selected = input$selected_ant
          )

          # Zones de Sante
          selected_antennes <- input$selected_ant
          filtered_zs <- if (!is.null(selected_prov) &&
            !is.null(selected_antennes) &&
            length(selected_prov) > 0 &&
            length(selected_antennes) > 0) {
            current_data |>
              filter(
                provinces %in% selected_prov,
                antennes %in% selected_antennes
              ) |>
              pull(zones_de_sante) |>
              unique()
          } else {
            unique(current_data |>
              filter(antennes %in% selected_antennes) |>
              pull(zones_de_sante))
          }

          updateCheckboxGroupInput(
            session,
            "selected_zs",
            choices = sort(filtered_zs),
            selected = input$selected_zs
          )

          # Reset Zones de Sante if no Antennes are selected
          if (length(selected_antennes) == 0) {
            updateCheckboxGroupInput(session, "selected_zs", selected = character(0))
          }

          ##### Permissions tab ----

          req(current_data) # Ensure current_data is available

          # Update province select input
          updateSelectizeInput(
            session,
            "perm_province",
            choices = sort(unique(current_data$provinces)),
            selected = input$perm_province
          )

          # Antennes
          selected_prov <- input$perm_province
          filtered_antennes_p <- if (!is.null(selected_prov) &&
            length(selected_prov) > 0) {
            current_data |>
              filter(provinces %in% selected_prov) |>
              pull(antennes) |>
              unique()
          } else {
            unique(current_data$antennes)
          }
          updateSelectizeInput(
            session,
            "perm_antenne",
            choices = sort(unique(filtered_antennes_p)),
            selected = input$perm_antenne
          )

          # Zones de Sante
          selected_zs <- input$perm_zs
          filtered_zs_p <- if (!is.null(selected_prov) &&
            length(selected_prov) > 0) {
            current_data |>
              filter(provinces %in% selected_prov) |>
              pull(zones_de_sante) |>
              unique()
          } else {
            unique(current_data$zones_de_sante)
          }
          updateSelectizeInput(
            session,
            "perm_zs",
            choices = sort(unique(filtered_zs_p)),
            selected = input$perm_zs
          )
        })

        ### Triggered events ----

        #### Campaign creation tab ----

        ##### Select all toggle ----
        observeEvent(input$select_all_prov, {
          choices <- sort(unique(geo_values$data$provinces))
          currently_selected <- input$selected_provinces
          if (length(currently_selected) == length(choices) &&
            all(choices %in% currently_selected)) {
            updateCheckboxGroupInput(session, "selected_provinces", selected = character(0))
            updateCheckboxGroupInput(session, "selected_ant", selected = character(0)) # Reset Antennes
            updateCheckboxGroupInput(session, "selected_zs", selected = character(0)) # Reset Zones de Sante
          } else {
            updateCheckboxGroupInput(session, "selected_provinces", selected = choices)
          }
        })

        observeEvent(input$select_all_ant, {
          choices <- sort(unique(geo_values$data |>
            filter(provinces %in% input$selected_provinces) |>
            pull(antennes)))
          currently_selected <- input$selected_ant
          if (length(currently_selected) == length(choices) &&
            all(choices %in% currently_selected)) {
            updateCheckboxGroupInput(session, "selected_ant", selected = character(0))
            updateCheckboxGroupInput(session, "selected_zs", selected = character(0)) # Reset Zones de Sante
          } else {
            updateCheckboxGroupInput(session, "selected_ant", selected = choices)
          }
        })

        observeEvent(input$select_all_zs, {
          choices <- sort(unique(geo_values$data |>
            filter(antennes %in% input$selected_ant) |>
            pull(zones_de_sante)))
          currently_selected <- input$selected_zs
          if (length(currently_selected) == length(choices) &&
            all(choices %in% currently_selected)) {
            updateCheckboxGroupInput(session, "selected_zs", selected = character(0))
          } else {
            updateCheckboxGroupInput(session, "selected_zs", selected = choices)
          }
        })

        ##### Campaign creation button ----
        observeEvent(input$create_campaign, {
          req(
            input$campaign_name,
            input$start_date,
            input$end_date,
            selected_dir()
          )
          req(
            input$selected_provinces,
            input$selected_ant,
            input$selected_zs
          )

          showModal(
            modalDialog(
              title = "Cr\u00e9ation d'une campagne",
              "Veuillez patienter pendant la cr\u00e9ation de la campagne...",
              easyClose = FALSE,
              footer = NULL,
              style = "background-color: #faf3e8;"
            )
          )

          tryCatch(
            {
              init_campaign(
                start_date = input$start_date,
                end_date = input$end_date,
                campaign_name = input$campaign_name,
                campaign_folder = getwd(),
                prov_target = input$selected_provinces,
                antenne_target = input$selected_ant,
                zs_target = input$selected_zs,
                gdb = geo_data_reactive(),
                zs_masque = system.file("extdata", "zs_masque_template.xlsx",
                  package = "rdcAVS"
                ),
                output_folder = selected_dir()
              )

              showModal(
                modalDialog(
                  title = "Succ\u00e8s",
                  paste(
                    "Dossiers de campagne cr\u00e9\u00e9s \u00e0 :",
                    file.path(selected_dir(), input$campaign_name)
                  ),
                  easyClose = TRUE,
                  footer = NULL,
                  style = "background-color: #ecfae8;"
                )
              )
            },
            error = function(e) {
              showModal(
                modalDialog(
                  title = "Erreur",
                  paste("Quelque chose s'est mal pass\u00e9:", e$message),
                  easyClose = TRUE,
                  footer = NULL,
                  style = "background-color: #fae8e8;"
                )
              )
            }
          )
        })

        ##### Geographic table editing ----

        ###### Upload option for geo table ----
        observeEvent(input$upload_geo, {
          req(input$upload_geo)
          uploaded_geo <- read_csv(
            input$upload_geo$datapath,
            show_col_types = FALSE,
            na = c("", "NA")
          )
          uploaded_geo <- uploaded_geo |>
            mutate(across(any_of(
              c("provinces", "antennes", "zones_de_sante", "aires_de_sante")
            ), \(x) toupper(trimws(x))))

          tryCatch(
            {
              # Validate columns exist
              required_cols <- c(
                "provinces",
                "antennes",
                "zones_de_sante",
                "aires_de_sante",
                "population_totale"
              )

              missing_cols <- setdiff(required_cols, names(uploaded_geo))
              if (length(missing_cols) > 0) {
                showNotification(paste(
                  "Missing columns:",
                  paste(missing_cols, collapse = ", ")
                ), type = "error")
                return()
              }

              # Create validation flags

              uploaded_geo$province_valid <- is.na(uploaded_geo$provinces) |
                uploaded_geo$provinces == "" | is.character(uploaded_geo$provinces)

              uploaded_geo$antenne_valid <- is.na(uploaded_geo$antennes) |
                uploaded_geo$antennes == "" | is.character(uploaded_geo$antennes)

              uploaded_geo$zs_valid <- is.na(uploaded_geo$zones_de_sante) |
                uploaded_geo$zones_de_sante == "" | is.character(uploaded_geo$zones_de_sante)

              uploaded_geo$as_valid <- is.na(uploaded_geo$aires_de_sante) |
                uploaded_geo$aires_de_sante == "" | is.character(uploaded_geo$aires_de_sante)

              uploaded_geo$pop_valid <- is.na(uploaded_geo$population_totale) |
                uploaded_geo$population_totale == "" |
                is.numeric(uploaded_geo$population_totale)

              # Identify invalid rows
              uploaded_geo$all_valid <- with(
                uploaded_geo,
                province_valid &
                  antenne_valid &
                  zs_valid &
                  as_valid &
                  pop_valid
              )

              # Validate levels and roles
              if (all(uploaded_geo$all_valid)) {
                showNotification("G\u00e9ographies t\u00e9l\u00e9charg\u00e9es et ajout\u00e9es avec succ\u00e8s.",
                  type = "message"
                )
                uploaded_geo <- uploaded_geo |>
                  select(!ends_with("_valid"))
                valid_geo <- uploaded_geo
              } else {
                invalid_rows <- uploaded_geo[!uploaded_geo$all_valid, ]

                # Add explanation for invalid entries
                invalid_rows$error_reason <- apply(invalid_rows, 1, function(row) {
                  reasons <- c()

                  if (!(is.na(row["provinces"]) || row["provinces"] == "")) {
                    reasons <- c(reasons, "Province invalide")
                  }

                  if (!(is.na(row["antennes"]) || row["antennes"] == "")) {
                    reasons <- c(reasons, "Antenne invalide")
                  }

                  if (!(is.na(row["zones_de_sante"]) || row["zones_de_sante"] == "")) {
                    reasons <- c(reasons, "Zone de sant\u00e9 invalide")
                  }

                  if (!(is.na(row["aires_de_sante"]) || row["aires_de_sante"] == "")) {
                    reasons <- c(reasons, "Aire de sant\u00e9 invalide")
                  }

                  if (!(is.na(row["population_totale"]) ||
                    row["population_totale"] == "" ||
                    is.numeric(row["population_totale"]))) {
                    reasons <- c(reasons, "Total de la population non valide ou non num\u00e9rique")
                  }

                  paste(reasons, collapse = "; ")
                })

                # Show modal dialog with invalid entries
                showModal(modalDialog(
                  title = "Entr\u00e9es invalides",
                  renderUI({
                    datatable(
                      invalid_rows[, c(
                        "provinces",
                        "antennes",
                        "zones_de_sante",
                        "aires_de_sante",
                        "population_totale",
                        "error_reason"
                      )],
                      options = list(pageLength = 5, scrollX = TRUE),
                      caption = "Les lignes suivantes n'ont pas \u00e9t\u00e9 ajout\u00e9es en raison d'erreurs de validation."
                    )
                  }),
                  easyClose = TRUE,
                  footer = NULL
                ))

                valid_geo <- anti_join(
                  uploaded_geo |>
                    select(!ends_with("_valid")),
                  invalid_rows[, c(
                    "provinces",
                    "antennes",
                    "zones_de_sante",
                    "aires_de_sante",
                    "population_totale"
                  )]
                )
              }

              current_geos <- geo_values$data

              if (nrow(current_geos) == 0) {
                combined_geos <- valid_geo |> distinct()
              } else {
                combined_geos <- dplyr::bind_rows(current_geos, valid_geo) |>
                  dplyr::distinct()
              }

              geo_values$data <- arrange(combined_geos)

              geo_data <- combined_geos
              save(geo_data, file = geo_cache_path)
              showNotification("Fichier g\u00e9ographique charg\u00e9 et standardis\u00e9.", type = "message")
            },
            error = \(e) {
              showNotification("Le fichier g\u00e9ographique contient des entr\u00e9es non valides.")
            }
          )
        })

        ###### Download current geo table ----
        output$download_geo <- downloadHandler(
          filename = function() {
            paste0("geo_table_", Sys.Date(), ".csv")
          },
          content = function(file) {
            write_csv(geo_data_reactive(), file, na = "")
          }
        )

        ###### Edit geo table ----
        observeEvent(input$geo_table_cell_edit, {
          geo_stack$undo <- c(list(geo_values$data), geo_stack$undo)
          geo_stack$redo <- list()
          info <- input$geo_table_cell_edit

          # Get the column name based on the index
          col_name <- colnames(geo_values$data)[info$col + 1]

          if (col_name != "population_totale") {
            geo_values$data[info$row, col_name] <- toupper(info$value)
          } else {
            if (info$value %in% c("NA", "", NA, NULL)) {
              info$value <- 0
            }
            geo_values$data[info$row, col_name] <- info$value
          }

          geo_values$data <- arrange(distinct(geo_values$data))

          geo_data <- arrange(geo_values$data)
          save(geo_data, file = geo_cache_path)
          showNotification("Donn\u00e9es g\u00e9ographiques mises \u00e0 jour et enregistr\u00e9es",
            type = "message"
          )
          updateGeoCheckboxes()
        })

        ###### Add geographic entry ----
        observeEvent(input$add_row, {
          geo_stack$undo <- c(list(geo_values$data), geo_stack$undo)
          geo_stack$redo <- list()
          new_row <- data.frame(
            provinces = toupper(input$new_province),
            antennes = toupper(input$new_antenne),
            zones_de_sante = toupper(input$new_zs),
            aires_de_sante = toupper(input$new_as),
            population_totale = input$new_pop,
            stringsAsFactors = FALSE
          )
          if (!any(duplicated(rbind(geo_values$data, new_row)))) {
            geo_values$data <- arrange(rbind(geo_values$data, new_row))

            geo_data <- geo_values$data
            save(geo_data, file = geo_cache_path)
            showNotification("Ligne ajout\u00e9e et enregistr\u00e9e.", type = "message")
            updateGeoCheckboxes()
          } else {
            showNotification("Entr\u00e9e en double. Non ajout\u00e9e.", type = "error")
          }
        })

        ###### Delete geographic entry ----
        observeEvent(input$delete_row, {
          geo_stack$undo <- c(list(geo_values$data), geo_stack$undo)
          geo_stack$redo <- list()
          selected <- input$geo_table_rows_selected
          if (length(selected)) {
            geo_values$data <- arrange(geo_values$data[-selected, ])
            geo_data <- geo_values$data
            save(geo_data, file = geo_cache_path)
            showNotification("Ligne supprim\u00e9e", type = "message")
            updateGeoCheckboxes()
          }
        })

        ###### Undo/redo/clear all geo table ----
        observeEvent(input$undo_geo, {
          if (length(geo_stack$undo) > 0) {
            geo_stack$redo <- c(list(geo_values$data), geo_stack$redo)
            geo_values$data <- geo_stack$undo[[1]]
            geo_stack$undo <- geo_stack$undo[-1]
            updateGeoCheckboxes()
          }
        })

        observeEvent(input$redo_geo, {
          if (length(geo_stack$redo) > 0) {
            geo_stack$undo <- c(list(geo_values$data), geo_stack$undo)
            geo_values$data <- geo_stack$redo[[1]]
            geo_stack$redo <- geo_stack$redo[-1]
            updateGeoCheckboxes()
          }
        })

        observeEvent(input$clear_geo, {
          showModal(
            modalDialog(
              title = "Confirmation",
              "\u00cates-vous s\u00fbr de vouloir effacer toutes les donn\u00e9es g\u00e9ographiques?",
              footer = tagList(
                modalButton("Annuler"),
                actionButton("confirm_clear_geo", "Confirmer", class = "btn-danger")
              )
            )
          )
        })

        observeEvent(input$confirm_clear_geo, {
          removeModal()
          geo_stack$undo <- c(list(geo_values$data), geo_stack$undo)
          geo_stack$redo <- list()
          geo_values$data <- geo_values$data[0, ]

          geo_data <- geo_values$data
          save(geo_data, file = geo_cache_path)

          showNotification("Toutes les donn\u00e9es g\u00e9ographiques ont \u00e9t\u00e9 effac\u00e9es",
            type = "warning"
          )
          updateGeoCheckboxes()
        })

        ##### Setting permissions ----

        ###### Authenticate via Google Drive ----
        # Track auth status
        # Dynamically list campaign folders in Drive after auth
        drive_files <- reactiveVal(NULL)
        campaign_drive_folders <- reactiveVal(NULL)
        auth_status <- reactiveVal("Non authentifi\u00e9")

        if (drive_has_token()) {
          query <- "mimeType = 'application/vnd.google-apps.folder' and name contains 'CAMPAGNE_'"
          folders <- googledrive::drive_find(q = query)
          campaign_drive_folders(folders)
          auth_status("\u2705 Suthentifi\u00e9 avec succ\u00e8s avec Google Drive.")
          show("refresh_drive")
        }

        observeEvent(input$auth_drive, {
          if (!drive_has_token()) {
            drive_auth(email = NA) # Will open browser to authenticate
          }

          tryCatch(
            {
              showModal(
                modalDialog(
                  title = "R\u00e9cup\u00e9ration des informations de Google Drive",
                  "Veuillez patienter pendant que les donn\u00e9es sont collect\u00e9es...",
                  easyClose = FALSE,
                  footer = NULL
                )
              )

              query <- "mimeType = 'application/vnd.google-apps.folder' and name contains 'CAMPAGNE_'"
              folders <- googledrive::drive_find(q = query)
              campaign_drive_folders(folders)

              showModal(
                modalDialog(
                  title = "Succ\u00e8s",
                  "Donn\u00e9es Google Drive collect\u00e9es.",
                  easyClose = TRUE,
                  footer = NULL,
                  style = "background-color: #ecfae8;"
                )
              )
              auth_status("\u2705 Suthentifi\u00e9 avec succ\u00e8s avec Google Drive.")
              show("refresh_drive")
            },
            error = \(e) {
              auth_status("\u274c \u00c9chec de l'authentification.")
            }
          )
        })

        output$auth_status <- renderText({
          auth_status()
        })

        ###### Refresh Google Drive ----
        # Function to update Google Drive files
        updateDriveFiles <- function() {
          if (drive_has_token()) {
            showModal(
              modalDialog(
                title = "R\u00e9cup\u00e9ration des informations de Google Drive",
                "Veuillez patienter pendant que les donn\u00e9es sont collect\u00e9es...",
                easyClose = FALSE,
                footer = NULL
              )
            )

            query <- "mimeType = 'application/vnd.google-apps.folder' and name contains 'CAMPAGNE_'"
            folders <- googledrive::drive_find(q = query)
            campaign_drive_folders(folders)

            files <- drive_files()

            if (!is.null(files)) {
              folders <- campaign_drive_folders()
              files <- purrr::map(folders$id, \(x) {
                googledrive::drive_ls(googledrive::as_id(x), recursive = TRUE)
              }) |>
                dplyr::bind_rows() |>
                googledrive::drive_reveal(what = "path")

              drive_files(files)

              removeModal()
              showNotification("Fichiers Google Drive actualis\u00e9s.", type = "message")
            }

            showModal(
              modalDialog(
                title = "Succ\u00e8s",
                "Donn\u00e9es Google Drive collect\u00e9es.",
                easyClose = TRUE,
                footer = NULL,
                style = "background-color: #ecfae8;"
              )
            )
          }
        }

        observeEvent(input$refresh_drive, {
          updateDriveFiles() # Refresh the list of files
        })

        ###### Display available campaigns ----
        output$campaign_drive_picker <- renderUI({
          folders <- campaign_drive_folders()
          selectInput(
            "selected_campaign_drive_folder",
            "S\u00e9lectionnez le dossier de campagne dans Google Drive",
            choices = folders$name
          )
        })

        ###### Upload option for permissions table ----
        # Handle file upload
        observeEvent(input$upload_permissions, {
          req(input$upload_permissions)
          uploaded_permissions <- read_csv(
            input$upload_permissions$datapath,
            show_col_types = FALSE,
            na = c("", "NA")
          )
          uploaded_permissions <- uploaded_permissions |>
            mutate(across(any_of(
              c("province", "antenne", "zone_de_sante")
            ), \(x) toupper(trimws(x)))) |>
            mutate(across(
              any_of(c("email", "level", "role")),
              \(x) tolower(trimws(x))
            ))

          tryCatch(
            {
              # Checking for valid inputs
              # Get valid geo options
              geo_lookup <- geo_data_reactive()
              valid_levels <- c("global", "province", "antenne", "zone de sante")
              valid_roles <- c("writer", "reader", "commenter")
              valid_provinces <- unique(geo_lookup$provinces)
              valid_antenne <- unique(geo_lookup$antennes)
              valid_zs <- unique(geo_lookup$zones_de_sante)

              # Validate columns exist
              required_cols <- c(
                "name",
                "email",
                "level",
                "role",
                "province",
                "antenne",
                "zone_de_sante"
              )

              missing_cols <- setdiff(required_cols, names(uploaded_permissions))
              if (length(missing_cols) > 0) {
                showNotification(paste(
                  "Missing columns:",
                  paste(missing_cols, collapse = ", ")
                ), type = "error")
                return()
              }

              # Create validation flags
              uploaded_permissions$level_valid <- uploaded_permissions$level %in% valid_levels
              uploaded_permissions$role_valid <- uploaded_permissions$role %in% valid_roles

              uploaded_permissions$province_valid <- is.na(uploaded_permissions$province) |
                uploaded_permissions$province == "" |
                uploaded_permissions$province %in% valid_provinces

              uploaded_permissions$antenne_valid <- is.na(uploaded_permissions$antenne) |
                uploaded_permissions$antenne == "" |
                uploaded_permissions$antenne %in% valid_antenne

              uploaded_permissions$zs_valid <- is.na(uploaded_permissions$zone_de_sante) |
                uploaded_permissions$zone_de_sante == "" |
                uploaded_permissions$zone_de_sante %in% valid_zs

              # Identify invalid rows
              uploaded_permissions$all_valid <- with(
                uploaded_permissions,
                level_valid &
                  role_valid &
                  province_valid &
                  antenne_valid &
                  zs_valid
              )

              # Validate levels and roles
              if (all(uploaded_permissions$all_valid)) {
                showNotification("Les autorisations ont \u00e9t\u00e9 t\u00e9l\u00e9charg\u00e9es et ajout\u00e9es avec succ\u00e8s.",
                  type = "message"
                )
                valid_permissions <- uploaded_permissions |>
                  select(!ends_with("_valid"))
              } else {
                invalid_rows <- uploaded_permissions[!uploaded_permissions$all_valid, ]

                # Add explanation for invalid entries
                invalid_rows$error_reason <- apply(invalid_rows, 1, function(row) {
                  reasons <- c()
                  if (!(row["level"] %in% valid_levels)) {
                    reasons <- c(reasons, "Invalid level")
                  }
                  if (!(row["role"] %in% valid_roles)) {
                    reasons <- c(reasons, "Invalid role")
                  }
                  if (!(is.na(row["province"]) ||
                    row["province"] == "" ||
                    row["province"] %in% valid_provinces)) {
                    reasons <- c(reasons, "Invalid province")
                  }
                  if (!(is.na(row["antenne"]) ||
                    row["antenne"] == "" ||
                    row["antenne"] %in% valid_antenne)) {
                    reasons <- c(reasons, "Invalid antenne")
                  }
                  if (!(is.na(row["zone_de_sante"]) ||
                    row["zone_de_sante"] == "" ||
                    row["city"] %in% valid_zs)) {
                    reasons <- c(reasons, "Invalid zone de sante")
                  }
                  paste(reasons, collapse = "; ")
                })

                # Show modal dialog with invalid entries
                showModal(modalDialog(
                  title = "Invalid Entries",
                  renderUI({
                    datatable(
                      invalid_rows[, c(
                        "email",
                        "level",
                        "role",
                        "province",
                        "antenne",
                        "zone_de_sante",
                        "error_reason"
                      )],
                      options = list(pageLength = 5, scrollX = TRUE),
                      caption = "Les lignes suivantes n'ont pas \u00e9t\u00e9 ajout\u00e9es en raison d'erreurs de validation."
                    )
                  }),
                  easyClose = TRUE,
                  footer = NULL
                ))

                valid_permissions <- anti_join(
                  uploaded_permissions |>
                    select(!ends_with("_valid")),
                  invalid_rows[, c(
                    "email", "level",
                    "role",
                    "province",
                    "antenne",
                    "zone_de_sante"
                  )]
                )
              }

              current_permissions <- perm_values$data

              if (nrow(current_permissions) == 0) {
                combined_permissions <- valid_permissions
              } else {
                combined_permissions <- dplyr::bind_rows(current_permissions, valid_permissions) |>
                  dplyr::distinct()
              }

              perm_values$data <- arrange(combined_permissions)
              perm_data <- perm_values$data
              save(perm_data, file = perm_cache_path)
              showNotification("Fichier d'autorisations charg\u00e9 et standardis\u00e9.", type = "message")
            },
            error = \(e) {
              showNotification("Le fichier d'autorisations contient des entr\u00e9es non valides.")
            }
          )
        })

        ####### Download current permissions table ----
        output$download_permissions <- downloadHandler(
          filename = function() {
            paste0("permissions_table_", Sys.Date(), ".csv")
          },
          content = function(file) {
            write_csv(perm_data_reactive(), file, na = "")
          }
        )

        ####### Edit permissions table ----
        # Automatically save edits to geo_data when a cell is edited
        observeEvent(input$permissions_table_cell_edit, {
          perm_stack$undo <- c(list(perm_values$data), perm_stack$undo)
          perm_stack$redo <- list()

          info <- input$permissions_table_cell_edit

          # Get the column name based on the index
          col_name <- colnames(perm_values$data)[info$col + 1]

          if (col_name %in% c("email", "level", "role")) {
            perm_values$data[info$row, col_name] <- tolower(info$value)
          } else if (col_name %in% c("province", "antenne", "zone_de_sante")) {
            perm_values$data[info$row, col_name] <- toupper(info$value)
          } else {
            perm_values$data[info$row, col_name] <- info$value
          }

          perm_values$data <- arrange(distinct(perm_values$data))

          perm_data <- perm_values$data
          save(perm_data, file = perm_cache_path)

          showNotification("Donn\u00e9es d'autorisation mises \u00e0 jour et enregistr\u00e9es",
            type = "message"
          )
          updateGeoCheckboxes()
        })

        ####### Add new permission entry ----
        observeEvent(input$add_permission, {
          perm_stack$undo <- c(list(perm_values$data), perm_stack$undo)
          perm_stack$redo <- list()

          # Validate required fields
          req(
            input$perm_name,
            input$perm_email,
            input$perm_level,
            input$perm_role
          )

          # Convert to uppercase, strip whitespace
          name <- stringr::str_to_title(input$perm_name)
          email <- tolower(trimws(input$perm_email))
          level <- tolower(input$perm_level)
          role <- tolower(input$perm_role)

          # Optional fields based on level
          province <- if (level %in% c("province", "antenne") &&
            length(input$perm_province) > 0) {
            input$perm_province
          } else {
            NA_character_
          }
          antenne <- if (level == "antenne" &&
            length(input$perm_antenne) > 0) {
            input$perm_antenne
          } else {
            NA_character_
          }
          zone_de_sante <- if (level == "zone de sante" &&
            length(input$perm_zs) > 0) {
            input$perm_zs
          } else {
            NA_character_
          }

          # Ensure unique entries by checking if a row already exists
          new_row <- data.frame(
            name = name,
            email = email,
            level = level,
            role = role,
            province = province,
            antenne = antenne,
            zone_de_sante = zone_de_sante,
            stringsAsFactors = FALSE
          )

          # Check for duplicates (optional)
          if (!any(duplicated(rbind(perm_values$data, new_row)))) {
            perm_values$data <- arrange(rbind(perm_values$data, new_row))

            perm_data <- perm_values$data
            save(perm_data, file = perm_cache_path)
            showNotification("Ligne ajout\u00e9e et enregistr\u00e9e.", type = "message")
          } else {
            showNotification("Entr\u00e9e en double. Non ajout\u00e9e.", type = "error")
          }
        })

        ####### Delete permission entry ----
        observeEvent(input$delete_permission, {
          perm_stack$undo <- c(list(perm_values$data), perm_stack$undo)
          perm_stack$redo <- list()

          selected <- input$permissions_table_rows_selected

          if (length(selected)) {
            perm_values$data <- arrange(perm_values$data[-selected, ])
            perm_data <- perm_values$data
            save(perm_data, file = perm_cache_path)
            showNotification("Ligne supprim\u00e9e", type = "message")
          }
        })

        ####### Set permissions button ----

        observeEvent(input$set_permissions_btn, {
          req(input$selected_campaign_drive_folder)

          showModal(
            modalDialog(
              title = "D\u00e9finition des autorisations",
              "Veuillez patienter pendant que les autorisations sont d\u00e9finies...",
              easyClose = FALSE,
              footer = NULL,
              style = "background-color: #faf3e8;"
            )
          )

          folders <- campaign_drive_folders()
          files <- drive_files()

          if (is.null(files)) {
            files <- purrr::map(
              folders$id,
              \(x) googledrive::drive_ls(googledrive::as_id(x),
                recursive = TRUE
              )
            )
            files <- dplyr::bind_rows(files)
            drive_files(files)
          }

          files <- drive_files()
          if (!"path" %in% names(files)) {
            files <- googledrive::drive_reveal(files, what = "path")
            drive_files(files)
          }

          tryCatch(
            {
              set_permissions(
                input$selected_campaign_drive_folder,
                perm_data_reactive(),
                drive_files()
              )

              showModal(
                modalDialog(
                  title = "Succ\u00e8s",
                  "Autorisations d\u00e9finies",
                  easyClose = TRUE,
                  footer = NULL,
                  style = "background-color: #ecfae8;"
                )
              )
            },
            error = function(e) {
              showModal(
                modalDialog(
                  title = "Erreur",
                  paste("Quelque chose s'est mal pass\u00e9:", e$message),
                  easyClose = TRUE,
                  footer = NULL,
                  style = "background-color: #fae8e8;"
                )
              )
            }
          )
        })

        ###### Undo/redo/clear all perm table ----
        observeEvent(input$undo_perm, {
          if (length(perm_stack$undo) > 0) {
            perm_stack$redo <- c(list(perm_values$data), perm_stack$redo)
            perm_values$data <- perm_stack$undo[[1]]
            perm_stack$undo <- perm_stack$undo[-1]
          }
        })

        observeEvent(input$redo_perm, {
          if (length(perm_stack$redo) > 0) {
            perm_stack$undo <- c(list(perm_values$data), perm_stack$undo)
            perm_values$data <- perm_stack$redo[[1]]
            perm_stack$redo <- perm_stack$redo[-1]
          }
        })

        observeEvent(input$clear_perm, {
          showModal(
            modalDialog(
              title = "Confirmation",
              "\u00cates-vous s\u00fbr de vouloir effacer toutes les autorisations?",
              footer = tagList(
                modalButton("Annuler"),
                actionButton("confirm_clear_perm", "Confirmer", class = "btn-danger")
              )
            )
          )
        })

        observeEvent(input$confirm_clear_perm, {
          removeModal()
          perm_stack$undo <- c(list(perm_values$data), perm_stack$undo)
          perm_stack$redo <- list()
          perm_values$data <- perm_values$data[0, ]
          perm_data <- perm_values$data
          save(perm_data, file = perm_cache_path)

          showNotification("Toutes les autorisations ont \u00e9t\u00e9 effac\u00e9es", type = "warning")
        })

        #### End session ----
        observeEvent(input$end_session, {
          showModal(
            modalDialog(
              title = "Confirmation",
              "\u00cates-vous s\u00fbr de vouloir quitter l'application ?",
              footer = tagList(
                modalButton("Annuler"),
                actionButton("confirm_quit", "Confirmer", class = "btn-danger")
              ), easyClose = TRUE
            )
          )
        })

        observeEvent(input$confirm_quit, {
          removeModal()
          showModal(modalDialog(
            title = "S\u00e9ance termin\u00e9e",
            "Vous pouvez maintenant fermer le navigateur.",
            easyClose = FALSE
          ))
          stopApp()
        })

        ### Outputs ----
        #### Geographic table ----
        output$geo_table <- renderDT({
          datatable(
            geo_data_reactive(),
            editable = TRUE,
            rownames = FALSE,
            colnames = c(
              "Province",
              "Antennes",
              "Zones de sant\u00e9",
              "Aires de sant\u00e9",
              "Population Totale"
            ),
            options = list(pageLength = 10, language = "fr")
          )
        })

        #### Permissions table ----
        output$permissions_table <- DT::renderDT({
          DT::datatable(
            perm_data_reactive(),
            selection = "single",
            rownames = FALSE,
            editable = TRUE,
            colnames = c(
              "Name",
              "Email",
              "Level",
              "Role",
              "Provinces",
              "Antennes",
              "Zones de sant\u00e9"
            ),
            options = list(scrollX = TRUE, pageLength = 10)
          )
        })
      }

      shinyApp(gui, server)
    },
    launch.browser = TRUE
  )
}
