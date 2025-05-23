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
  devtools::load_all()
  checkbox_title_style <- "border: 1px solid #ccc; background-color: white; padding: 1px; margin-bottom: 10px;"
  checkbox_style <- "height: 100px; overflow-y: scroll; background-color: white;"
  gui <- fluidPage(
    useShinyjs(),
    titlePanel("Créateur de Campagne (beta)"),
    verticalLayout(sidebarPanel(
      helpText(
        "Enter a unique campaign name. This will also be the main folder name."
      ),
      width = 8
    ), mainPanel(
      tabsetPanel(
        ## Campaign creation panel ----
        tabPanel(
          "Créer Une Campagne",
          sidebarPanel(
            textInput("campaign_name", "Nom de la Campagne"),
            dateInput("start_date", "Début", language = "fr", format = "dd/mm/yyyy"),
            dateInput("end_date", "Fin", language = "fr", format = "dd/mm/yyyy"),
            br(),
            fluidRow(
              column(
                8,
                actionLink("select_all_prov", "Sélectionner Tout / Désélectionner Tout"),
                h4("Provinces"),
                tags$div(style = checkbox_title_style, tags$div(
                  style = checkbox_style,
                  checkboxGroupInput("selected_provinces", NULL, choices = NULL)
                ))
              ),
              column(
                8,
                actionLink("select_all_ant", "Sélectionner Tout / Désélectionner Tout"),
                h4("Antennes"),
                tags$div(style = checkbox_title_style, tags$div(
                  style = checkbox_style,
                  checkboxGroupInput("selected_ant", NULL, choices = NULL)
                ))
              ),
              column(
                8,
                actionLink("select_all_zs", "Sélectionner Tout / Désélectionner Tout"),
                h4("Zones de Sante"),
                tags$div(style = checkbox_title_style, tags$div(
                  style = checkbox_style, checkboxGroupInput("selected_zs", NULL, choices = NULL)
                ))
              )
            ),
            shinyDirButton(
              "dir",
              "Sélectionner le Répertoire Cible",
              "Sélectionner un Dossier"
            ),
            verbatimTextOutput("selected_dir"),
            br(),
            actionButton("create_campaign", "Créer une Campagne", class = "btn-primary"),
            width = 20
          ),
        ),

        ## Geo panel ----
        tabPanel(
          "Géographiques",
          fluidRow(
            h4("Données Géographiques"),
            fluidRow(column(8, fileInput("upload_geo", "Télécharger Un Fichier CSV Géographique",
                                         accept = ".csv"
            ))),
            DTOutput("geo_table"),
            br(),
            fluidRow(column(
              9, actionButton("delete_row", "Supprimer la sélection", class = "btn-danger"),
              actionButton("undo_geo", "Annuler", icon = icon("undo")),
              actionButton("redo_geo", "Rétablir", icon = icon("redo"))
            ), br(), br()),
            actionButton("clear_geo", "Tout Effacer", icon = icon("trash"), class = "btn-danger"),
            downloadButton("download_geo", "Télécharger la base de données géographique actuelle"),
            fluidRow(
              br(),
              column(3, textInput("new_province", "Province")),
              column(3, textInput("new_antenne", "Antenne")),
              column(3, textInput("new_zs", "Zones de Sante")),
              column(3, textInput("new_as", "Aires de Sante")),
              column(3, numericInput("new_pop", "Population Totale", value = 0))
            ),
            fluidRow(column(3, actionButton("add_row", "Ajouter Une Entrée", class = "btn-success"))),
            br(),
            br()
          )
        ),

        ## Permission panel ----
        tabPanel(
          "Gérer les Autorisations",
          fluidRow(
            h4("Gérer les Niveaux d'Autorisation"),
            fluidRow(column(
              8,
              fileInput(
                "upload_permissions",
                "Autorisations de Téléchargement CSV",
                accept = ".csv"
              )
            )),
            DT::DTOutput("permissions_table"),
            br(),
            fluidRow(
              column(
                8, actionButton("delete_permission", "Supprimer la Sélection", class = "btn-danger"),
                actionButton("undo_perm", "Annuler", icon = icon("undo")),
                actionButton("redo_perm", "Rétablir", icon = icon("redo"))
              ),
              br(), br()
            ),
            actionButton(
              "clear_perm",
              "Tout Effacer",
              icon = icon("trash"),
              class = "btn-danger"
            ),
            downloadButton(
              "download_permissions",
              "Télécharger les Autorisations Actuelles"
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
                  options = list(placeholder = "Sélectionnez une province")
                )
              )),

              # Antenne options
              conditionalPanel(condition = "input.perm_level == 'antenne' || input.perm_level == 'zone de sante'", column(
                4,
                selectizeInput(
                  "perm_antenne",
                  "Antenne",
                  choices = NULL,
                  options = list(placeholder = "Sélectionnez une antenne")
                )
              )),

              # Zone de sante options
              conditionalPanel(condition = "input.perm_level == 'zone de sante'", column(
                4,
                selectizeInput(
                  "perm_zs",
                  "Zones de Sante",
                  choices = NULL,
                  options = list(placeholder = "Sélectionnez une zone de santé")
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
          fluidRow(
            h4("Sélection de Campagne"),
            uiOutput("campaign_drive_picker"),
            actionButton("set_permissions_btn", "Set Permissions", class = "btn-primary"),
            br(),
            br()
          )
        )
      )
    ))
  )

  # Server ----
  server <- function(input, output, session) {
    ## Loading data ----

    ### Geographic data ----

    if (!file.exists("data/geo_data.rda")) {
      geo_data <- tibble(
        provinces = character(),
        antennes = character(),
        zones_de_sante = character(),
        aires_de_sante = character(),
        population_totale = character()
      )
      usethis::use_data(geo_data)
    }

    geo_data <- arrange(geo_data)
    geo_values <- reactiveValues(data = geo_data)
    geo_data_reactive <- reactive({
      arrange(geo_values$data)
    })

    ### Permissions data ----
    if (!file.exists("data/perm_data.rda")) {
      perm_data <- tibble(
        name = character(),
        email = character(),
        level = character(),
        role = character(),
        province = character(),
        antenne = character(),
        zone_de_sante = character(),
      )
      usethis::use_data(perm_data)
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
          title = "Creating Campaign",
          "Please wait while the campaign is being created...",
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
            gdb = geo_data,
            zs_masque = file.path("data", "zs_masque_template.xlsx"),
            output_folder = selected_dir()
          )

          showModal(
            modalDialog(
              title = "Succès",
              paste(
                "Dossiers de campagne créés à :",
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
              paste("Quelque chose s'est mal passé:", e$message),
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
            showNotification("Geographies uploaded and appended successfully.",
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
                reasons <- c(reasons, "Invalid province")
              }

              if (!(is.na(row["antennes"]) || row["antennes"] == "")) {
                reasons <- c(reasons, "Invalid antennes")
              }

              if (!(is.na(row["zones_de_sante"]) || row["zones_de_sante"] == "")) {
                reasons <- c(reasons, "Invalid Zones de Sante")
              }

              if (!(is.na(row["aires_de_sante"]) || row["aires_de_sante"] == "")) {
                reasons <- c(reasons, "Invalid Aires de Sante")
              }

              if (!(is.na(row["population_totale"]) ||
                    row["population_totale"] == "" ||
                    is.numeric(row["population_totale"]))) {
                reasons <- c(reasons, "Invalid or non-numeric Population Totale")
              }

              paste(reasons, collapse = "; ")
            })

            # Show modal dialog with invalid entries
            showModal(modalDialog(
              title = "Invalid Entries",
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
                  caption = "The following rows were not added due to validation errors."
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
          usethis::use_data(geo_data, overwrite = TRUE)
          showNotification("Geographic file loaded and standardized.", type = "message")
        },
        error = \(e) {
          showNotification("Geographic file have invalid entries.")
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
      usethis::use_data(geo_data, overwrite = TRUE)
      showNotification("Données géographiques mises à jour et enregistrées",
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
        usethis::use_data(geo_data, overwrite = TRUE)
        showNotification("Ligne ajoutée et enregistrée.", type = "message")
        updateGeoCheckboxes()
      } else {
        showNotification("Entrée en double. Non ajoutée.", type = "error")
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
        usethis::use_data(geo_data, overwrite = TRUE)
        showNotification("Ligne supprimée", type = "message")
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
          "Êtes-vous sûr de vouloir effacer toutes les données géographiques?",
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
      usethis::use_data(geo_data, overwrite = TRUE)

      showNotification("Toutes les données géographiques ont été effacées",
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
    auth_status <- reactiveVal("Non authentifié")

    observeEvent(input$auth_drive, {
      drive_auth(email = NA) # Will open browser to authenticate
      if (drive_has_token()) {
        showModal(
          modalDialog(
            title = "Récupération des informations de Google Drive",
            "Veuillez patienter pendant que les données sont collectées...",
            easyClose = FALSE,
            footer = NULL
          )
        )

        query <- "mimeType = 'application/vnd.google-apps.folder' and name contains 'CAMPAGNE_'"
        folders <- googledrive::drive_find(q = query)
        files <- purrr::map(folders$id,
                            \(x) googledrive::drive_ls(googledrive::as_id(x),
                                                       recursive = TRUE))
        files <- dplyr::bind_rows(files)
        files <- googledrive::drive_reveal(files, what = "path")

        showModal(
          modalDialog(
            title = "Succès",
            "Données Google Drive collectées.",
            easyClose = TRUE,
            footer = NULL,
            style = "background-color: #ecfae8;"
          )
        )

        drive_files(files)
        campaign_drive_folders(folders)
        auth_status("✅ Suthentifié avec succès avec Google Drive.")
        show("refresh_drive")
      } else {
        auth_status("❌ Échec de l'authentification.")
      }
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
            title = "Récupération des informations de Google Drive",
            "Veuillez patienter pendant que les données sont collectées...",
            easyClose = FALSE,
            footer = NULL
          )
        )

        query <- "mimeType = 'application/vnd.google-apps.folder' and name contains 'CAMPAGNE_'"
        folders <- googledrive::drive_find(q = query)
        files <- purrr::map(folders$id,
                            \(x) googledrive::drive_ls(googledrive::as_id(x),
                                                       recursive = TRUE))
        files <- dplyr::bind_rows(files)
        files <- googledrive::drive_reveal(files, what = "path")

        showModal(
          modalDialog(
            title = "Succès",
            "Données Google Drive collectées.",
            easyClose = TRUE,
            footer = NULL,
            style = "background-color: #ecfae8;"
          )
        )

        drive_files(files)
        campaign_drive_folders(folders)
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
        "Select Campaign Folder in Google Drive",
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
            showNotification("Permissions uploaded and appended successfully.",
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
                  caption = "The following rows were not added due to validation errors."
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
          usethis::use_data(perm_data, overwrite = TRUE)
          showNotification("Permissions file loaded and standardized.", type = "message")
        },
        error = \(e) {
          showNotification("Permissions file have invalid entries.")
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
      usethis::use_data(perm_data, overwrite = TRUE)

      showNotification("Données d'autorisation mises à jour et enregistrées",
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
        usethis::use_data(perm_data, overwrite = TRUE)
        showNotification("Ligne ajoutée et enregistrée.", type = "message")
      } else {
        showNotification("Entrée en double. Non ajoutée.", type = "error")
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
        usethis::use_data(perm_data, overwrite = TRUE)
        showNotification("Ligne supprimée", type = "message")
      }
    })

    ####### Set permissions button ----

    observeEvent(input$set_permissions_btn, {
      req(input$selected_campaign_drive_folder)

      showModal(
        modalDialog(
          title = "Setting permissions",
          "Please wait while permissions are being set...",
          easyClose = FALSE,
          footer = NULL,
          style = "background-color: #faf3e8;"
        )
      )

      tryCatch(
        {
          set_permissions(
            input$selected_campaign_drive_folder,
            perm_data_reactive(),
            drive_files()
          )

          showModal(
            modalDialog(
              title = "Succès",
              "Permissions set",
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
              paste("Quelque chose s'est mal passé:", e$message),
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
          "Êtes-vous sûr de vouloir effacer toutes les autorisations?",
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
      usethis::use_data(perm_data, overwrite = TRUE)

      showNotification("Toutes les autorisations ont été effacées", type = "warning")
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
          "Zones de Sante",
          "Aires de Sante",
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
          "Province",
          "Antenne",
          "Zone de Sante"
        ),
        options = list(scrollX = TRUE, pageLength = 10)
      )
    })
  }

  shinyApp(gui, server)

}
