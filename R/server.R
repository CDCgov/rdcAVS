server <- function(input, output, session) {

  ## Loading data ----

  ### Creating local data cache ----
  cache_dir <- user_data_dir("rdcAVS") # OS-specific user data dir
  geo_cache_path <- file.path(cache_dir, "geo_data.rda")
  perm_cache_path <- file.path(cache_dir, "perm_data.rda")
  data_quality_path <- file.path(cache_dir, "data_quality_info.rda")
  campaign_quality_path <- file.path(cache_dir, "campaign_quality_info.rda")
  invalid_rows <- NULL

  cli::cli_alert(paste0("Cache dir: ", cache_dir))

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

  # Geographic cache ----
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

  # Permissions cache ----
  if (!file.exists(perm_cache_path)) {
    perm_data <- tibble(
      name = character(),
      phone = character(),
      notes = character(),
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

  ### Cache reactive values ----

  geo_data <- arrange(geo_data)
  geo_values <- reactiveValues(data = geo_data)
  geo_data_reactive <- reactive({
    arrange(geo_values$data)
  })

  perm_data <- arrange(perm_data)
  perm_values <- reactiveValues(data = perm_data)
  perm_data_reactive <- reactive({
    arrange(perm_values$data)
  })

  ### Monitoring data ----
  surveillance_summary <- reactiveVal(NULL)
  campaign_quality <- reactiveVal(NULL)
  refresh_status <- reactiveVal("No data selected")

  if (file.exists(data_quality_path)) {
    refresh_file_info <- file.info(data_quality_path)
    refresh_status(paste0("Last updated on: ",
                          refresh_file_info$mtime))
    load(data_quality_path)

    surveillance_summary(data_quality_info)

    show("refresh_date")
    show("download_data_quality_monitoring")

  } else {
    data_quality_info <- dplyr::tibble(
      prov = character(),
      antenne = character(),
      zone_de_sante = character(),
      sheet = character(),
      range = character(),
      section = character(),
      filled_cells = integer(),
      total_cells = integer(),
      completeness = character(),
      days_since_last_modified = integer(),
      date_ran = as.Date(character())
    )
    save(data_quality_info, file = data_quality_path)
    surveillance_summary(data_quality_info)
  }

  if (file.exists(campaign_quality_path)) {
    load(campaign_quality_path)
    campaign_quality(campaign_quality_info)
    show("download_campaign_quality_monitoring")
  } else {
    campaign_quality_info <- dplyr::tibble(
      province = character(),
      antenne = character(),
      zone_de_sante = character(),
      aire_de_sante = character(),
      jour = character(),
      rapport_completude_pct = numeric(),
      couverture_campaign_cumulative = numeric(),
      avg_vax_rural = numeric(),
      avg_vax_urban = numeric(),
      recovery_0_11_cumulative = numeric(),
      recovery_12_23_cumulative = numeric(),
      recovery_24_59_cumulative = numeric(),
    )
    save(campaign_quality_info, file = campaign_quality_path)
    campaign_quality(campaign_quality_info)
  }

  ### Data stacks for undo/redo ----
  geo_stack <- reactiveValues(undo = list(), redo = list())
  perm_stack <- reactiveValues(undo = list(), redo = list())

  ## Campaign creation tab ----

  ### Geographic check box settings ----
  update_geo_checkboxes <- function() {
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
          width = 180,
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


    # Selected provinces
    campaign_completeness <- campaign_quality()

    updateSelectInput(
      session,
      "prov_selector_campaign_completeness",
      choices = sort(unique(campaign_completeness$province)),
      selected = input$prov_selector_campaign_completeness
    )

    selected_campaign_prov <- input$prov_selector_campaign_completeness
    filtered_campaign_zs <- if (!is.null(selected_campaign_prov) &&
                                length(selected_campaign_prov) > 0) {
      campaign_completeness |>
        dplyr::filter(province == selected_campaign_prov) |>
        dplyr::pull(zone_de_sante) |>
        unique() |>
        sort()
    } else {
      unique(campaign_completeness$zone_de_sante)
    }

    updateSelectInput(
      session,
      "zs_selector_campaign_completeness",
      choices = filtered_campaign_zs)
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
      input$zs_template_url
    )
    req(
      input$selected_provinces,
      input$selected_ant,
      input$selected_zs
    )

    showNotification("Veuillez patienter pendant la cr\u00e9ation de la campagne...",
                     type = "default")

    tryCatch(
      {
        zs_masque_dribble <- googledrive::drive_get(stringr::str_trim(input$zs_template_url))
        campagne_folder_url <- drive_init_campaign(
          start_date = input$start_date,
          end_date = input$end_date,
          campaign_name = input$campaign_name,
          prov_target = input$selected_provinces,
          antenne_target = input$selected_ant,
          zs_target = input$selected_zs,
          gdb = geo_data_reactive(),
          zs_masque = zs_masque_dribble
        )

        showNotification("Campagne initialis\u00e9e avec succ\u00e8s",
                         type = "message")
        output$campagne_folder_url <- renderUI({tagList(a("Lien vers le dossier Campagne", href = campagne_folder_url))})
      },
      error = function(e) {
        showNotification(paste("Erreur - " , "Quelque chose s'est mal pass\u00e9:", e$message),
                         type = "error",
                         duration = NULL)

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
      na = c("", "NA"),
      col_types = "ccccn"
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
    filename = paste0("geo_table_", Sys.Date(), ".csv"),
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
    update_geo_checkboxes()
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
      update_geo_checkboxes()
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
      update_geo_checkboxes()
    }
  })

  ###### Undo/redo/clear all geo table ----
  observeEvent(input$undo_geo, {
    if (length(geo_stack$undo) > 0) {
      geo_stack$redo <- c(list(geo_values$data), geo_stack$redo)
      geo_values$data <- geo_stack$undo[[1]]
      geo_stack$undo <- geo_stack$undo[-1]
      update_geo_checkboxes()
    }
  })

  observeEvent(input$redo_geo, {
    if (length(geo_stack$redo) > 0) {
      geo_stack$undo <- c(list(geo_values$data), geo_stack$undo)
      geo_values$data <- geo_stack$redo[[1]]
      geo_stack$redo <- geo_stack$redo[-1]
      update_geo_checkboxes()
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
    update_geo_checkboxes()
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
    show("refresh_campaign")
  }

  observeEvent(input$auth_drive, {
    if (!drive_has_token()) {
      drive_auth(email = FALSE) # Will open browser to authenticate
    }

    tryCatch(
      {
        query <- "mimeType = 'application/vnd.google-apps.folder' and name contains 'CAMPAGNE_'"
        folders <- googledrive::drive_find(q = query)
        campaign_drive_folders(folders)

        showNotification("Donn\u00e9es Google Drive collect\u00e9es.",
                         type = "message")

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
  update_drive_files <- function() {
    if (drive_has_token()) {
      showNotification(
          paste0("R\u00e9cup\u00e9ration des informations de Google Drive. ",
          "Veuillez patienter pendant que les donn\u00e9es sont collect\u00e9es..."),
          type = "default"
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
        showNotification("Fichiers Google Drive actualis\u00e9s.", type = "message")
      }

      showNotification("Succ\u00e8s. Donn\u00e9es Google Drive collect\u00e9es.",
                       type = "message")

    }
  }

  observeEvent(input$refresh_drive, {
    update_drive_files() # Refresh the list of files
  })

  ###### Display available campaigns ----
  output$campaign_drive_picker <- renderUI({
    folders <- campaign_drive_folders()
    selectInput(
      "selected_campaign_drive_folder",
      NULL,
      choices = folders$name
    )
  })

  ###### Display available campaigns in the surveillance tab ----
  output$campaign_surveillance <- renderUI({
    selectInput(
      "selected_surveillance_drive_folder",
      h6("Campagne"),
      choices = campaign_drive_folders()$name
    )
  })
  output$refresh_date <- renderText({
    refresh_status()
  })

  ###### Upload option for permissions table ----
  # Handle file upload
  observeEvent(input$upload_permissions, {
    req(input$upload_permissions)
    uploaded_permissions <- read_csv(
      input$upload_permissions$datapath,
      show_col_types = FALSE,
      na = c("", "NA"),
      col_types = "ccccccccc"
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
          "phone",
          "notes",
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
                  row["zone_de_sante"] %in% valid_zs)) {
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
            footer = tagList(
              downloadButton("download_invalid", "T\u00e9l\u00e9charger les entr\u00e9es invalides"),
              modalButton("Fermer"))
          ))

          ####### Download invalid permission entries ----

          output$download_invalid <- downloadHandler(
            filename = paste0("invalid_entries_", Sys.Date(), ".csv"),
            content = function(file) {
              write.csv(invalid_rows, file, row.names = FALSE)
            }
          )

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

  ####### Download invalid permission entries ----

  output$download_invalid <- downloadHandler(
    filename = paste0("invalid_entries_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(invalid_rows, file, row.names = FALSE)
    }
  )

  ####### Download current permissions table ----
  output$download_permissions <- downloadHandler(
    filename = paste0("permissions_table_", Sys.Date(), ".csv"),
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
    update_geo_checkboxes()
  })

  ####### Add new permission entry ----
  observeEvent(input$add_permission, {
    perm_stack$undo <- c(list(perm_values$data), perm_stack$undo)
    perm_stack$redo <- list()

    level <- tolower(input$perm_level)
    # Check for required fields
    missing_fields <- c()
    if (input$perm_email == "") {
      missing_fields <- c(missing_fields, "Email")
    }
    if (input$perm_role == "") {
      missing_fields <- c(missing_fields, "Role")
    }

    if (level == "province" && input$perm_province == "") {
      missing_fields <- c(missing_fields, "Province")
    }

    if (level == "antenne" && (input$perm_province == "" || input$perm_antenne == "")) {
      if (input$perm_province == "") {
        missing_fields <- c(missing_fields, "Province")
      }

      if (input$perm_antenne == "") {
        missing_fields <- c(missing_fields, "Antenne")
      }

    }

    if (level == "zone de sante" && (input$perm_province == "" || input$perm_antenne == "" || input$perm_zs == "")) {
      if (input$perm_province == "") {missing_fields <- c(missing_fields, "Province")}
      if (input$perm_antenne == "") {missing_fields <- c(missing_fields, "Antenne")}
      if (input$perm_zs == "") {missing_fields <- c(missing_fields, "Zone de Sant\u00e9")}
    }

    if (length(missing_fields) > 0) {
      showNotification(
        paste("Veuillez remplir les champs obligatoires :", paste(unique(missing_fields), collapse = ", ")),
        type = "error"
      )
      return()
    }

    switch(level,
           "global" = {
             req(
               input$perm_email,
               input$perm_level,
               input$perm_role
             )
           },
           "province" = {
             req(
               input$perm_email,
               input$perm_level,
               input$perm_role,
               input$perm_province
             )
           },
           "antenne" = {
             req(
               input$perm_email,
               input$perm_level,
               input$perm_role,
               input$perm_province,
               input$perm_antenne
             )
           },
           "zone de sante" = {
             req(
               input$perm_email,
               input$perm_level,
               input$perm_role,
               input$perm_province,
               input$perm_antenne,
               input$perm_zs
             )
           })

    # Convert to uppercase, strip whitespace
    name <- stringr::str_to_title(input$perm_name)
    phone <- stringr::str_trim(input$perm_phone)
    notes <- input$perm_notes
    email <- tolower(trimws(input$perm_email))
    role <- tolower(input$perm_role)

    # Optional fields based on level
    province <- if (length(input$perm_province) > 0 & level == "province") {
      input$perm_province
    } else {
      NA_character_
    }
    antenne <- if (length(input$perm_antenne) > 0 & level == "antenne") {
      input$perm_antenne
    } else {
      NA_character_
    }
    zone_de_sante <- if (length(input$perm_zs) > 0 & level == "zone de sante") {
      input$perm_zs
    } else {
      NA_character_
    }

    # Ensure unique entries by checking if a row already exists
    new_row <- data.frame(
      name = name,
      phone = phone,
      notes = notes,
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

    showNotification("Veuillez patienter pendant que les autorisations sont d\u00e9finies...",
                     type = "default")


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

        showNotification("Autorisations d\u00e9finies",
                         type = "message")
      },
      error = function(e) {

        showNotification(
            paste("Erreur - Quelque chose s'est mal pass\u00e9:", e$message),
            type = "error")
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

  #### Surveillance table ----
  observeEvent(input$compile_campaign_btn, {
    req(input$selected_surveillance_drive_folder)

    surveillance_folder <- campaign_drive_folders() |>
      dplyr::filter(name == input$selected_surveillance_drive_folder)

    # Compile all zs templates in the folder to the national template
    showNotification("Veuillez patienter pendant que la demande est traitée")
    national_dribble_url <- compile_masques(input$selected_surveillance_drive_folder)
    output$campaign_template_url <- renderUI({tagList(a("Lien vers le masque de campagne",
                                                        href = national_dribble_url))})
    showNotification("Traitement terminé", type = "message")

    # Obtain completeness information


    national_template_dribble <- googledrive::drive_get(national_dribble_url)
    surveillance_folder_sheets <- find_drive_sheets(surveillance_folder)
    surveillance_summary(get_sheet_info(surveillance_folder_sheets))
    # Obtain campaign quality information
    campaign_quality(get_campaign_progress(national_template_dribble,
                                           5:8))
    showNotification("Informations sur la campagne traitées", type = "message")
    refresh_status(paste0("Last updated on: ",
                          as.character(Sys.time())))
    data_quality_info <- surveillance_summary()
    campaign_quality_info <- campaign_quality()

    save(data_quality_info, file = data_quality_path)
    save(campaign_quality_info, file = campaign_quality_path)

    show("refresh_date")
    show("download_data_quality_monitoring")
    show("download_campaign_quality_monitoring")
  })

  ##### Download current data quality monitoring table ----
  output$download_data_quality_monitoring <- downloadHandler(
    filename = paste0("data_quality_table_", Sys.Date(), ".csv"),
    content = function(file) {
      readr::write_csv(surveillance_summary(), file, na = "")
    }
  )

  output$download_campaign_quality_monitoring <- downloadHandler(
    filename = paste0("campaign_quality_table_", Sys.Date(), ".csv"),
    content = function(file) {
      readr::write_csv(campaign_quality(), file, na = "")
    }
  )

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
      options = list(pageLength = 10, language = "fr",
                     searchHighlight = TRUE),
      filter = "top"
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
        "Phone",
        "Notes",
        "Email",
        "Level",
        "Role",
        "Provinces",
        "Antennes",
        "Zones de sant\u00e9"
      ),
      options = list(scrollX = TRUE, pageLength = 10,
                     searchHighlight = TRUE),
      filter = "top"
    )
  })

  #### Surveillance table ----
  output$campaign_info_table <- DT::renderDT(
    DT::datatable(
      surveillance_summary(),
      options = list(scrollX = TRUE, pageLength = 10,
                     searchHighlight = TRUE),
      filter = "top",
      colnames = c(
        "Province",
        "Antenne",
        "Zone de Sante",
        "Sheet",
        "Spreadsheet Name",
        "Range",
        "Section",
        "Cellules Remplies",
        "Cellules Totales",
        "Exhaustivité",
        "Jours Depuis la Dernière Modification",
        "Date d'Exécution"
      )
    )
  )

  output$campaign_progress_table <- DT::renderDT(
    DT::datatable(
      campaign_quality(),
      options = list(scrollX = TRUE, pageLength = 10,
                     searchHighlight = TRUE),
      filter = "top",
      colnames = c(
        "Province",
        "Antenne",
        "Zone de Sante",
        "Aire de Sante",
        "Jour",
        "Rapport Completude (%)",
        "Couverture (Cumulative)",
        "Nb moyen d'enfants vaccinés/équipe (Rural)",
        "Nb moyen d'enfants vaccinés/équipe (Urban)",
        "Récupérations (0-11 mois, cumulative)",
        "Récupérations (12-23 mois, cumulative)",
        "Récupérations (24-59 mois, cumulative)"
      )
    )
  )

  #### Plots ----
  output$campaign_completeness_plot <- renderPlot(
    {

      validate(
        need(!is.null(campaign_quality()), "No campaign quality data."),
        need(nrow(campaign_quality()) > 0, "No campaign quality data.")
      )

      create_campaign_progress_heatmap(campaign_quality() |>
                                         dplyr::filter(province == input$prov_selector_campaign_completeness,
                                                       zone_de_sante == input$zs_selector_campaign_completeness))

    }
  )
  output$campaign_urban_rural_plot <- renderPlot(
    {
      validate(
        need(!is.null(campaign_quality()), "No campaign quality data."),
        need(nrow(campaign_quality()) > 0, "No campaign quality data.")
      )

      create_urban_rural_heatmap(campaign_quality() |>
                                         dplyr::filter(province == input$prov_selector_campaign_completeness,
                                                       zone_de_sante == input$zs_selector_campaign_completeness))

    }
  )

  output$campaign_recovery_plot <- renderPlot(
    {
      validate(
        need(!is.null(campaign_quality()), "No campaign quality data."),
        need(nrow(campaign_quality()) > 0, "No campaign quality data.")
      )
      create_recovery_heatmap(campaign_quality() |>
                                   dplyr::filter(province == input$prov_selector_campaign_completeness,
                                                 zone_de_sante == input$zs_selector_campaign_completeness))

    },

  )
}
