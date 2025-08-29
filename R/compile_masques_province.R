compile_masques_province <- function(campaign_name) {

  # Get dribbles of provinces
  folders <- googledrive::drive_ls(campaign_name)
  folders <- folders |>
    googledrive::drive_reveal("mimeType") |>
    dplyr::filter(mime_type == "application/vnd.google-apps.folder")

  for (i in 1:nrow(folders)) {
    prov_dribble <- googledrive::drive_ls(folders[i, ]) |>
      googledrive::drive_reveal("mimeType") |>
      dplyr::filter(mime_type == "application/vnd.google-apps.spreadsheet")

    if (nrow(prov_dribble) == 0) {
      cli::cli_alert_warning("Masque needs to be recompiled")
    } else {
      prov_url <- complete_compiled_masque(province_dribble)
      if (!is.na(prov_url)) {
        showNotification(paste0(folders[i, ]$name, " masque refreshed"), type = "message")
        next
      } else {
        cli::cli_alert_warning(paste0(folders[i, ]$name, " masque needs to be recompiled"))
      }
    }

    # List masques dribbles for a specific province
    templates <- gather_data_templates_from_folder(folders[i, ], level = "province")

    # Create province level dribble
    province_dribble <- create_masque_database(folders[i, ], templates[1, ], level = "province")
    tab_names <- googlesheets4::sheet_names(province_dribble)

    # Set permissions
    showNotification(paste0("Obtention des autorisations requises pour le masque: ", folders[i, ]$name))
    grant_read_permission_from_masques(province_dribble, templates)
    showNotification("Autorisations requises obtenues.", type = "message")

    # Compile masques
    # Compile all templates for one sheet at a time
    withProgress(message = "Compilation des données dans: ",
                 {
                   n <- length(tab_names)
                   for (j in 1:n) {
                     incProgress(1/n, detail = tab_names[j])

                     copy_sheet_info_to_summary_masque(province_dribble,
                                                       templates,
                                                       tab_names[j])
                   }
                 })
  }
  showNotification("Masques au niveau des provinces compilés", type = "message")
}
