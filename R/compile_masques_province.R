compile_masques_province <- function(campaign_name) {

  # Get dribbles of provinces
  folders <- googledrive::drive_ls(campaign_name)
  folders <- folders |>
    googledrive::drive_reveal("mimeType") |>
    dplyr::filter(mime_type == "application/vnd.google-apps.folder")

  for (i in folders) {
    # List masques dribbles for a specific province
    templates <- gather_data_templates_from_folder(i) # make sure to uncomment the progress bars in this function after testing

    # Create province level dribble
    province_dribble <- create_masque_database(i, templates[1, ], level = "province")
    tab_names <- googlesheets4::sheet_names(province_dribble)

    # Set permissions
    showNotification(paste0("Obtention des autorisations requises pour le masque: ", i$name))
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
