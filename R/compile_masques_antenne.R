#' Compile masque at the antenne level
#'
#' @param campaign_name `str` Name of the campaign.
#'
#' @returns NULL, invisibly.
#' @keywords internal
compile_masques_antenne <- function(campaign_name) {

  googledrive::drive_auth(TRUE)
  googlesheets4::gs4_auth(TRUE)

  # Get dribbles of provinces
  folders_prov <- googledrive::drive_ls(campaign_name)
  folders_prov <- folders_prov |>
    googledrive::drive_reveal("mimeType") |>
    dplyr::filter(mime_type == "application/vnd.google-apps.folder")

  # Get dribbles of antennes
  folders <- purrr::map(1:nrow(folders_prov), \(x) {
    googledrive::drive_ls(folders_prov[x, ]) |>
      googledrive::drive_reveal("mimeType") |>
      dplyr::filter(mime_type == "application/vnd.google-apps.folder")
  })
  folders <- dplyr::bind_rows(folders)

  for (i in 1:nrow(folders)) {
   antenne_dribble <- googledrive::drive_ls(folders[i, ]) |>
      googledrive::drive_reveal("mimeType") |>
      dplyr::filter(mime_type == "application/vnd.google-apps.spreadsheet")

    if (nrow(antenne_dribble) == 0) {
      cli::cli_alert_warning("Masque needs to be recompiled")
    } else {
      antenne_url <- complete_compiled_masque(antenne_dribble)
      if (!is.na(antenne_url)) {
        showNotification(paste0(folders[i, ]$name, " masque refreshed"), type = "message")
        next
      } else {
        cli::cli_alert_warning(paste0(folders[i, ]$name, " masque needs to be recompiled"))
      }
    }

   # MODIFY here for the antenne dribbles
    # List masques dribbles for a specific province
    templates <- gather_data_templates_from_folder(folders[i, ], level = "province")

    # Create antenne level dribble
    antenne_dribble <- create_masque_database(campaign_name, folders[i, ], templates[1, ], level = "antenne")
    tab_names <- googlesheets4::sheet_names(antenne_dribble)

    # Set permissions
    showNotification(paste0("Obtention des autorisations requises pour le masque: ", folders[i, ]$name))
    grant_read_permission_from_masques(antenne_dribble, templates)
    showNotification("Autorisations requises obtenues.", type = "message")

    # Compile masques
    # Compile all templates for one sheet at a time
    withProgress(message = "Compilation des données dans: ",
                 {
                   n <- length(tab_names)
                   for (j in 1:n) {
                     incProgress(1/n, detail = tab_names[j])

                     copy_sheet_info_to_summary_masque(antenne_dribble,
                                                       templates,
                                                       tab_names[j])
                   }
                 })
  }
  showNotification("Masques au niveau des antennes compilés", type = "message")

  return(invisible())
}
