#' Compiles the zone de sante masques
#'
#' @param campaign_name `str` Name of the campaign.
#'
#' @returns `str` URL to the national compiled masque.
#' @export
#'
#' @examples
#' \dontrun{
#' compile_masques("CAMPAGNE_new_campaign") # must be a valid campaign folder
#' }
compile_masques <- function(campaign_name) {

  # Gather the right files
  query <- paste0("mimeType = 'application/vnd.google-apps.folder' and name contains '", campaign_name, "'")
  folders <- googledrive::drive_find(q = query)
  templates <- googledrive::drive_ls(folders, recursive = TRUE) |>
    googledrive::drive_reveal("mimeType") |>
    dplyr::filter(stringr::str_detect(mime_type, ".spreadsheet"),
                  stringr::str_detect(name, "ZS")) |>
    dplyr::arrange(name)

  # Create a new template
  national_dribble <- googledrive::drive_cp(templates[1, ], folders,
                        name = paste0(campaign_name, "_national_rdc"),
                        overwrite = TRUE)

  # Delete graphiques sheet because not necessary and incompatible
  googlesheets4::sheet_delete(national_dribble, "Graphiques")

  # Clear everything except for the header columns
  tab_names <- googlesheets4::sheet_names(national_dribble)
  purrr::map(tab_names,
             \(x) {
               googlesheets4::range_clear(national_dribble,
                                          x,
                                          range = googlesheets4::cell_rows(c(4, NA)))
             })

  # Compile all templates for one sheet at a time
  withProgress(message = "Compilation des données dans: ",
               {
                 n <- length(tab_names)
                 for (i in 1:n) {
                   incProgress(1/n, detail = tab_names[i])
                   withProgress(message = "Copie du ", {
                     m <- nrow(templates)

                     sheet_data <- NULL
                     for (j in 1:m) {
                       incProgress(1/m, detail = templates[j, ]$name)
                       tryCatch(
                         {
                           zs_data <- copy_template_tab(templates[j, ], sheet = tab_names[i])
                         },
                         error = \(e) {
                           showNotification(paste0("Erreur de copie: ", templates[j, ]$name), type = "error")
                         }
                       )

                       tryCatch({
                         sheet_data <- dplyr::bind_rows(sheet_data, zs_data)
                       },
                       error = \(e) {
                         showNotification(paste0("Invalid structure: ", templates[j, ]$name), type = "error")
                       })

                     }

                     sheet_data <- sheet_data |> readr::type_convert()

                     # Append to the national template
                     googlesheets4::sheet_append(national_dribble, sheet_data, sheet = tab_names[i])

                   })
                 }
               })

  national_dribble_url <- googledrive::drive_reveal(national_dribble, "webViewLink") |>
    dplyr::pull(web_view_link)
  return(national_dribble_url)

}

# Private functions
copy_template_tab <- function(zs_dribble, sheet) {

    # Read in data (no column names)
    data <- googlesheets4::range_read(zs_dribble, sheet,
                                      range = googlesheets4::cell_rows(c(3, NA)),
                                      col_names = FALSE)

    #Drop the first row because it is also a heading
    data <- data[2:nrow(data), ]
    # Drop the last row because it is a total
    data <- data[-nrow(data), ]

    # Filter to only include AZ with values
    data <- data[data[4] != "NULL", ]

    # Filter to only include columns AZ without a number (signifies a row total)
    data <- data[!grepl(data[4], "\\d")]

    # Convert NULL values to NAs first because NULLs can't be represented in a vector
    # Make sure to unlist list columns (initial mixed data types likely the reason)
    data <- data |>
      dplyr::mutate(dplyr::across(dplyr::everything(),
                                  \(x) lapply(x, function(y) if (is.null(y)) NA else y))) |>
      dplyr::mutate(dplyr::across(dplyr::everything(),
                                  \(x) unlist(x))) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

    return(data)

}
