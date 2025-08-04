compile_masques <- function(campaign_name) {

  # Helper function
  a1_to_colnum <- function(col) {
    col <- toupper(col)
    chars <- strsplit(col, "")[[1]]
    n <- length(chars)
    col_num <- 0
    for (i in 1:n) {
      col_num <- col_num * 26 + (utf8ToInt(chars[i]) - utf8ToInt("A") + 1)
    }
    return(col_num)
  }

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

  # Clear everything except for the header columns
  tab_names <- googlesheets4::sheet_names(national_dribble)
  tab_names <- tab_names[tab_names != "Graphiques"]
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
                           sheet_data <- dplyr::bind_rows(sheet_data, copy_template_tab(templates[j, ], sheet = tab_names[i]))
                         },
                         error = \(e) {
                           showNotification(paste0("Erreur de copie: "), type = "error")
                         }
                       )

                     }

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

    return(data)

}
