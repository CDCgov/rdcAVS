#' Compiles the zone de sante masques
#'
#' Compared to the original version of the function, this masque uses the
#' IMPORTRANGE() function in Google Sheets to copy over the information. Rather
#' than having multiple imports and exports.
#'
#' @param campaign_name `str` Name of the campaign.
#'
#' @returns `str` URL to the national compiled masque.
#' @export
#'
#' @examples
#' \dontrun{
#' compile_masques_v2("CAMPAGNE_new_campaign") # must be a valid campaign folder
#' }
compile_masques_national <- function(campaign_name) {

  # Gather the right files
  query <- paste0("mimeType = 'application/vnd.google-apps.folder' and name contains '", campaign_name, "'")
  folders <- googledrive::drive_find(q = query)
  templates <- googledrive::drive_ls(folders, recursive = TRUE) |>
    googledrive::drive_reveal("mimeType") |>
    googledrive::drive_reveal("webViewLink") |>
    dplyr::filter(stringr::str_detect(mime_type, ".spreadsheet"),
                  stringr::str_detect(name, "ZS")) |>
    dplyr::arrange(name)

  # Calculate the max row for each template
  template_rows <- dplyr::tibble(name = character(),
                                 max_rows = integer())

  withProgress(message = "Analyzing ", {
    m <- nrow(templates)

    for (i in 1:nrow(templates)) {
      incProgress(1/m, detail = templates[i, ]$name)
      # Calculate max rows for each masque
      ss_max_row <- googlesheets4::range_read(templates[i, ], 1,
                                              range = googlesheets4::cell_cols("D"),
                                              col_names = FALSE) |>
        dplyr::filter(!is.na(`...1`),
                      `...1` != "NULL")
      ss_max_row <- ss_max_row[-nrow(ss_max_row), ]
      # Get rid of totals
      ss_max_row <- ss_max_row[4:nrow(ss_max_row), ]
      # Get rid of counts, if they exist
      ss_max_row <- nrow(ss_max_row)
      template_rows <- template_rows |>
        dplyr::add_row(name = templates[i, ]$name,
                       max_rows = ss_max_row)
    }

  })

  # Add column to templates
  templates <- templates |>
    dplyr::left_join(template_rows)

  # Create a new template
  national_dribble <- googledrive::drive_cp(templates[1, ], folders,
                                            name = paste0(campaign_name, "_national_rdc"),
                                            overwrite = TRUE)

  # Delete graphiques sheet because not necessary and incompatible
  googlesheets4::sheet_delete(national_dribble, "Graphiques")

  # Clear everything except for the header columns
  ss_info <- googlesheets4::gs4_get(national_dribble)
  tab_names <- ss_info$sheets$name
  purrr::map(tab_names,
             \(x) {
               googlesheets4::range_clear(national_dribble,
                                          x,
                                          range = googlesheets4::cell_rows(c(4, NA)))
             })

  # Grant permission for templates
  # Obtain token
  token <- googlesheets4::gs4_token()

  # Set spreadsheet IDs
  ssId <- national_dribble$id[1]

  showNotification("Obtaining required permissions", type = "message")
  for (i in templates$id) {
    # Construct the URL
    url <- sprintf("https://docs.google.com/spreadsheets/d/%s/externaldata/addimportrangepermissions?donorDocId=%s", ssId, i)

    # Make the POST request
    response <- httr::POST(
      url,
      httr::add_headers(Authorization = paste("Bearer",
                                              token$auth_token$credentials$access_token))
    )
    print(response)
  }

  # Compile all templates for one sheet at a time
  withProgress(message = "Compilation des données dans: ",
               {
                 n <- length(tab_names)
                 for (i in 1:n) {
                   incProgress(1/n, detail = tab_names[i])

                   templates <- templates |>
                     dplyr::mutate(ss_function = paste0("=IMPORTRANGE(",
                                                        '"',
                                                        web_view_link,
                                                        '", "',
                                                        tab_names[i],
                                                        "!",
                                                        "A4:",
                                                        num_to_col(ss_info$sheets[i, ]$grid_columns),
                                                        max_rows + 3,
                                                        '")'))

                   # Set up structure
                   sheet_functions <- dplyr::tibble(ss_function = character())
                   for (j in 1:nrow(templates)) {
                     sheet_functions <- sheet_functions |> dplyr::add_row(ss_function = templates[j, ]$ss_function)
                     sheet_functions <- dplyr::bind_rows(sheet_functions,
                                                         dplyr::tibble(ss_function = rep(NA, templates[j, ]$max_rows - 1)))
                   }

                   # Declare column as a Google Sheets formula column
                   sheet_functions$ss_function <- googlesheets4::gs4_formula(sheet_functions$ss_function)

                   # Append to the national template
                   googlesheets4::sheet_append(national_dribble,
                                               sheet_functions,
                                               sheet = tab_names[i])
                 }
               })

  national_dribble_url <- googledrive::drive_reveal(national_dribble, "webViewLink") |>
    dplyr::pull(web_view_link)
  return(national_dribble_url)

}

# Private function
num_to_col <- function(n) {
  s <- ""
  while(n > 0) {
    n <- n - 1
    s <- paste0(LETTERS[n %% 26 + 1], s)
    n <- n %/% 26
  }

  return(s)
}
