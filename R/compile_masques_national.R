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

  googledrive::drive_auth(TRUE)
  googlesheets4::gs4_auth(TRUE)

  query <- paste0("mimeType = 'application/vnd.google-apps.spreadsheet' and name contains '",
                  paste0(campaign_name, "_national_rdc"), "'")
  national_masque <- googledrive::drive_find(q = query)

  if (nrow(national_masque) == 0) {
    cli::cli_alert_warning("Masque needs to be recompiled")
  } else {
    # First check if the masque has been created correctly, because if so
    # can skip the compilation step on the next run since it will automatically
    # update
    national_url <- complete_compiled_masque(national_masque)
    if (!is.na(national_url)) {
      showNotification("National masque refreshed", type = "message")
      return(national_url)
    } else {
      cli::cli_alert_warning("Masque needs to be recompiled")
    }
  }

  # Gather the right files
  query <- paste0("mimeType = 'application/vnd.google-apps.folder' and name contains '", campaign_name, "'")
  folders <- googledrive::drive_find(q = query)
  templates <- gather_data_templates_from_folder(folders)

  # Create a new template
  national_dribble <- create_masque_database(campaign_name, folders, templates[1, ], level = "national")
  # Get information
  tab_names <- googlesheets4::sheet_names(national_dribble)

  # Grant permission for templates
  showNotification("Obtention des autorisations requises...")
  grant_read_permission_from_masques(national_dribble, templates)
  showNotification("Autorisations requises obtenues.", type = "message")

  # Compile all templates for one sheet at a time
  withProgress(message = "Compilation des données dans: ",
               {
                 n <- length(tab_names)
                 for (i in 1:n) {
                   incProgress(1/n, detail = tab_names[i])

                   copy_sheet_info_to_summary_masque(national_dribble,
                                                     templates,
                                                     tab_names[i])
                 }
               })

  national_dribble_url <- googledrive::drive_reveal(national_dribble, "webViewLink") |>
    dplyr::pull(web_view_link)
  return(national_dribble_url)

}

# Private function

#' Convert column number to column name.
#'
#' @param n `int` Positive integer corresponding to column location. (1 = "A").
#'
#' @returns `str` The column name in A1 format.
#' @keywords internal
num_to_col <- function(n) {
  s <- ""
  while(n > 0) {
    n <- n - 1
    s <- paste0(LETTERS[n %% 26 + 1], s)
    n <- n %/% 26
  }

  return(s)
}

#' Gather masques from a folder
#'
#' @description
#' Gathers the data templates (masques) from a drive folder.
#'
#' @param folder `dribble` Dribble of the folder.
#' @param level `str` "national" or "province". National will look for province
#' level masques while province will look for zone de sante level masques.
#'
#' @returns `dribble` A dribble with each row corresponding to a data masque.
#' @keywords internal
gather_data_templates_from_folder <- function(folder, level = "national") {

  if (!level %in% c("national", "province", "antenne")) {
    cli::cli_abort("Invalid level. Please use either national or province.")
  }

  dribble_type <- switch(level,
                         # national will only use the compiled province tibbles
                         "national" = "_province_",
                         "province" = "ZS")

  templates <- googledrive::drive_ls(folder, recursive = TRUE) |>
    googledrive::drive_reveal("mimeType") |>
    googledrive::drive_reveal("webViewLink") |>
    dplyr::filter(stringr::str_detect(mime_type, ".spreadsheet"),
                  stringr::str_detect(name, dribble_type)) |>
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

      if (level == "province") {
        ss_max_row <- ss_max_row[-nrow(ss_max_row), ]
      }
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

  return(templates)
}

#' Create a summary masque
#'
#' @description
#' Creates a summary masque. At the national level, the function will compile from
#' the province level masques.
#'
#' @param campaign_name `str` Campaign name
#' @param folder `dribble` Where the masque should be created
#' @param template_dribble `dribble` An example of masque to use as a template.
#' This is simply either a province or zone de sante masque (determined by level) so
#' that the function knows which columns to copy.
#' @param level `str` "national" or "province". What level to compile at.
#'
#' @returns `dribble` Dribble to the newly created summary masque.
#' @keywords internal
create_masque_database <- function(campaign_name, folder, template_dribble, level) {
  if (!level %in% c("national", "province", "antenne")) {
    cli::cli_abort("Invalid level. Accepted values are national, province, and antenne")
  }

  file_suffix <- switch(level,
                        "national" = "_national_rdc",
                        "province" = paste0("_province_", folder$name),
                        "antenne" = paste0("_antenne_", folder$name))

  # Create a new template
  summary_dribble <- googledrive::drive_cp(template_dribble, folder,
                                            name = paste0(campaign_name, file_suffix),
                                            overwrite = TRUE)

  # Clear everything except for the header columns
  ss_info <- googlesheets4::gs4_get(summary_dribble)
  tab_names <- ss_info$sheets$name

  if ("Graphiques" %in% tab_names) {
    # Delete graphiques sheet because not necessary and incompatible
    googlesheets4::sheet_delete(summary_dribble, "Graphiques")
    tab_names <- tab_names[tab_names != "Graphiques"]
  }

  purrr::map(tab_names,
             \(x) {
               googlesheets4::range_clear(summary_dribble,
                                          x,
                                          range = googlesheets4::cell_rows(c(4, NA)))
             })

  return(summary_dribble)
}

grant_read_permission_from_masques <- function(target_masque, source_masques,
                                               token = googlesheets4::gs4_token()) {
  # Set spreadsheet IDs
  ss_id <- target_masque$id[1]

  for (i in source_masques$id) {
    # Construct the URL
    url <- sprintf("https://docs.google.com/spreadsheets/d/%s/externaldata/addimportrangepermissions?donorDocId=%s", ss_id, i)

    # Make the POST request
    response <- httr::POST(
      url,
      httr::add_headers(Authorization = paste("Bearer",
                                              token$auth_token$credentials$access_token))
    )
  }

}

#' Copy data from data masques into the summary masque
#'
#' @description
#' The function compiles the structure of how the Google Sheet
#' function IMPORTFROM() and fills in the appropriate arguments of the function.
#' This is then appended to the summary masque.
#'
#' @param summary_masque `dribble` The masque where the data should be copied to.
#' @param templates `dribble` A dribble of dribbles to copy.
#' @param sheet_name `str` Name of the sheet to copy from the templates
#'
#' @details
#' googlesheets4 behaves differently based on the locality. In a French locale,
#' functions are delimited by a semi-colon but in an English locale, they are delimited
#' by a comma. As of now, the function strictly deals with either English or French with
#' no guarantees for other languages.
#'
#'
#' @returns NULL
#' @keywords internal
copy_sheet_info_to_summary_masque <- function(summary_masque, templates, sheet_name) {
  ss_info <- googlesheets4::gs4_get(summary_masque)

  delimiter <- ifelse(stringr::str_detect(Sys.getlocale(), "English"), '", "', '"; "')

  templates <- templates |>
    dplyr::mutate(ss_function = paste0("=IMPORTRANGE(",
                                       '"',
                                       web_view_link,
                                       delimiter,
                                       sheet_name,
                                       "!",
                                       "A4:",
                                       num_to_col(ss_info$sheets |>
                                                    dplyr:: filter(name == sheet_name) |>
                                                    dplyr::pull(grid_columns)),
                                       max_rows + 3,
                                       '")'))

  # Set up structure
  sheet_functions <- dplyr::tibble(ss_function = character())
  for (j in 1:nrow(templates)) {
    sheet_functions <- sheet_functions |>
      dplyr::add_row(ss_function = templates[j, ]$ss_function)
    sheet_functions <- dplyr::bind_rows(sheet_functions,
                                        dplyr::tibble(ss_function = rep(NA, templates[j, ]$max_rows - 1)))
  }

  # Declare column as a Google Sheets formula column
  sheet_functions$ss_function <- googlesheets4::gs4_formula(sheet_functions$ss_function)

  # Append to the national template
  googlesheets4::sheet_append(summary_masque,
                              sheet_functions,
                              sheet = sheet_name)
}

#' Check if the masque was compiled completely
#'
#' @description
#' The function basically checks if the number of rows in the first sheet is
#' equal to the last sheet. This is a basic check to see if the summary masque
#' was compiled successfully without any interruptions.
#'
#'
#' @param dribble `dribble` A dribble of a summary masque, ideally.
#'
#' @returns `str` The URL of the summary masque or NA, if the masque compilation was unsuccessful.
#' @keywords internal
complete_compiled_masque <- function(dribble) {

  # Check number of rows are the same for first and last tabs
  tabs <- googlesheets4::sheet_names(dribble)
  first_tab <- googlesheets4::read_sheet(dribble,
                                         sheet = tabs[1],
                                         range = "A3:A",
                                         col_names = TRUE) |>
    # dplyr method of accessing the first col index
    dplyr::filter(dplyr::if_any(1, \(x) !is.na(x))) |>
    nrow()
  last_tab <- googlesheets4::read_sheet(dribble,
                                        sheet = tabs[length(tabs)],
                                        range = "A3:A",
                                        col_names = TRUE) |>
    # dplyr method of accessing the first col index
    dplyr::filter(dplyr::if_any(1, \(x) !is.na(x))) |>
    nrow()

  if (first_tab == last_tab & (first_tab > 0)) {
    cli::cli_alert_info("Masque already compiled successfully")
    national_dribble_url <- googledrive::drive_reveal(dribble, "webViewLink") |>
      dplyr::pull(web_view_link)
    return(national_dribble_url)
  } else {
    return(NA)
  }

}
