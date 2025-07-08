get_sheet_info <- function(dribble, sheet = NULL) {

  names <- get_names_from_sheet(sheet)

  dribble_info <- lapply(1:nrow(dribble),
                         \(x) get_section_info(dribble[x, ],
                                               sheet,
                                               names))


}


#' Get information on the completeness of a section
#'
#' @param dribble `dribble` A dribble object identifying a specific Google Sheet.
#' @param sheet `str` or `int` Specify sheet ("tab") either by name or position.
#' @param name `str` Name of the section.
#'
#' @returns `dribble` A dribble with section information on completeness.
#' @export
#'
#' @examples
#' \dontrun{
#' ss <- googlesheets4::drive_get("example_sheet")
#' ss_summary <- get_section_info(ss)
#' }
get_section_info <- function(dribble, sheet= NULL, name = NULL) {

  # Check that what is passed is a single dribble
  if (nrow(dribble) > 1) {
    cli::cli_abort("Please pass only a one-row dribble.")
  }

  # Obtain the modified time from a dribble object
  dribble_info <- googledrive::drive_reveal(dribble, what = "modifiedTime")

  # Get only the max row based on aires de sante column
  ss_max_row <- googlesheets4::range_read(dribble_info, 1, "D3:D48") |>
    dplyr::filter(!is.na(`Aires de santé`)) |>
    nrow()

  range <- get_range_from_name(sheet, name)
  range <- str_split(range, ":")
  range <- lapply(range, a1_to_colnum) |> unlist()

  # Read the spreadsheet
  ss_range <- googlesheets4::range_read(dribble_info, sheet, skip = 2, n_max = ss_max_row)
  ss_range <- ss_range |>
    dplyr::select(range)


  # Obtain % emptiness of the range
  empty_info <- sum(is.na(ss_range))
  val_counts <- length(ss_range) * nrow(ss_range)
  prop_empty <- scales::percent(empty_info / val_counts)

  # Join back to the dribble
  dribble_summary <- dplyr::tibble(#name = dribble_info$name,
                                   prov = sub(".*PROV_([^_]+)_AN_.*", "\\1", dribble_info$name),
                                   antenne = sub(".*AN_([^_]+)_ZS_.*", "\\1", dribble_info$name),
                                   zone_de_sante = sub(".*ZS_([^_]+)$", "\\1", dribble_info$name),
                                   #sheet = sheet,
                                   #range = range,
                                   #empty_cells = empty_info,
                                   #total_cells = val_counts,
                                   section = name,
                                   missing = prop_empty,
                                   #modified_time = dribble_info$modified_time,
                                   days_since_last_modified = as.numeric(Sys.Date() - as.Date(dribble_info$modified_time)))

  return(dribble_summary)
}

# Private functions
get_range_from_name <- function(sheet, name) {
  if (sheet == 1) {

  } else if (sheet %in% 2:4) {
    switch(name,
           "Nombre de ménage" = "G:G",
           "Complétude MoSo" = "I:I",
           "Nombre de personnes de plus de 15 ans sensibilisées" = "K:L",
           "0-11 mois dénombrés" = "N:O",
           "12-59 mois dénombrés" = "Q:R",
           "5-14 ans dénombrés" = "W:X",
           "Cas de maladies sous surveillance" = "Y:AC",
           "REFUS" = "AD:AE",
           "Mobilisation communautaire des FEMMES" = "AF:AN",
           "Mobilisation communautaire des HOMMES" = "AO:AW",
           "Mobilisation communautaire générale" = "AX:BI",
           "Rattrapage/récupération en PEV" = "BJ:BK"
    )
  } else if (sheet %in% 5:8) {
    switch(name,
           "Nombre de ménage" = "G:G",
           "Complétude MoSo" = "I:I",
           "Nombre de personnes de plus de 15 ans sensibilisées" = "K:L",
           "0-11 mois dénombrés" = "N:O",
           "12-59 mois dénombrés" = "Q:R",
           "5-14 ans dénombrés" = "W:X",
           "Cas de maladies sous surveillance" = "Y:AC",
           "REFUS" = "AD:AE",
           "Mobilisation communautaire des FEMMES" = "AF:AN",
           "Mobilisation communautaire des HOMMES" = "AO:AW",
           "Mobilisation communautaire générale" = "AX:BI",
           "Rattrapage/récupération en PEV" = "BJ:BK",
           "Complétude des rapports de vaccination" = "CL:CN",
           "Vaccines 0-11 mois Zéro Dose" = "CP:CQ",
           "Vaccines 0-11 mois 1 dose et plus" = "CR:CS",
           "Vaccines 12-59 mois Zéro Dose" = "CW:CX",
           "Vaccines 12-59 mois 1 dose et plus" = "CY:CZ"
           #not complete!
    )

  }
}

get_names_from_sheet <- function(sheet = NULL) {
  if (sheet == 1) {

  } else if (sheet %in% 2:4) {

    c("Nombre de ménage",
    "Complétude MoSo",
    "Nombre de personnes de plus de 15 ans sensibilisées",
    "0-11 mois dénombrés",
    "12-59 mois dénombrés",
    "5-14 ans dénombrés",
    "Cas de maladies sous surveillance",
    "REFUS",
    "Mobilisation communautaire des FEMMES",
    "Mobilisation communautaire des HOMMES",
    "Mobilisation communautaire générale",
    "Rattrapage/récupération en PEV")

  } else if (sheet %in% 5:8) {

  }

}

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
