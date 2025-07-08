get_sheet_info <- function(dribble, sheet = NULL) {

  # Get only the max row based on aires de sante column
  ss_max_row <- googlesheets4::range_read(dribble, 1, "D3:D48") |>
    dplyr::filter(!is.na(`Aires de santé`)) |>
    nrow()

  # Obtain the section names of a sheet
  names <- get_names_from_sheet(sheet)

  # Get missingness of each section passed unto names
  dribble_info <- lapply(1:nrow(dribble),
                         \(x) get_section_info(dribble[x, ],
                                               sheet,
                                               names,
                                               ss_max_row))
  dribble_info <- dplyr::bind_rows(dribble_info) |>
    mutate(section = as.factor(section))

  return(dribble_info)

}


#' Get information on the completeness of a section
#'
#' @param dribble `dribble` A dribble object identifying a specific Google Sheet.
#' @param sheet `str` or `int` Specify sheet ("tab") either by name or position.
#' @param name `str` Name of the section.
#' @param ss_max_row `int` How many rows to read.
#'
#' @returns `dribble` A dribble with section information on completeness.
#' @export
#'
#' @examples
#' \dontrun{
#' ss <- googlesheets4::drive_get("example_sheet")
#' ss_summary <- get_section_info(ss)
#' }
get_section_info <- function(dribble, sheet= NULL, name = NULL, ss_max_row = NULL) {

  # Check that what is passed is a single dribble
  if (nrow(dribble) > 1) {
    cli::cli_abort("Please pass only a one-row dribble.")
  }

  # Obtain the modified time from a dribble object
  dribble_info <- googledrive::drive_reveal(dribble, what = "modifiedTime")

  # Read the spreadsheet
  ss_range <- googlesheets4::range_read(dribble_info, sheet, skip = 2, n_max = ss_max_row)

  sections <- lapply(1:length(name), \(x) {
    range <- get_range_from_name(sheet, name[x])
    range <- str_split(range, ":") |> unlist()
    range <- a1_to_colnum(range[1]):a1_to_colnum(range[2])
    section_info <- ss_range[, range]

    # Obtain % emptiness of the range
    empty_info <- sum(is.na(section_info))
    val_counts <- length(section_info) * nrow(section_info)
    prop_empty <- scales::percent(empty_info / val_counts)

    # Create a tibble
    dplyr::tibble(
      range = get_range_from_name(sheet, name[x]),
      section = name[x],
      empty_cells = empty_info,
      total_cells = val_counts,
      missingness = prop_empty
    )
  })
  sections <- bind_rows(sections)

  # Join back to the dribble
  dribble_summary <- dplyr::tibble(prov = sub(".*PROV_([^_]+).*", "\\1", dribble_info$name),
                                   antenne = sub(".*AN_([^_]+).*", "\\1", dribble_info$name),
                                   zone_de_sante = sub(".*ZS_([^_]+)$", "\\1", dribble_info$name),
                                   sheet = sheet) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(c("prov", "antenne", "zone_de_sante")),
                                \(x) dplyr::if_else(stringr::str_detect(x, "CAMPAGNE"), NA, x)))
  dribble_summary <- dplyr::cross_join(dribble_summary, sections) |>
    dplyr::mutate(days_since_last_modified = as.numeric(Sys.Date() - as.Date(dribble_info$modified_time)))

  return(dribble_summary)
}

# Private functions
get_range_from_name <- function(sheet, name) {
  if (sheet == 1) {
    switch(name,
           "Situation des équipes de vaccination par aires de santé" = "E:G",
           "Populations cibles vaccinées à la campagne précédente" = "N:O",
           "Disponilité du vaccin, antigène utilisé" = "V:V",
           "Nombre d'équipes de mobilisation sociale de proxi" = "Y:Y",
           "RESEAU/ INTERNET" = "Z:AB",
           "Enfants 0 - 11 mois identifiés pour etre récupérés en PEV de routine" = "AC:AQ",
           "Enfants 12 - 23 mois identifiés pour etre récupérés en PEV de routine" = "AS:BG",
           "Enfants 24 - 59 mois identifiés pour etre récupérés en PEV de routine" = "BI:BW",
           "Femmes cibles identifiées pour etre récupérées en PEV de routine" = "BY:CC",
           "LOCALISATION DES POPULATIONS SPECIALES" = "CE:CG",
           "Nombre de ménages dénombrés" = "CH:CH",
           "Populations spéciales (Nombre de personnes selon le Statut)" = "CI:CK",
           "Effectifs des Populations spéciales recensées" = "CL:CU")
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
           "Vaccines 12-59 mois 1 dose et plus" = "CY:CZ",
           "Vaccines 0-11 mois Zéro Dose" = "DD:DE",
           "Vaccines 0-11 mois 1 dose et plus" = "DF:DG",
           "Vaccines 12-59 mois Zéro Dose" = "DK:DL",
           "Vaccines 12-59 mois 1 dose et plus" = "DM:DN",
           "Vaccinés par les Equipes en milieu rural" = "EA:EB",
           "Vaccinés par les Equipes en milieu urbain" = "EC:ED",
           "GESTION DES FLACONS" = "EQ:EW",
           "Recherche active des cas de MEV" = "EX:FA",
           "MAPI" = "FB:FC",
           "Ver de Guinée" = "FD:FD"
           # NOT DONE
    )

  }
}

get_names_from_sheet <- function(sheet = NULL) {
  if (sheet == 1) {
    c(
      "Situation des équipes de vaccination par aires de santé",
      "Populations cibles vaccinées à la campagne précédente",
      "Disponilité du vaccin, antigène utilisé" ,
      "Nombre d'équipes de mobilisation sociale de proxi",
      "RESEAU/ INTERNET" ,
      "Enfants 0 - 11 mois identifiés pour etre récupérés en PEV de routine",
      "Enfants 12 - 23 mois identifiés pour etre récupérés en PEV de routine",
      "Enfants 24 - 59 mois identifiés pour etre récupérés en PEV de routine",
      "Femmes cibles identifiées pour etre récupérées en PEV de routine",
      "LOCALISATION DES POPULATIONS SPECIALES",
      "Nombre de ménages dénombrés",
      "Populations spéciales (Nombre de personnes selon le Statut)",
      "Effectifs des Populations spéciales recensées"
    )

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

    c(
      "Nombre de ménage",
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
      "Rattrapage/récupération en PEV",
      "Complétude des rapports de vaccination",
      "Vaccines 0-11 mois Zéro Dose",
      "Vaccines 0-11 mois 1 dose et plus",
      "Vaccines 12-59 mois Zéro Dose",
      "Vaccines 12-59 mois 1 dose et plus",
      "Vaccines 0-11 mois Zéro Dose",
      "Vaccines 0-11 mois 1 dose et plus",
      "Vaccines 12-59 mois Zéro Dose",
      "Vaccines 12-59 mois 1 dose et plus",
      "Vaccinés par les Equipes en milieu rural",
      "Vaccinés par les Equipes en milieu urbain",
      "GESTION DES FLACONS",
      "Recherche active des cas de MEV",
      "MAPI",
      "Ver de Guinée"
    )

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
