#' Obtain the information from a dribble tab or tabs
#'
#' @description
#' Collect information related to missingness of section for a particular tab.
#'
#' @param dribble `dribble` Can be single or multi-row dribble containing
#' only dribbles of spreadsheets.
#' @param sheets `int` The tab to look into. Defaults to reading the first through
#' the eighth tab.
#'
#' @returns `tibble` A tibble containing all the information in a sheet.
#' @export
#'
#' @examples
#' \dontrun{
#' get_sheet_info(dribble, 1)
#' }
get_sheet_info <- function(dribble, sheets = 1:8) {

  googlesheets4::gs4_auth(TRUE)

  ## Functions ----

  get_sheet_info_single <- function(dribble, sheet = NULL) {

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
      empty_info <- sum(!is.na(section_info))
      val_counts <- length(section_info) * nrow(section_info)
      prop_empty <- scales::percent(empty_info / val_counts)

      # Create a tibble
      info <- dplyr::tibble(
        sheet_name = dribble_info$name,
        range = get_range_from_name(sheet, name[x]),
        section = name[x],
        filled_cells = empty_info,
        total_cells = val_counts,
        completeness = prop_empty
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
      dplyr::mutate(days_since_last_modified = as.numeric(Sys.Date() - as.Date(dribble_info$modified_time)),
                    date_ran = Sys.Date(),
                    sheet = dplyr::case_when(sheet == 1 ~ "Donnees de base",
                                             sheet == 2 ~ "J(-3)",
                                             sheet == 3 ~ "J(-2)",
                                             sheet == 4 ~ "J(-1)",
                                             sheet == 5 ~ "Jour1",
                                             sheet == 6 ~ "Jour2",
                                             sheet == 7 ~ "Jour3",
                                             sheet == 8 ~ "Jour4",
                                             .default = as.character(sheet))
                    )

    return(dribble_summary)
  }
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
             "Ver de Guinée" = "FD:FD",
             "BCG (0-11)" = "FE:FF",
             "VPI1 (0-11)" = "FG:FH",
             "DTC-HepB-Hib 1 (0-11)" = "FI:FJ",
             "DTC-HepB-Hib 2 (0-11)" = "FK:FL",
             "DTC-HepB-Hib 3 (0-11)" = "FM:FN",
             "PCV-13 1 (0-11)" = "FP:FQ",
             "PCV-13 2 (0-11)" = "FR:FS",
             "PCV-13 3 (0-11)" = "FT:FU",
             "Rota 1 (0-11)" = "FW:FX",
             "Rota 2 (0-11)" = "FY:FZ",
             "Rota 3 (0-11)" = "GA:GB",
             "VAR1 (0-11)" = "GD:GE",
             "VAR2 (0-11)" = "GF:GG",
             "VAA (0-11)" = "GH:GI",
             "vPI2 (0-11)" = "GJ:GK",
             "BCG (12-23)" = "GM:GN",
             "VPI1 (12-23)" = "GO:GP",
             "DTC-HepB-Hib 1 (12-23)" = "GQ:GR",
             "DTC-HepB-Hib 2 (12-23)" = "GS:GT",
             "DTC-HepB-Hib 3 (12-23)" = "GU:GV",
             "PCV-13 1 (12-23)" = "GX:GY",
             "PCV-13 2 (12-23)" = "GZ:HA",
             "PCV-13 3 (12-23)" = "HB:HC",
             "Rota 1 (12-23)" = "HE:HF",
             "Rota 2 (12-23)" = "HG:HH",
             "Rota 3 (12-23)" = "HI:HJ",
             "VAR1 (12-23)" = "HL:HL",
             "VAR2 (12-23)" = "HN:HO",
             "VAA (12-23)" = "HP:HQ",
             "vPI2 (12-23)" = "HR:HS",
             "BCG (24-59)" = "HU:HV",
             "VPI1 (24-59)" = "HW:HX",
             "DTC-HepB-Hib 1 (24-59)" = "HY:HZ",
             "DTC-HepB-Hib 2 (24-59)" = "IA:IB",
             "DTC-HepB-Hib 3 (24-59)" = "IC:ID",
             "PCV-13 1 (24-59)" = "IF:IG",
             "PCV-13 2 (24-59)" = "IH:II",
             "PCV-13 3 (24-59)" = "IJ:IK",
             "Rota 1 (24-59)" = "IM:IN",
             "Rota 2 (24-59)" = "IO:IP",
             "Rota 3 (24-59)" = "IQ:IR",
             "VAR1 (24-59)" = "IT:IU",
             "VAR2 (24-59)" = "IV:IW",
             "VAA (24-59)" = "IX:IY",
             "vPI2 (24-59)" = "IZ:JA",
             "FE_Td1 (15-49 ans)" = "JC:JD",
             "FE_Td2 (15-49 ans)" = "JE:JE",
             "FE_Td3 (15-49 ans)" = "JF:JF",
             "FE_Td4 (15-49 ans)" = "JG:JG",
             "FE_Td5 (15-49 ans)" = "JH:JH"
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

  ## Parallel call ----
  doFuture::registerDoFuture()

  if (stringr::str_starts(Sys.getenv("SF_PARTNER"), "posit_workbench")) {
    future::plan(future::multicore)
  } else {
    future::plan(future::multisession)
  }

  options(doFuture.rng.onMisuse = "ignore")
  xs <- 1:nrow(dribble)

  progressr::handlers("cli")
  progressr::with_progress({
    p <- progressr::progressor(along = xs)
    y <-
      foreach::`%dopar%`(foreach::foreach(
        x = xs,
        .packages = c("googlesheets4", "dplyr", "scales", "googledrive"),
        .export = "template_info"
      ), {
        p()
        sheet_info <- purrr::map(sheets, \(i) get_sheet_info_single(dribble[x, ], i))
        sheet_info <- sheet_info |> dplyr::bind_rows()
      })
  })

  return(dplyr::bind_rows(y))
}
