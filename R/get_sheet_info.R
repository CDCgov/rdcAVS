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

    # Calculate max rows for each masque
    ss_max_row <- googlesheets4::range_read(dribble, 1,
                                            range = googlesheets4::cell_cols("D"),
                                            col_names = FALSE) |>
      dplyr::filter(!is.na(`...1`),
                    `...1` != "NULL")
    ss_max_row <- ss_max_row[-nrow(ss_max_row), ]
    # Get rid of totals
    ss_max_row <- ss_max_row[4:nrow(ss_max_row), ]
    # Get rid of counts, if they exist
    ss_max_row <- nrow(ss_max_row)

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
      start_range <-  a1_to_colnum(range[1])
      end_range <- a1_to_colnum(range[2])
      range <- start_range:end_range
      section_info <- ss_range[, c(1:4, # geographic info
                                   range)]

      info <- section_info |>
        dplyr::group_by_at(1:3) |>
        dplyr::summarize(
          empty_cells = sum(is.na(dplyr::across(2:(ncol(section_info)-3)))),
          filled_cells = sum(!is.na(dplyr::across(2:(ncol(section_info)-3))))
        ) |>
        dplyr::ungroup() |>
        dplyr::rename(
          "prov" = 1,
          "antenne" = 2,
          "zone_de_sante" = 3
        ) |>
        dplyr::mutate(
          total_cells = empty_cells + filled_cells,
          completeness = scales::percent(filled_cells / total_cells),
          range = get_range_from_name(sheet, name[x]),
          section = name[x],
          sheet = sheet,
        ) |>
        dplyr::select(-empty_cells) |>
        dplyr::select(prov, antenne, zone_de_sante, sheet, range, section,
                      filled_cells, total_cells, completeness)
    })
    sections <- dplyr::bind_rows(sections)

    # Join back to the dribble
    dribble_summary <- sections |>
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
             "Situation des \u00e9quipes de vaccination par aires de sant\u00e9" = "E:G",
             "Populations cibles vaccin\u00e9es \u00e0 la campagne pr\u00e9c\u00e9dente" = "N:O",
             "Disponilit\u00e9 du vaccin, antig\u00e8ne utilis\u00e9" = "V:V",
             "Nombre d'\u00e9quipes de mobilisation sociale de proxi" = "Y:Y",
             "RESEAU/ INTERNET" = "Z:AB",
             "Enfants 0 - 11 mois identifi\u00e9s pour etre r\u00e9cup\u00e9r\u00e9s en PEV de routine" = "AC:AQ",
             "Enfants 12 - 23 mois identifi\u00e9s pour etre r\u00e9cup\u00e9r\u00e9s en PEV de routine" = "AS:BG",
             "Enfants 24 - 59 mois identifi\u00e9s pour etre r\u00e9cup\u00e9r\u00e9s en PEV de routine" = "BI:BW",
             "Femmes cibles identifi\u00e9es pour etre r\u00e9cup\u00e9r\u00e9es en PEV de routine" = "BY:CC",
             "LOCALISATION DES POPULATIONS SPECIALES" = "CE:CG",
             "Nombre de m\u00e9nages d\u00e9nombr\u00e9s" = "CH:CH",
             "Populations sp\u00e9ciales (Nombre de personnes selon le Statut)" = "CI:CK",
             "Effectifs des Populations sp\u00e9ciales recens\u00e9es" = "CL:CU")
    } else if (sheet %in% 2:4) {
      switch(name,
             "Nombre de m\u00e9nage" = "G:G",
             "Compl\u00e9tude MoSo" = "I:I",
             "Nombre de personnes de plus de 15 ans sensibilis\u00e9es" = "K:L",
             "0-11 mois d\u00e9nombr\u00e9s" = "N:O",
             "12-59 mois d\u00e9nombr\u00e9s" = "Q:R",
             "5-14 ans d\u00e9nombr\u00e9s" = "W:X",
             "Cas de maladies sous surveillance" = "Y:AC",
             "REFUS" = "AD:AE",
             "Mobilisation communautaire des FEMMES" = "AF:AN",
             "Mobilisation communautaire des HOMMES" = "AO:AW",
             "Mobilisation communautaire g\u00e9n\u00e9rale" = "AX:BI",
             "Rattrapage/r\u00e9cup\u00e9ration en PEV" = "BJ:BK"
      )
    } else if (sheet %in% 5:8) {
      switch(name,
             "Nombre de m\u00e9nage" = "G:G",
             "Compl\u00e9tude MoSo" = "I:I",
             "Nombre de personnes de plus de 15 ans sensibilis\u00e9es" = "K:L",
             "0-11 mois d\u00e9nombr\u00e9s" = "N:O",
             "12-59 mois d\u00e9nombr\u00e9s" = "Q:R",
             "5-14 ans d\u00e9nombr\u00e9s" = "W:X",
             "Cas de maladies sous surveillance" = "Y:AC",
             "REFUS" = "AD:AE",
             "Mobilisation communautaire des FEMMES" = "AF:AN",
             "Mobilisation communautaire des HOMMES" = "AO:AW",
             "Mobilisation communautaire g\u00e9n\u00e9rale" = "AX:BI",
             "Rattrapage/r\u00e9cup\u00e9ration en PEV" = "BJ:BK",
             "Compl\u00e9tude des rapports de vaccination" = "CL:CN",
             "Vaccines 0-11 mois Z\u00e9ro Dose" = "CP:CQ",
             "Vaccines 0-11 mois 1 dose et plus" = "CR:CS",
             "Vaccines 12-59 mois Z\u00e9ro Dose" = "CW:CX",
             "Vaccines 12-59 mois 1 dose et plus" = "CY:CZ",
             "Vaccines 0-11 mois Z\u00e9ro Dose" = "DD:DE",
             "Vaccines 0-11 mois 1 dose et plus" = "DF:DG",
             "Vaccines 12-59 mois Z\u00e9ro Dose" = "DK:DL",
             "Vaccines 12-59 mois 1 dose et plus" = "DM:DN",
             "Vaccin\u00e9s par les Equipes en milieu rural" = "EA:EB",
             "Vaccin\u00e9s par les Equipes en milieu urbain" = "EC:ED",
             "GESTION DES FLACONS" = "EQ:EW",
             "Recherche active des cas de MEV" = "EX:FA",
             "MAPI" = "FB:FC",
             "Ver de Guin\u00e9e" = "FD:FD",
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
        "Situation des \u00e9quipes de vaccination par aires de sant\u00e9",
        "Populations cibles vaccin\u00e9es \u00e0 la campagne pr\u00e9c\u00e9dente",
        "Disponilit\u00e9 du vaccin, antig\u00e8ne utilis\u00e9" ,
        "Nombre d'\u00e9quipes de mobilisation sociale de proxi",
        "RESEAU/ INTERNET" ,
        "Enfants 0 - 11 mois identifi\u00e9s pour etre r\u00e9cup\u00e9r\u00e9s en PEV de routine",
        "Enfants 12 - 23 mois identifi\u00e9s pour etre r\u00e9cup\u00e9r\u00e9s en PEV de routine",
        "Enfants 24 - 59 mois identifi\u00e9s pour etre r\u00e9cup\u00e9r\u00e9s en PEV de routine",
        "Femmes cibles identifi\u00e9es pour etre r\u00e9cup\u00e9r\u00e9es en PEV de routine",
        "LOCALISATION DES POPULATIONS SPECIALES",
        "Nombre de m\u00e9nages d\u00e9nombr\u00e9s",
        "Populations sp\u00e9ciales (Nombre de personnes selon le Statut)",
        "Effectifs des Populations sp\u00e9ciales recens\u00e9es"
      )

    } else if (sheet %in% 2:4) {

      c("Nombre de m\u00e9nage",
        "Compl\u00e9tude MoSo",
        "Nombre de personnes de plus de 15 ans sensibilis\u00e9es",
        "0-11 mois d\u00e9nombr\u00e9s",
        "12-59 mois d\u00e9nombr\u00e9s",
        "5-14 ans d\u00e9nombr\u00e9s",
        "Cas de maladies sous surveillance",
        "REFUS",
        "Mobilisation communautaire des FEMMES",
        "Mobilisation communautaire des HOMMES",
        "Mobilisation communautaire g\u00e9n\u00e9rale",
        "Rattrapage/r\u00e9cup\u00e9ration en PEV")

    } else if (sheet %in% 5:8) {

      c(
        "Nombre de m\u00e9nage",
        "Compl\u00e9tude MoSo",
        "Nombre de personnes de plus de 15 ans sensibilis\u00e9es",
        "0-11 mois d\u00e9nombr\u00e9s",
        "12-59 mois d\u00e9nombr\u00e9s",
        "5-14 ans d\u00e9nombr\u00e9s",
        "Cas de maladies sous surveillance",
        "REFUS",
        "Mobilisation communautaire des FEMMES",
        "Mobilisation communautaire des HOMMES",
        "Mobilisation communautaire g\u00e9n\u00e9rale",
        "Rattrapage/r\u00e9cup\u00e9ration en PEV",
        "Compl\u00e9tude des rapports de vaccination",
        "Vaccines 0-11 mois Z\u00e9ro Dose",
        "Vaccines 0-11 mois 1 dose et plus",
        "Vaccines 12-59 mois Z\u00e9ro Dose",
        "Vaccines 12-59 mois 1 dose et plus",
        "Vaccines 0-11 mois Z\u00e9ro Dose",
        "Vaccines 0-11 mois 1 dose et plus",
        "Vaccines 12-59 mois Z\u00e9ro Dose",
        "Vaccines 12-59 mois 1 dose et plus",
        "Vaccin\u00e9s par les Equipes en milieu rural",
        "Vaccin\u00e9s par les Equipes en milieu urbain",
        "GESTION DES FLACONS",
        "Recherche active des cas de MEV",
        "MAPI",
        "Ver de Guin\u00e9e"
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
        sheet_info <- purrr::map(sheets, \(i) {
          tryCatch(
            {
              get_sheet_info_single(dribble[x, ], i)
              },
            error = \(e) {
              cli::cli_alert_info(paste0("Extraction failed for the following template: ",
                                         dribble[x, ]$name, " Sheet number: ", i))
              NULL
            }
          )
          })
        sheet_info <- sheet_info |> dplyr::bind_rows()
      })
  })

  return(dplyr::bind_rows(y))
}
