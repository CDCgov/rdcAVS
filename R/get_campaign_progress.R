#' Monitor campaign quality
#'
#' @description
#' Obtains the completeness of coverage for each day of the campaign.
#'
#'
#' @param dribble `dribble` Dribble containing ZS dribbles related to a specific campaign.
#' @param sheets `int` Sheet number. Corresponds to Jour 1-4 for sheets 5-8, respectively.
#'
#' @returns `tibble` Containing summary of daily coverage
#' @export
#'
#' @examples
#' \dontrun{
#' dribble <- drive_get("zs_spreadsheet_url")
#' summary <- get_campaign_progress(dribble)
#' }
get_campaign_progress <- function(dribble, sheets = 5:8) {
  googlesheets4::gs4_auth(TRUE)
  googledrive::drive_auth(TRUE)

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

  get_campaign_progress_single <- function(dribble, sheet) {

    # Extract the following columns for each section
    # Target: Cible 0-59 Mois (column CO)
    # Coverage: Vaccination 0-59 mois (column DZ)
    # Completude des rapports de vaccination (CL:CN)
    # Calculate totals

    sheet_name <- dplyr::case_when(
      sheet == 5 ~ "Jour 1",
      sheet == 6 ~ "Jour 2",
      sheet == 7 ~ "Jour 3",
      sheet == 8 ~ "Jour 4"
    )

    # Check that what is passed is a single dribble
    if (nrow(dribble) > 1) {
      cli::cli_abort("Please pass only a one-row dribble.")
    }

    # Calculate max rows for each masque
    # Calculate max rows for each masque
    ss_max_row <- googlesheets4::range_read(dribble, 1,
                                            range = googlesheets4::cell_cols("D"),
                                            col_names = FALSE) |>
      dplyr::filter(!is.na(`...1`),
                    `...1` != "NULL")
    # Get rid of totals
    ss_max_row <- ss_max_row[4:nrow(ss_max_row), ]
    # Get rid of counts, if they exist
    ss_max_row <- nrow(ss_max_row)

    # Obtain the modified time from a dribble object
    dribble_info <- googledrive::drive_reveal(dribble, what = "modifiedTime")

    # Read the spreadsheet once
    ss_data <- googlesheets4::range_read(dribble_info, sheet, range=googlesheets4::cell_rows(c(3, ss_max_row + 3)))
    geo_info <- ss_data |> dplyr::select(a1_to_colnum("A"):a1_to_colnum("D")) |>
      dplyr::rename_with(\(x) gsub("\\.\\.\\.[0-9]+$", "", x))
    target <- ss_data |> dplyr::select(a1_to_colnum("CO")) |>
      dplyr::rename_with(\(x) gsub("\\.\\.\\.[0-9]+$", "", x))
    coverage <- ss_data |> dplyr::select(a1_to_colnum("DZ")) |>
      dplyr::rename_with(\(x) gsub("\\.\\.\\.[0-9]+$", "", x))
    completeness <- ss_data |> dplyr::select(a1_to_colnum("CL"):a1_to_colnum("CM")) |>
      dplyr::rename_with(\(x) gsub("\\.\\.\\.[0-9]+$", "", x))
    rural_urban <- ss_data |> dplyr::select(a1_to_colnum("EF"):a1_to_colnum("EI")) |>
      dplyr::select(2,4) |>
      dplyr::rename_with(\(x) gsub("\\.\\.\\.[0-9]+$", "", x)) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), \(x) round(x)))
    recoveries_0_11 <- ss_data |> dplyr::select(a1_to_colnum("IX")) |>  # Previously GL
      dplyr::rename_with(\(x) gsub("\\.\\.\\.[0-9]+$", "", x))
    recoveries_12_23 <- ss_data |> dplyr::select(a1_to_colnum("KF")) |> # Previously HT
      dplyr::rename_with(\(x) gsub("\\.\\.\\.[0-9]+$", "", x))
    recoveries_24_59 <- ss_data |> dplyr::select(a1_to_colnum("LN")) |> # Previously JB
      dplyr::rename_with(\(x) gsub("\\.\\.\\.[0-9]+$", "", x))

    summary <- dplyr::bind_cols(geo_info, target, coverage, completeness,
                                rural_urban,
                                recoveries_0_11, recoveries_12_23, recoveries_24_59) |>
      dplyr::rename(
        "province" = "Province",
        "antenne" = "Antenne",
        "zone_de_sante" = "Zone de Santé",
        "aire_de_sante" = "Aire de Santé",
        "rapports_de_vaccination_attendus" = "Attendus",
        "rapports_de_vaccination_recus" = "Recus",
        "cible_0_59_mois" = "Cible 0-59 mois",
        "vaccination_0_59_mois" = "Total",
        "avg_vax_rural" = "Nb moyen d'enfants vaccinés/équipe (ref 100 enfants)",
        "avg_vax_urban" = "Nb moyen d'enfants vaccinés/équipe (ref 200 enfants)",
        "recovery_0_11" = "# Récupérations 0-11 mois",
        "recovery_12_23" = "# Récupérations 12-23 mois",
        "recovery_24_59" = "# Récupérations 24-59 mois"
      ) |>
      dplyr::mutate(cible_0_59_mois = round(cible_0_59_mois, 0),
                    rapport_completude_pct = round(rapports_de_vaccination_recus / rapports_de_vaccination_attendus * 100, 0),
                    couverture_campagne_pct = round(vaccination_0_59_mois / cible_0_59_mois * 100, 0),
                    jour = sheet_name) |>
      dplyr::select(dplyr::any_of(c(
        "province",
        "antenne",
        "zone_de_sante",
        "aire_de_sante",
        "jour",
        "rapports_de_vaccination_attendus",
        "rapports_de_vaccination_recus",
        "rapport_completude_pct",
        "cible_0_59_mois",
        "vaccination_0_59_mois",
        "couverture_campagne_pct",
        "avg_vax_rural",
        "avg_vax_urban",
        "recovery_0_11",
        "recovery_12_23",
        "recovery_24_59"
      ))) |> 
      dplyr::distinct(.keep_all = TRUE)
      
      
    return(summary)
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
        .packages = c("googlesheets4", "dplyr", "googledrive"),
        .export = "template_info"
      ), {
        p()

        sheet_info <- purrr::map(sheets,
                                 \(i) {
                                   tryCatch(
                                     {
                                       get_campaign_progress_single(dribble[x, ], i) 

                                       },
                                     error = \(e) {
                                       cli::cli_alert_info(paste0("Extraction failed for the following template: ",
                                                                  dribble[x, ]$name, " Sheet number: ", i))
                                       NULL
                                     }
                                 )
                                   }
                                )
        sheet_info <- sheet_info |> dplyr::bind_rows()
      })
  })

  final_summary <- dplyr::bind_rows(y) |>    
    dplyr::group_by(province, antenne, zone_de_sante, aire_de_sante) |>
    dplyr::arrange(jour) |>
    dplyr::mutate(couverture_campaign_cumulative = cumsum(couverture_campagne_pct),
                  recovery_0_11_cumulative = cumsum(recovery_0_11),
                  recovery_12_23_cumulative = cumsum(recovery_12_23),
                  recovery_24_59_cumulative = cumsum(recovery_24_59)
                  ) |>
    dplyr::arrange(aire_de_sante) |>
    dplyr::ungroup() |>
    dplyr::select(
      dplyr::any_of(c(
        "province",
        "antenne",
        "zone_de_sante",
        "aire_de_sante",
        "jour",
        #"rapports_de_vaccination_attendus",
        #"rapports_de_vaccination_recus",
        "rapport_completude_pct",
        #"cible_0_59_mois",
        #"vaccination_0_59_mois",
        "couverture_campagne_pct",
        "couverture_campaign_cumulative",
        "avg_vax_rural",
        "avg_vax_urban",
        "recovery_0_11",
        "recovery_12_23",
        "recovery_24_59",
        "recovery_0_11_cumulative",
        "recovery_12_23_cumulative",
        "recovery_24_59_cumulative"
      ))
    ) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(c(
      #"rapports_de_vaccination_attendus",
      #"rapports_de_vaccination_recus",
      "rapport_completude_pct",
      #"cible_0_59_mois",
      #"vaccination_0_59_mois",
      "couverture_campagne_pct",
      "couverture_campaign_cumulative",
      "avg_vax_rural",
      "avg_vax_urban",
      "recovery_0_11",
      "recovery_12_23",
      "recovery_24_59",
      "recovery_0_11_cumulative",
      "recovery_12_23_cumulative",
      "recovery_24_59_cumulative")), \(x) ifelse(is.na(x), 0, x)))

  return(final_summary)
}
