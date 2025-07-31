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
    ss_max_row <- googlesheets4::range_read(dribble, 1, "D3:D48") |>
      dplyr::filter(!is.na(`Aires de santé`)) |>
      nrow()

    # Obtain the modified time from a dribble object
    dribble_info <- googledrive::drive_reveal(dribble, what = "modifiedTime")

    # Read the spreadsheet
    geo_info <- googlesheets4::range_read(dribble_info, sheet, range=paste0("A3:D", ss_max_row + 3))
    target <- googlesheets4::range_read(dribble_info, sheet, range=paste0("CO3:CO", ss_max_row + 3))
    coverage <- googlesheets4::range_read(dribble_info, sheet, range=paste0("DZ3:DZ", ss_max_row + 3))
    completeness <- googlesheets4::range_read(dribble_info, sheet, range=paste0("CL3:CM", ss_max_row + 3))
    rural_urban <- googlesheets4::range_read(dribble_info, sheet, range = paste0("EF3:EI", ss_max_row + 3))
    rural_urban <- rural_urban |>
      dplyr::select(2,4) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), \(x) round(x)))
    recoveries_0_11 <- googlesheets4::range_read(dribble_info, sheet, range = paste0("GL3:GL", ss_max_row + 3))
    recoveries_12_23 <- googlesheets4::range_read(dribble_info, sheet, range = paste0("HT3:HT", ss_max_row + 3))
    recoveries_24_59 <- googlesheets4::range_read(dribble_info, sheet, range = paste0("JB3:JB", ss_max_row + 3))

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
      )))

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
        sheet_info <- purrr::map(sheets, \(i) get_campaign_progress_single(dribble[x, ], i))
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
        #"couverture_campagne_pct",
        "couverture_campaign_cumulative",
        "avg_vax_rural",
        "avg_vax_urban",
        #"recovery_0_11",
        #"recovery_12_23",
        #"recovery_24_59",
        "recovery_0_11_cumulative",
        "recovery_12_23_cumulative",
        "recovery_24_59_cumulative"
      ))
    )

  return(final_summary)
}
