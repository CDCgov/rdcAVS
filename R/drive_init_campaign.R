# This function will be used to manage the logic for setting up
# SIA campaigns.

#' Initialize an SIA campaign
#'
#' @description
#' Creates the proper folder structure for a campaign.
#'
#'
#' @param zs_target `str` A vector of Zone de Santes targeted for the campaign.
#' @param gdb `rda` rda file for the Geo database.
#' @param campaign_name `str` The name of the SIA campaign. Defaults to the
#' date ran.
#' @param prov_target `str` A province or a vector of province names.
#' @param antenne_target `str` Antenne or a vector of antenne.
#' @param start_date `str` Start date of the campaign.
#' @param end_date `str` End date of the campaign.
#' @param zs_masque `str` Dribble of the masque template.
#'
#' @returns Success message
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' init_campaign()
#' }
drive_init_campaign <- function(start_date,
                          end_date,
                          campaign_name = Sys.Date(),
                          prov_target = NULL,
                          antenne_target = NULL,
                          zs_target = NULL,
                          gdb = NULL,
                          zs_masque) {

  start_time <- Sys.time()
  googledrive::drive_auth(TRUE)
  googlesheets4::gs4_auth(TRUE)

  if (is.null(campaign_name) | campaign_name  == "") {
    campaign_name <- Sys.Date()
  }

  start_date <- lubridate::as_date(start_date, format = c("%d/%m/%Y", "%Y-%m-%d"))
  end_date <- lubridate::as_date(end_date, format = c("%d/%m/%Y", "%Y-%m-%d"))
  campaign_name <- paste0("CAMPAGNE_", campaign_name)

  if (!is.null(prov_target)) {
    prov_target <- stringr::str_trim(stringr::str_to_upper(prov_target))
    validate_geography(gdb, prov_target, "prov")
  }

  if (!is.null(antenne_target)) {
    antenne_target <- stringr::str_trim(stringr::str_to_upper(antenne_target))
    validate_geography(gdb, antenne_target, "ant")
  }

  if (!is.null(zs_target)) {
    zs_target <- stringr::str_trim(stringr::str_to_upper(zs_target))
    validate_geography(gdb, zs_target, "zs")
  }

  staged_folder <- gdb

  if (!is.null(prov_target)) {
    staged_folder <- staged_folder |>
      dplyr::filter(provinces %in% prov_target)
  }

  if (!is.null(antenne_target)) {
    staged_folder <- staged_folder |>
      dplyr::filter(antennes %in% antenne_target)
  }

  base_structure <- staged_folder |>
    dplyr::distinct(provinces, antennes)

  zs_filtered <- staged_folder
  if (!is.null(zs_target)) {
    zs_filtered <- zs_filtered |>
      dplyr::filter(zones_de_sante %in% zs_target)
  }

  folder_structure <- dplyr::bind_rows(
    zs_filtered |> dplyr::select(provinces, antennes, zones_de_sante),
    base_structure |>
      dplyr::anti_join(zs_filtered |>
                         dplyr::select(provinces, antennes), by = c("provinces", "antennes")) |>
      dplyr::mutate(zones_de_sante = NA_character_)
  ) |> dplyr::distinct()

  cli::cli_alert_info(paste0(length(unique(folder_structure$zones_de_sante[!is.na(folder_structure$zones_de_sante)])),
                             " unique zone de santes identified"))

  cli::cli_process_start("Creating folder hierarchy in Google Drive")

  folder_structure <- folder_structure |>
    dplyr::mutate(
      prov_path = file.path(provinces),
      antenne_path = file.path(provinces,
                               antennes),
      zs_path = file.path(provinces,
        antennes,
        ifelse(is.na(zones_de_sante), "", zones_de_sante)
      ),
      masque_names = ifelse(
        is.na(zones_de_sante),
        NA_character_,
        paste0(
          campaign_name,
          "_PROV_", provinces,
          "_AN_", antennes,
          "_ZS_", zones_de_sante
        )
      ),
      masque_path = ifelse(
        is.na(zones_de_sante),
        NA_character_,
        file.path(zs_path, masque_names)
      ),
      debut = strftime(start_date, "%d/%m/%Y"),
      fin = strftime(end_date, "%d/%m/%Y"),
      period = paste0("Du ", debut, " au ", fin)
    )

  folder_structure <- dplyr::left_join(
    folder_structure,
    staged_folder |> dplyr::group_by(provinces, zones_de_sante) |> dplyr::summarize(
      aires_de_sante = list(aires_de_sante),
      population_totale = list(population_totale),
      .groups = "drop"
    ),
    by = c("provinces", "zones_de_sante")
  )

  cli::cli_process_done()

  campaign_drive_folder <- googledrive::drive_mkdir(campaign_name,
                                                      path = "~",
                                                    overwrite = TRUE)

  cli::cli_process_start("Making province folders")
  prov_drive_folders <- drive_mkdir_parallel(folder_structure$prov_path |>
                                               unique(),
                                             campaign_drive_folder)
  cli::cli_process_done()

  # There are rate limits to the API (12K/min)
  # To reduce the likelihood of exceeding rate limits, need to wait

  cli::cli_process_start("Making antenne folders")

  antenne_drive_folders <- purrr::map(1:nrow(prov_drive_folders), \(i) {
    prov_name <- prov_drive_folders$name[i]
    relevant_antennes <- folder_structure |>
      dplyr::filter(provinces %in% prov_name) |>
      pull(antennes) |>
      unique()

    drive_mkdir_parallel(relevant_antennes, prov_drive_folders[i, ])
  }, .progress = TRUE)

  antenne_drive_folders <- dplyr::bind_rows(antenne_drive_folders)

  cli::cli_process_done()

  # Make zs folders in parallel
  cli::cli_process_start("Making zones de sante folders")

  zs_drive_folders <- purrr::map(1:nrow(antenne_drive_folders), \(i) {

    antenne_name <- antenne_drive_folders$name[i]
    prov_name <- googledrive::drive_get(as_id(drive_reveal(antenne_drive_folders[i, ],
                                             what = "parents")$parents |>
                                  purrr::pluck(1,1))) |>
      dplyr::pull(name)

    relevant_zs <- folder_structure |>
      dplyr::filter(provinces %in% prov_name,
                    antennes %in% antenne_name) |>
      pull(zones_de_sante) |>
      unique()

    zs_drive_folders <- drive_mkdir_parallel(relevant_zs, antenne_drive_folders[i, ])
    zs_drive_folders <- zs_drive_folders |>
      dplyr::mutate(provinces = prov_name,
                    antennes = antenne_name)
  }, .progress = TRUE)

  zs_drive_folders <- dplyr::bind_rows(zs_drive_folders)

  cli::cli_process_done()

  cli::cli_process_start("Creating template files")
  zs_drive_folders <- dplyr::left_join(zs_drive_folders, folder_structure |>
                             dplyr::select(provinces, antennes,
                                           name = zones_de_sante, masque_names,
                                           aires_de_sante, population_totale,
                                           debut, fin, period) |>
                             dplyr::distinct())

  drive_cp_zs_template_parallel(zs_masque, zs_drive_folders)

  cli::cli_process_done()

  cli::cli_alert_success(paste0(
    "Campaign successfully initialized in ",
    round(difftime(Sys.time(), start_time, units = "mins"), 2), " mins!"
  ))

  return(drive_reveal(campaign_drive_folder,
                      what = "webViewLink") |>
           pull(web_view_link)
         )
}


# Private functions ----

#' Create Drive folders into another folder in parallel
#'
#' @param names `list` A list of folder names.
#' @param target_folder `dribble` Dribble of the target folder.
#'
#' @returns `dribble` A dribble of the recently created folders.
#' @keywords internal
#'
drive_mkdir_parallel <- function(names, target_folder) {
  doFuture::registerDoFuture()

  if (stringr::str_starts(Sys.getenv("SF_PARTNER"), "posit_workbench")) {
    future::plan(future::multicore)
  } else {
    future::plan(future::multisession)
  }

  options(doFuture.rng.onMisuse = "ignore")
  idx <- 1:length(names)

  progressr::handlers("cli")
  progressr::with_progress({
    p <- progressr::progressor(along = idx)
    dir_dribbles <-
      foreach::`%dopar%`(foreach::foreach(
        x = idx,
        .packages = "googledrive"
      ), {
        p()
          googledrive::with_drive_quiet(googledrive::drive_mkdir(names[x], path = target_folder,
                                                                 overwrite = TRUE))
        })
  })

  return(dplyr::bind_rows(dir_dribbles))

  }

#' Copy template folder to the ZS folder
#'
#' @param template_dribble `dribble` A dribble object for the template sheet.
#' @param zs_drive_folders `dribble` A dribble of zones de sante folders.
#'
#' @returns `dribble` A dribble containing the template files of a campaign.
#' @keywords internal
#'
drive_cp_zs_template_parallel <- function(template_dribble, zs_drive_folders) {
  doFuture::registerDoFuture()

  if (stringr::str_starts(Sys.getenv("SF_PARTNER"), "posit_workbench")) {
    future::plan(future::multicore)
  } else {
    future::plan(future::multisession)
  }

  options(doFuture.rng.onMisuse = "ignore")
  idx <- 1:nrow(zs_drive_folders)

  progressr::handlers("cli")
  progressr::with_progress({
    p <- progressr::progressor(along = idx)
    dir_dribbles <-
      foreach::`%dopar%`(foreach::foreach(
        x = idx,
        .packages = c("googledrive", "googlesheets4")
      ), {
        p()

        googledrive::drive_auth(TRUE)
        googlesheets4::gs4_auth(TRUE)
        zs_template <- googledrive::drive_cp(template_dribble,
                                             path = zs_drive_folders[x, ],
                              name = zs_drive_folders[x, ] |>
                                dplyr::pull(masque_names),
                              overwrite = TRUE)

        googlesheets4::range_write(zs_template,
                                   zs_drive_folders[x, ] |> dplyr::select(provinces),
                                   sheet = 1,
                                   range = "K1",
                                   col_names = FALSE,
                                   reformat = FALSE)

        googlesheets4::range_write(zs_template,
                                   zs_drive_folders[x, ] |> dplyr::select(antennes),
                                   sheet = 1,
                                   range = "P1",
                                   col_names = FALSE,
                                   reformat = FALSE)

        googlesheets4::range_write(zs_template,
                                   zs_drive_folders[x, ] |> dplyr::select(name),
                                   sheet = 1,
                                   range = "V1",
                                   col_names = FALSE,
                                   reformat = FALSE)

        googlesheets4::range_write(zs_template,
                                   zs_drive_folders[x, ] |>
                                     dplyr::select(period),
                                   sheet = 1,
                                   range = "B1",
                                   col_names = FALSE,
                                   reformat = FALSE)

        googlesheets4::range_write(zs_template,
                                   zs_drive_folders[x, ] |> dplyr::select(debut),
                                   sheet = 1,
                                   range = "B2",
                                   col_names = FALSE,
                                   reformat = FALSE)

        googlesheets4::range_write(zs_template,
                                   zs_drive_folders[x, ] |> dplyr::select(fin),
                                   sheet = 1,
                                   range = "D2",
                                   col_names = FALSE,
                                   reformat = FALSE)

        googlesheets4::range_write(zs_template,
                                   zs_drive_folders[x, ] |>
                                     dplyr::pull(aires_de_sante) |>
                                     purrr::pluck(1) |>
                                     dplyr::as_tibble(),
                                   sheet = 1,
                                   range = "D4",
                                   col_names = FALSE,
                                   reformat = FALSE)

        googlesheets4::range_write(zs_template,
                                   zs_drive_folders[x, ] |>
                                     dplyr::pull(population_totale) |>
                                     purrr::pluck(1) |>
                                     dplyr::as_tibble(),
                                   sheet = 1,
                                   range = "H4",
                                   col_names = FALSE,
                                   reformat = FALSE)

      })
  })

  return(invisible())
}
