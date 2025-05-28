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
#' @param campaign_folder `str` Path to the campaign folder. Defaults to [getwd()].
#' @param prov_target `str` A province or a vector of province names.
#' @param antenne_target `str` Antenne or a vector of antenne.
#' @param start_date `str` Start date of the campaign.
#' @param end_date `str` End date of the campaign.
#' @param zs_masque `str` Path to the masque template.
#' @param output_folder `str` Where to output the campaign.
#'
#' @returns Success message
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' init_campaign()
#' }
init_campaign <- function(start_date,
                          end_date,
                          campaign_name = Sys.Date(),
                          campaign_folder = getwd(),
                          prov_target = NULL,
                          antenne_target = NULL,
                          zs_target = NULL,
                          gdb = NULL,
                          zs_masque = system.file("extdata", "zs_masque_template.xlsx",
                            package = "rdcAVS"
                          ),
                          output_folder = campaign_folder) {
  # start
  start_time <- Sys.time()

  # Format the start and end dates
  start_date <- lubridate::as_date(start_date, format = c("%d/%m/%Y", "%Y-%m-%d"))
  end_date <- lubridate::as_date(end_date, format = c("%d/%m/%Y", "%Y-%m-%d"))
  campaign_name <- paste0("CAMPAGNE_", campaign_name)

  # Check if the ZS masque template is in the campaign folder
  if (!file.exists(zs_masque)) {
    if (zs_masque == "") {
      cli::cli_abort("data not found in package.")
    }
    cli::cli_abort(paste0(
      "Please add the zone de sante template file in the ",
      "campaign folder."
    ))
  } else {
    cli::cli_alert_success("zone de sante template file found!")
  }

  # Standardize geographic inputs
  if (!is.null(prov_target)) {
    prov_target <- stringr::str_trim(stringr::str_to_upper(prov_target))
  }

  if (!is.null(zs_target)) {
    zs_target <- stringr::str_trim(stringr::str_to_upper(zs_target))
  }

  if (!is.null(antenne_target)) {
    antenne_target <- stringr::str_trim(stringr::str_to_upper(antenne_target))
  }

  # Validate entries
  if (!is.null(prov_target)) {
    validate_geography(gdb, prov_target, "prov")
  }

  if (!is.null(antenne_target)) {
    validate_geography(gdb, antenne_target, "ant")
  }

  if (!is.null(zs_target)) {
    validate_geography(gdb, zs_target, "zs")
  }

  # Creating the folder structure

  # Create filtering query to the geographic database
  staged_folder <- gdb
  if (!is.null(prov_target)) {
    staged_folder <- staged_folder |>
      dplyr::filter(provinces %in% prov_target)
  }

  if (!is.null(antenne_target)) {
    staged_folder <- staged_folder |>
      dplyr::filter(antennes %in% antenne_target)
  }

  if (!is.null(zs_target)) {
    staged_folder <- staged_folder |>
      dplyr::filter(zones_de_sante %in% zs_target)
  }

  # Alert user for how many ZS folders will be created
  cli::cli_alert_info(paste0(length(unique(
    staged_folder$zones_de_sante
  )), " unique zone de santes identified"))

  # Create folder hierarchy
  cli::cli_process_start("Creating folder hierarchy locally")
  folder_structure <- staged_folder |>
    dplyr::select(provinces, zones_de_sante, antennes) |>
    dplyr::distinct() |>
    dplyr::mutate(
      local_path = file.path(
        output_folder,
        campaign_name,
        provinces,
        antennes,
        zones_de_sante
      ),
      masque_names = paste0(
        campaign_name,
        "_PROV_",
        provinces,
        "_AN_",
        antennes,
        "_ZS_",
        zones_de_sante,
        ".xlsx"
      ),
      masque_path = file.path(local_path, masque_names),
      debut = strftime(start_date, "%d/%m/%Y"),
      fin = strftime(end_date, "%d/%m/%Y"),
      period = paste0("Du ", debut, " au ", fin)
    )

  # Add the aires de santes
  folder_structure <- dplyr::left_join(
    folder_structure,
    staged_folder |>
      dplyr::group_by(provinces, zones_de_sante) |>
      dplyr::summarize(
        aires_de_sante = list(aires_de_sante),
        population_totale = list(population_totale)
      )
  )

  purrr::walk(
    folder_structure$local_path,
    \(x) dir.create(x, showWarnings = FALSE, recursive = TRUE),
    .progress = TRUE
  )

  cli::cli_process_done()

  # Add the masques to each folder
  cli::cli_process_start("Creating template files")
  purrr::walk(folder_structure$masque_path,
    \(x) file.copy(zs_masque, x),
    .progress = TRUE
  )
  cli::cli_process_done()

  # Edit the masques and add information
  cli::cli_process_start("Prefilling information for each template file")
  edit_zs_template_parallel(folder_structure)
  cli::cli_process_done()

  # Check folder size
  cli::cli_alert_info(paste0(
    "NOTE: the size of this campaign folder is ",
    round(check_folder_size(
      file.path(output_folder, campaign_name)
    ), 0),
    "MB"
  ))

  cli::cli_alert_success(paste0(
    "Campaign successfully initialized in ",
    round(difftime(Sys.time(),
      start_time,
      units = "mins"
    ), 2), " mins!"
  ))
  invisible()
}

# Private functions ----

#' Validates inputs against the geodatabase
#'
#' @param geo_vector `str` A name or a vector of names.
#' @param spatial_scale `str` Spatial scale. Valid values are:
#' - "prov": province
#' - "zs": zone de sante
#' - "as": aires de sante
#' @param gdb `parquet conn` A connection to the geodatabase.
#'
#' @returns `NULL`
#' @keywords internal
#'
validate_geography <- function(gdb, geo_vector, spatial_scale) {
  check <- switch(spatial_scale,
    "prov" = unique(dplyr::pull(gdb, "provinces")),
    "zs" = unique(dplyr::pull(gdb, "zones_de_sante")),
    "as" = unique(dplyr::pull(gdb, "aires_de_sante")),
    "ant" = unique(dplyr::pull(gdb, "antennes"))
  )

  check <- setdiff(geo_vector, check)

  col_name <- switch(spatial_scale,
    "prov" = "provinces",
    "zs" = "zones de sante",
    "as" = "aires de sante",
    "ant" = "antennes"
  )

  if (length(check) > 0) {
    cli::cli_alert_warning(paste0("The following ", col_name, " are not in the geodatabase: "))
    cli::cli_li(check)
    cli::cli_abort(paste0(
      "\nPlease double check or add this entry into the geodatabase. ",
      "To add a new entry to the geodatabase, please see: ?add_gdb_record()"
    ))
  } else {
    cli::cli_alert_success(paste0("All ", col_name, " in the geodatabase"))
  }
}


#' Checks the folder size
#'
#' @description
#' Check the folder size in GB.
#'
#'
#' @param folder_path `str` Path to the folder.
#'
#' @returns `int` Size of the folder in GB.
#' @keywords internal
#'
check_folder_size <- function(folder_path) {
  files <- list.files(folder_path, full.names = TRUE, recursive = TRUE)
  size <- sapply(files, function(x) file.size(x)) |> sum()
  gb_size <- size / 10e5

  return(gb_size)
}

edit_zs_template_parallel <- function(template_info) {
  doFuture::registerDoFuture()

  if (stringr::str_starts(Sys.getenv("SF_PARTNER"), "posit_workbench")) {
    future::plan(future::multicore)
  } else {
    future::plan(future::multisession)
  }

  options(doFuture.rng.onMisuse = "ignore")
  xs <- 1:nrow(template_info)

  progressr::handlers("cli")
  progressr::with_progress({
    p <- progressr::progressor(along = xs)
    y <-
      foreach::`%dopar%`(foreach::foreach(
        x = xs,
        .packages = c("openxlsx"),
        .export = "template_info"
      ), {
        p()

        edit_zs_template <- function(template_info) {
          template_file <- openxlsx::loadWorkbook(template_info$masque_path)

          openxlsx::writeData(template_file,
            sheet = 1, template_info$debut,
            startRow = 2, startCol = "B"
          )
          openxlsx::writeData(template_file,
            sheet = 1, template_info$fin,
            startRow = 2, startCol = "D"
          )
          openxlsx::writeData(template_file,
            sheet = 1, template_info$period,
            startRow = 1, startCol = "B"
          )

          if ("provinces" %in% names(template_info)) {
            openxlsx::writeData(template_file,
              sheet = 1, template_info$provinces,
              startRow = 1, startCol = "K"
            )
          }

          if ("antennes" %in% names(template_info)) {
            openxlsx::writeData(template_file,
              sheet = 1, template_info$antennes,
              startRow = 1, startCol = "P"
            )
          }

          if ("zones_de_sante" %in% names(template_info)) {
            openxlsx::writeData(template_file,
              sheet = 1, template_info$zones_de_sante,
              startRow = 1, startCol = "V"
            )
          }

          openxlsx::writeData(template_file,
            sheet = 1,
            unlist(template_info$aires_de_sante),
            startRow = 4, startCol = "D"
          )

          openxlsx::writeData(template_file,
            sheet = 1,
            unlist(template_info$population_totale),
            startRow = 4, startCol = "H"
          )

          file.remove(template_info$masque_path)

          openxlsx::saveWorkbook(template_file, template_info$masque_path,
            overwrite = TRUE
          )

          invisible()
        }
        edit_zs_template(template_info[x, ])
      })
  })

  invisible()
}
