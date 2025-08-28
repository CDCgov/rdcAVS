# Public functions ----

#' Sets the permissions across dribbles based on permissions table
#'
#' @description
#' Sets the permissions of a dribble based on the permissions table. If more
#' than one record is passed unto the `dribble`, all records will have the same
#' permissions as specified by the `permissions_table`.
#'
#' @param campaign_name `str` Name of the campaign.
#' @param permissions_table `tibble` A tibble with permissions info. Required
#' columns are: `emailAddress` and `role`.
#' @param dribble_files `dribble_files` A dribble object to assign permissions to.
#'
#' @details
#' Please see [drive_share()](https://googledrive.tidyverse.org/reference/drive_share.html)
#' for a full explanation on valid roles.
#'
#'
#' @returns `NULL` upon success.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' files <- drive_find()
#' set_permissions("CAMPAGNE_example", perm_data, files)
#' }
set_permissions <- function(campaign_name,
                            permissions_table,
                            dribble_files) {
  # Define the drive_path_builder function
  drive_path_builder <- function(level,
                                 campaign_name,
                                 province = NA,
                                 antenne = NA,
                                 zone_de_sante = NA) {
    switch(level,
      "national" = paste0(campaign_name, "/"),
      "province" = paste0(campaign_name, "/", province, "/"),
      "antenne" = paste0(campaign_name, "/", province, "/", antenne, "/"),
      "zone de sante" = paste0(
        campaign_name,
        "/",
        province,
        "/",
        antenne,
        "/",
        zone_de_sante,
        "/"
      ),
      NA
    ) # Return NA if level doesn't match any case
  }

  # Create associations for each dribble
  full_permission <- permissions_table |>
    dplyr::mutate(
      across(dplyr::all_of(
        c("province", "antenne", "zone_de_sante")
      ), \(x) trimws(toupper(x))),
      email = stringr::str_to_lower(stringr::str_trim(email)),
      role = stringr::str_to_lower(stringr::str_trim(role))
    ) |>
    dplyr::mutate(path = mapply(
      drive_path_builder,
      level,
      campaign_name,
      province,
      antenne,
      zone_de_sante
    )) |>
    dplyr::rename(user_name = name) |>
    dplyr::filter(!is.na(path)) |>
    dplyr::mutate(path = stringr::str_to_upper(path))

  # For those with editor access to the ZS masques,
  # only set permissions to the masque and not the folder
  zs_editors <- full_permission |>
    dplyr::filter(role == "writer" & level == "zone de sante")

  dribble_files <- dribble_files |>
    dplyr::mutate(path = stringr::str_replace_all(path, "~/", ""))

  dribble_files <- dribble_files |>
    dplyr::mutate(dir_path = ifelse(stringr::str_ends(path, "/"), path, paste0(fs::path_dir(path), "/"))) |>
    googledrive::drive_reveal("mimeType")
  files_for_zs_editors <- dribble_files |>
    dplyr::filter(str_ends(mime_type, "spreadsheet")) |>
    dplyr::select(-"path") |>
    dplyr::rename(path = dir_path) |>
    dplyr::mutate(path = stringr::str_to_upper(path)) |>
    dplyr::inner_join(zs_editors, by = "path")

  # Join back the full permission table
  dribbles_with_permission <- dplyr::left_join(full_permission, dribble_files)

  # Filter out ZS editors
  # necessary to add back the correct ID of the spreadsheet
  dribbles_with_permission <- dribbles_with_permission |>
    dplyr::filter(!email %in% files_for_zs_editors$email)
  # Add back ZS editors
  dribbles_with_permission <- dribbles_with_permission |>
    dplyr::bind_rows(files_for_zs_editors)

  # Add ID of the campaign folder for global users
  global_id <- googledrive::drive_get(campaign_name)$id[1]

  dribbles_with_permission <- dribbles_with_permission |>
    dplyr::mutate(id = ifelse(level == "national", global_id, id))


  # Parallel setting of permissions
  if (nrow(dribbles_with_permission) > 0) {
    cli::cli_process_start("Setting permissions")
    set_permissions_parallel(dribbles_with_permission)
    cli::cli_process_done()
  } else {
    cli::cli_alert("No relevant permissions.")
  }

  invisible()
}

# Private functions ----

#' Set dribble permissions in parallel
#'
#' @description
#' Sets permissions of dribbles in parallel
#'
#' @param dribble_permissions `tibble` A permission table with associated dribbles.
#'
#' @returns `NULL`
#' @keywords internal
#'
set_permissions_parallel <- function(dribble_permissions) {
  doFuture::registerDoFuture()

  if (stringr::str_starts(Sys.getenv("SF_PARTNER"), "posit_workbench")) {
    future::plan(future::multicore)
  } else {
    future::plan(future::multisession)
  }

  options(doFuture.rng.onMisuse = "ignore")
  xs <- 1:nrow(dribble_permissions)

  progressr::handlers("cli")
  progressr::with_progress({
    p <- progressr::progressor(along = xs)
    y <-
      foreach::`%dopar%`(foreach::foreach(
        x = xs,
        .packages = c("googledrive")
      ), {
        p()

        tryCatch(
          {
            purrr::pwalk(dribble_permissions[x, c("email", "id", "role")],
              \(email, id, role) {

                googledrive::with_drive_quiet(
                googledrive::drive_share(googledrive::as_id(id),
                  type = "user",
                  role = role,
                  emailAddress = email))
                },
              .progress = TRUE
            )
          },
          error = \(e) {
            cli::cli_alert_info("Error setting permissions to: ")
            record <- dribble_permissions[x, ]
            cli::cli_alert_info(paste0("Email: ", record$email, " ", "Dribble ID: ", record$id))
            invisible()
          }
        )
      })
  })

  invisible()
}
