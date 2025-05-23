#Public functions ----

#' Sets the permissions across dribbles based on permissions table
#'
#' @description
#' Sets the permissions of a dribble based on the permissions table. If more
#' than one record is passed unto the `dribble`, all records will have the same
#' permissions as specified by the `permissions_table`.
#'
#' @param dribble `dribble` A dribble object to assign permissions to.
#' @param permissions_table `tibble` A tibble with permissions info. Required
#' columns are: `emailAddress` and `role`.
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
#' \dontrun{}
set_permissions <- function(campaign_name,
                            permissions_table,
                            dribble_files) {
  # Define the drive_path_builder function
  drive_path_builder <- function(level,
                                 campaign_name,
                                 province = NA,
                                 antenne = NA,
                                 zone_de_sante = NA) {
    switch(
      level,
      "global" = paste0(campaign_name, "/"),
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
    )  # Return NA if level doesn't match any case
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
    dplyr::select(-dplyr::all_of(c(
      "name", "province", "antenne", "zone_de_sante"
    ))) |>
    dplyr::filter(!is.na(path))

  cli::cli_process_start("Fetching relevant dribbles")
  dribbles_filtered <- dribble_files |>
    filter(
      stringr::str_starts(path, paste0("~/", campaign_name)) |
        stringr::str_starts(path, campaign_name),
      mime_type == "application/vnd.google-apps.folder"
    ) |>
    mutate(path = stringr::str_replace_all(path, "~/", ""))
  cli::cli_process_done()

  # Join back the full permission table
  dribbles_with_permission <- dplyr::left_join(full_permission, dribbles_filtered)

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

        tryCatch({
          purrr::pwalk(dribble_permissions[x, c("email", "id", "role")],
                       \(email, id, role) googledrive::with_drive_quiet(
                         googledrive::drive_share(googledrive::as_id(id),
                                                  type = "user",
                                                  role = role,
                                                  emailAddress = email)
                       ),
                       .progress = TRUE)
        }, error = \(e) {
          cli::cli_alert_info("Error setting permissions to: ")
          record <- dribble_permissions[x, ]
          cli::cli_alert_info(paste0("Email: ", record$email," ", "Dribble ID: ", record$id))
          invisible()
        })

      }
      )
  }
  )

  invisible()

}
