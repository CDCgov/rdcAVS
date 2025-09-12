#' Checks whether there is a new version of the app on GitHub
#'
#' @param local_version `str` Local version number
#'
#' @returns `str` Message if new version is available otherwise NULL.
#' @keywords internal
#'
check_new_version <- function(local_version = get_app_version()) {
  # Replace with your actual GitHub raw URL
  remote_desc_url <- "https://raw.githubusercontent.com/mcuadera/rdcAVS/refs/heads/master/DESCRIPTION"

  remote_desc <- tryCatch({
    read.dcf(url(remote_desc_url), fields = "Version")
  }, error = \(e) {
    cli::cli_alert_warning("Invalid url")
    NULL
  } )

  if (!is.null(remote_desc)) {
    remote_version <- remote_desc[1, "Version"]
    if (package_version(remote_version) > package_version(local_version)) {
      return(paste0("Une nouvelle version (v.", remote_version, ") de rdcAVS est disponible !"))
    } else {
      cli::cli_alert_success("App is up to date!")
    }
  }
  return(NULL)
}
