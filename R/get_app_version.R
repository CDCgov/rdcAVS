#' Get the version of the application
#'
#' @returns `str` Version number.
#' @keywords internal
#'
get_app_version <- function() {
  version <- read.dcf(file = system.file("DESCRIPTION", package = "rdcAVS"),
                      fields = "Version")
  return(version[1, "Version"])
}
