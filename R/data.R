#' Template for user permissions data
#'
#' A template dataset showing the expected structure and columns for the user
#' permissions file. Users can download this template to prepare their own
#' permissions file before importing it into the application.
#'
#' @format A tibble with 9 columns
#' \describe{
#'   \item{name}{Full name of the user (character)}
#'   \item{phone}{Phone number of the user (numeric)}
#'   \item{notes}{Optional notes about the user (character)}
#'   \item{email}{Email address of the user (character)}
#'   \item{level}{Access level of the user, e.g. \code{"national"} (character)}
#'   \item{role}{Role of the user, e.g. \code{"writer"} (character)}
#'   \item{province}{Province assigned to the user, \code{NA} if not applicable (logical)}
#'   \item{antenne}{Antenne assigned to the user, \code{NA} if not applicable (logical)}
#'   \item{zone_de_sante}{Health zone assigned to the user, \code{NA} if not applicable (logical)}
#' }
#' @note The expected file format is \code{.csv}.
#' @source Internal rdcAVS package template
"data_perm"


#' Template for geographic data
#'
#' A template dataset showing the expected structure and columns for the
#' geographic data file. Users can download this template to prepare their
#' own geographic file before importing it into the application.
#'
#' @format A tibble with 5 columns:
#' \describe{
#'   \item{provinces}{Name of the province (character)}
#'   \item{antennes}{Name of the antenne (character)}
#'   \item{zones_de_sante}{Name of the health zone (character)}
#'   \item{aires_de_sante}{Name of the health area (character)}
#'   \item{population_totale}{Total population of the health area (numeric)}
#' }
#' @note The expected file format is \code{.csv}.
#' @source Internal rdcAVS package template
"template_data_geographics"