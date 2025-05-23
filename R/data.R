#' Geographic and population database
#'
#' The database containing all the geographic names in DRC, including total populations,
#' at the Aires de Sante level. This database is used primarily as the source for filling
#' the Excel template, as well as the various drop-down menus in the app.
#'
#' @format ## `geo_data`
#' A data frame with five columns:
#' \describe{
#'   \item{provinces}{Country name}
#'   \item{antennes}{Antenne}
#'   \item{zones_de_sante}{Zones de Sante}
#'   \item{aires_de_sante}{Aires de Sante}
#'   \item{population_totale}{Total population of aires de sante}
#' }
"geo_data"

#' Permissions database
#'
#' The database containing all the permissions required to be set in the Google Drive
#' campaign folder.
#'
#' @format ## `perm_data`
#' A data frame with seven columns:
#' \describe{
#'   \item{name}{Name of the person}
#'   \item{email}{Email address, preferably a gmail email.}
#'   \item{level}{Either "global", "province", "antenne", or "zone de sante"}
#'   \item{role}{Currently supported are: "reader", "writer", "commenter".
#'   Reader has read-only access. Writer has edit access. Commenter can only comment.}
#'   \item{province}{Total population of aires de sante}
#'   \item{antenne}{Antenne folder to grant permission to}
#'   \item{zone_de_sante}{Zone de Sante folder to grant permissions to}
#' }
"perm_data"
