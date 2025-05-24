#' @keywords internal
"_PACKAGE"

## usethis namespace: start

#'
#' @import dplyr stringr readr
#' @import googledrive openxlsx
#' @import shiny shinyFiles
#' @importFrom bslib bs_theme
#' @importFrom cli cli_abort
#' @importFrom cli cli_alert
#' @importFrom cli cli_alert_info
#' @importFrom cli cli_alert_success
#' @importFrom cli cli_alert_warning
#' @importFrom cli cli_li
#' @importFrom cli cli_process_done
#' @importFrom cli cli_process_start
#' @importFrom doFuture registerDoFuture
#' @importFrom DT datatable
#' @importFrom DT DTOutput
#' @importFrom DT renderDT
#' @importFrom foreach %dopar%
#' @importFrom foreach foreach
#' @importFrom fs path_home
#' @importFrom future plan
#' @importFrom lubridate as_date
#' @importFrom progressr handlers
#' @importFrom progressr progressor
#' @importFrom progressr with_progress
#' @importFrom purrr pwalk
#' @importFrom purrr walk
#' @importFrom rappdirs user_data_dir
#' @importFrom shinyjs show
#' @importFrom shinyjs useShinyjs
## usethis namespace: end

utils::globalVariables(c("aires_de_sante", "antenne", "antennes", "debut", "email",
                         "fin", "level", "local_path", "masque_names",
                         "mime_type", "path", "population_totale", "province", "provinces", "role",
                         "x", "zone_de_sante", "zones_de_sante"))

NULL
