#' @keywords internal
"_PACKAGE"

## usethis namespace: start

#'
#' @import bsicons
#' @import bslib
#' @import dplyr stringr readr
#' @import googledrive
#' @import shiny shinyFiles
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
#' @importFrom fs path_dir
#' @importFrom fs path_home
#' @importFrom future plan
#' @importFrom ggpubr ggarrange
#' @importFrom googlesheets4 range_write
#' @importFrom httr add_headers
#' @importFrom httr POST
#' @importFrom lubridate as_date
#' @importFrom progressr handlers
#' @importFrom progressr progressor
#' @importFrom progressr with_progress
#' @importFrom purrr pwalk
#' @importFrom purrr walk
#' @importFrom rappdirs user_data_dir
#' @importFrom readr type_convert
#' @importFrom shinyjs disable
#' @importFrom shinyjs show toggle click
#' @importFrom shinyjs useShinyjs
#' @importFrom tidyr pivot_longer
#' @importFrom shiny.fluent Stack MessageBar updateDropdown.shinyInput Dropdown.shinyInput DefaultButton.shinyInput IconButton.shinyInput TooltipHost DatePicker.shinyInput Text TextField.shinyInput Separator PrimaryButton.shinyInput 
## usethis namespace: end

utils::globalVariables(c(
  "aires_de_sante", "antenne", "antennes", "debut", "email",
  "fin", "level", "local_path", "masque_names",
  "mime_type", "path", "population_totale", "province", "provinces", "role",
  "x", "zone_de_sante", "zones_de_sante"
))

NULL
