#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# UI ----

# Styles


#' Run the rdcAVS application
#'
#' @description
#' The rdcAVS application deploys to the web browser locally. It consists of three
#' tabs that creates a campaign, add geographic information, and set Google Drive
#' permissions. Creating a campaign outputs a folder prefixed with "CAMPAGNE_",
#' which contains a hierarchical structure, going from the largest geographic unit (Province)
#' to the lowest geographic unit (Zone de Sant\u00e9). Within the Zone de Sant\u00e9 folder, contains
#' a template file.
#'
#' @details
#' The campaign creation tab takes valid input based on what is given in the geographic
#' information tab. Likewise, entries in the permissions database also depend on the
#' geographic information tab to ensure consistency, especially when non-global
#' permissions are set.
#'
#' Both permissions and geographic information is stored locally in a user's machine.
#'
#'
#' @returns None.
#' @export
#'
#' @examples
#' \dontrun{
#' campagneApp()
#' }
campagneApp <- function() {
      gui <- page_fluid(
        theme = bslib::bs_theme(
          version = 5,
          bootswatch = "yeti"
        ),
        useShinyjs(),

        # Header
        tags$div(
          style = "display: flex; justify-content: space-between; align-items: center; padding: 10px 20px;",
          tags$div(
            style = "display: flex; gap: 15px;",
            imageOutput("logo", height = "60px"),
          ),
          tags$div(style = "flex-grow: 40;"),
          tags$div(
            style = "display: flex; gap: 15px;",
            imageOutput("drc_cdc_logo", height = "60px"),
          ),
        ),
        tags$hr(),
        fluidRow(
          layout_columns(
            bslib::input_dark_mode(id = "mode"),
            br(), br(), br(), br(),
            actionButton("end_session", label = tagList(icon("sign-out-alt"), "Quitter l'application"), class = "btn btn-danger")
          ),
        ),
        fluidRow(h4("Authentifiez-vous avec Google Drive"),
                 layout_columns(actionButton("auth_drive", "Authentifier", class = "btn-success"),
                                      verbatimTextOutput("auth_status"), col_widths = c(1, 3))),
        navset_tab(
          ui_geodatabase(),
          ui_campaign_creation(),
          ui_perm_table(),
          ui_monitoring()
        ),
        # Footer
        ui_footer()
      )

      # Server ----

      shinyApp(gui, server, options = list(launch.browser = TRUE))
}
