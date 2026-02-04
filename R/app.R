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
        bslib::input_dark_mode(id = "mode"),
        downloadButton("download_template", label = "", style = "visibility:hidden;"),
         ui_authenticate(),
      div(id = "main-app", style = "display:none;",
           fluidPage(
            title = "rdcAVS",
              div(
                style = "
               display:flex;
               justify-content:space-between;
               align-items:center;
               padding:15px 25px;
               background-color:#f8f9fa;
               border-bottom:2px solid #dee2e6;",    
    div(
      style = "display:flex; align-items:center; gap:15px;",      
      tags$img(
        src = "drc_cdc_logo.svg",
        style = "height:70px; width:auto;",
        alt = "Logo"
      ),
            h4(
        "Portail de création des masques de saisie pour les campagnes de vaccination en RDC",
        style = "margin:0; font-weight:600;"
      )
    ),    
    div(
      style = "display:flex; align-items:center; gap:8px;",
      tags$svg(
        xmlns = "http://www.w3.org/2000/svg",
        width = "24",
        height = "24",
        viewBox = "0 0 24 24",
        fill = "none",
        stroke = "#0d6efd",
        `stroke-width` = "2",
        `stroke-linecap` = "round",
        `stroke-linejoin` = "round",
        tags$path(d = "M20 21v-2a4 4 0 0 0-4-4H8a4 4 0 0 0-4 4v2"),
        tags$circle(cx = "12", cy = "7", r = "4")
      ),
      span(
         uiOutput("welcome_message"), 
        style = "font-size:18px; font-weight:500; color:#0d6efd;"
      )
    )
  ),br(),
   navset_tab(
          ui_geodatabase(),
          ui_campaign_creation(),
          ui_perm_table(),
          ui_monitoring()
        ),
      # Footer
        ui_footer()
  )
 )        
)

      # Server ----

      shinyApp(gui, server, options = list(launch.browser = TRUE))
}
