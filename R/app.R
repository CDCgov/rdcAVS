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
         tags$head(
           tags$style(HTML("    
              .notification-overlay {
               position: fixed;
               top: 0;
              left: 0;
              width: 100%;
              height: 100%;
              background-color: rgba(0, 0, 0, 0.5);
              z-index: 99998;
              display: none;
              opacity: 0;
              transition: opacity 0.3s ease-in-out;
            }
      
              .notification-overlay.show {
                display: block;
                opacity: 1;
            }
      
              #shiny-notification-panel {
                position: fixed;
                top: 50% !important;
                left: 50% !important;
                right: auto !important;
                bottom: auto !important;
                transform: translate(-50%, -50%) !important;
                width: auto;
                max-width: 500px;
                z-index: 99999;
            }
      
          @keyframes slideDown {
           from {
             opacity: 0;
             transform: translate(-50%, -60%);
            }
           to {
             opacity: 1;
             transform: translate(-50%, -50%);
           }
         }
      
           @keyframes pulse {
             0%, 100% {
              transform: scale(1);
            }
             50% {
          transform: scale(1.05);
             }
          }
      
             .shiny-notification {
                  background-color: white;
                  border: none;
                  border-radius: 12px;
                  box-shadow: 0 8px 32px rgba(0, 0, 0, 0.2);
                  padding: 40px 30px 30px 30px;
                  text-align: center;
                  font-size: 16px;
                  color: #333;
                   opacity: 1 !important;
                   min-width: 350px;
                  position: relative;
                  animation: slideDown 0.4s ease-out;
              }

              .shiny-notification::before {
                  content: '';
                  position: absolute;
                  top: 0;
                  left: 0;
                  right: 0;
                  height: 4px;
                  border-radius: 12px 12px 0 0;
                  background: linear-gradient(90deg, #3498db, #2980b9);
              }
      
              .shiny-notification-message::before {
                 background: linear-gradient(90deg, #3498db, #2980b9);
                }
      
              .shiny-notification-warning::before {
                 background: linear-gradient(90deg, #f39c12, #e67e22);
                }
      
              .shiny-notification-error::before {
                 background: linear-gradient(90deg, #e74c3c, #c0392b);
                }
      
             .shiny-notification.success-notification::before {
                background: linear-gradient(90deg, #27ae60, #229954);
              }
      
              .shiny-notification-message,
              .shiny-notification-warning,
              .shiny-notification-error {
                 background-color: white;
                 border-left: none;
              }
      
            .shiny-notification .message-text {
               color: #555;
               line-height: 1.6;
               margin-top: 8px;
               font-size: 15px;
            }"
          )
        )
      ),
        useShinyjs(),
        bslib::input_dark_mode(id = "mode"),
        downloadButton("download_template", label = "", style = "visibility:hidden;"),
        downloadButton("download_geo_ok", label = "", style = "visibility:hidden;"),
        downloadButton("download_template_permission", label = "", style = "visibility:hidden;"),
        downloadButton("download_permissions", label = "", style = "visibility:hidden;"),
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
          style = "display:flex; align-items:center; gap:10px; flex-wrap:nowrap;",   
          imageOutput("drc_cdc_logo", height = "60px"),  
            h5(
          "Portail de création des masques de saisie des campagnes de vaccination en RDC",
           style = "
            margin:0;
            font-weight:500;
            white-space:nowrap;
          "
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
      ),
    icon_btn("end_session", "SignOut", "Quitter l'application", type = "danger"),
    )
  ),
  br(),
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
