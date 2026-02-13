
ui_monitoring <- function() {  
   nav_panel(
        "Surveillance",
        tags$head(
          tags$style(HTML("
          .fluent-page-container {
            padding: 24px;
            max-width: 1400px;
          }
          
          .fluent-accordion {
            margin-top: 24px;
          }
          
          .fluent-accordion-item {
            background: white;
            border: 1px solid #e1e4e8;
            border-radius: 8px;
            margin-bottom: 12px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.04);
            transition: box-shadow 0.2s ease;
          }
          
          .fluent-accordion-item:hover {
            box-shadow: 0 4px 12px rgba(0,0,0,0.08);
          }
          
          .fluent-accordion-header {
            width: 100%;
            padding: 16px 20px;
            background: transparent;
            border: none;
            cursor: pointer;
            display: flex;
            align-items: center;
            justify-content: space-between;
            text-align: left;
            transition: background-color 0.2s ease;
          }
          
          .fluent-accordion-header:hover {
            background-color: #f6f8fa;
          }
          
          .fluent-accordion-title-section {
            display: flex;
            align-items: center;
            gap: 12px;
          }
          
          .fluent-accordion-icon {
            color: #0078d4;
            font-size: 18px;
          }
          
          .fluent-accordion-title {
            font-size: 15px;
            font-weight: 600;
            color: #201f1e;
            margin: 0;
          }
          
          .fluent-accordion-chevron {
            color: #605e5c;
            transition: transform 0.2s ease;
            font-size: 12px;
          }
          
          .fluent-accordion-item.is-open .fluent-accordion-chevron {
            transform: rotate(180deg);
          }
          
         .fluent-accordion-content {
           max-height:0;
           overflow: hidden;
           padding: 0 20px;
           opacity: 0;
           transition: max-height 0.3s ease, opacity 0.2s ease, padding 0.2s ease;         
         }
          .fluent-accordion-item.is-open .fluent-accordion-content {
            max-height: 5000px;
            opacity: 1;
            padding: 0 20px 20px 20px;
          }
          
          @keyframes slideDown {
            from {
              opacity: 0;
              transform: translateY(-10px);
            }
            to {
              opacity: 1;
              transform: translateY(0);
            }
          }
          
          /* Fluent button styling */
          .fluent-download-btn {
            margin-top: 12px;
          }
          
          /* Card styling for plots */
          .plot-card {
            background: white;
            border: 1px solid #e1e4e8;
            border-radius: 8px;
            padding: 20px;
            margin-top: 16px;
          }
          
          /* Selector styling */
          .selector-group {
            display: flex;
            gap: 16px;
            margin-bottom: 20px;
            flex-wrap: wrap;
          }
          
          .selector-group > div {
            flex: 1;
            min-width: 200px;
          }
        "))
        ),
        
        tags$div(
          class = "fluent-page-container",         
          
          shiny.fluent::Stack(
            tokens = list(childrenGap = 8),
            shiny.fluent::Text(
              variant = "xLarge",
              "Surveillance de Campagne",
              style = list(fontWeight = "600", color = "#201f1e")
            ),
            uiOutput("campaign_surveillance"),
            fluidRow(
            column(
              width = 6,
              align = "left",
            shiny.fluent::PrimaryButton.shinyInput(
              inputId = "compile_campaign_btn",
              text = "Compiler des masques",
              iconProps = list(iconName = "CompactDisc")
            )
          )
        ),
         shiny.fluent::Stack(
              horizontal = TRUE,
              tokens = list(childrenGap = 20),
              uiOutput("campaign_template_url"),
              verbatimTextOutput("refresh_date")
            )
          ),
          
          tags$div(
            class = "fluent-accordion",
            tags$div(
              class = "fluent-accordion-item",
              `data-accordion-id` = "completeness",
              tags$button(
                class = "fluent-accordion-header",
                onclick = "toggleAccordion(this)",
                
                tags$div(
                  class = "fluent-accordion-title-section",
                  tags$span(
                    class = "fluent-accordion-icon",
                    bsicons::bs_icon("card-checklist")
                  ),
                  tags$h3(
                    class = "fluent-accordion-title",
                    "Informations sur la complétude"
                  )
                ),
                tags$span(
                  class = "fluent-accordion-chevron",
                  "▼"
                )
              ),
              tags$div(
                class = "fluent-accordion-content",
                tags$div(
                  class = "fluent-download-btn",
                  icon_btn("click_download_data_quality_monitoring", "Download", "Télécharger", type = "success")
                ),
                DT::DTOutput("campaign_info_table")                
              )
            ),
            tags$div(
              class = "fluent-accordion-item",
              `data-accordion-id` = "progress",
              
              tags$button(
                class = "fluent-accordion-header",
                onclick = "toggleAccordion(this)",
                
                tags$div(
                  class = "fluent-accordion-title-section",
                  tags$span(
                    class = "fluent-accordion-icon",
                    bsicons::bs_icon("activity")
                  ),
                  tags$h3(
                    class = "fluent-accordion-title",
                    "Rapport de progres de la campagne"
                  )
                ),
                
                tags$span(
                  class = "fluent-accordion-chevron",
                  "▼"
                )
              ),
              
              tags$div(
                class = "fluent-accordion-content",                
                tags$div(
                  class = "fluent-download-btn",
                  icon_btn("click_download_campaign_quality_monitoring", "Download", "Télécharger", type = "success")
                ),
                DT::DTOutput("campaign_progress_table")                
              )
            ),
            tags$div(
              class = "fluent-accordion-item is-open",
              `data-accordion-id` = "charts",
              
              tags$button(
                class = "fluent-accordion-header",
                onclick = "toggleAccordion(this)",
                
                tags$div(
                  class = "fluent-accordion-title-section",
                  tags$span(
                    class = "fluent-accordion-icon",
                    bsicons::bs_icon("bar-chart")
                  ),
                  tags$h3(
                    class = "fluent-accordion-title",
                    "Graphiques"
                  )
                ),
                
                tags$span(
                  class = "fluent-accordion-chevron",
                  "▼"
                )
              ),
              
              tags$div(
                class = "fluent-accordion-content",
                tags$div(
                  class = "selector-group",
                  shiny.fluent::Dropdown.shinyInput(
                    inputId = "prov_selector_campaign_completeness",
                    label = "Sélectionner une province:",
                    options = list()
                  ),
                  shiny.fluent::Dropdown.shinyInput(
                    inputId = "ant_selector_campaign_completeness",
                    label = "Sélectionner une antenne:",
                    options = list()
                  ),
                  shiny.fluent::Dropdown.shinyInput(
                    inputId = "zs_selector_campaign_completeness",
                    label = "Sélectionner une zone de sante:",
                    options = list()
                  )
                ),
                tags$div(
                  class = "plot-card",
                  shiny.fluent::Pivot(
                    shiny.fluent::PivotItem(
                      headerText = "Couverture Campagne (Cumulative)",
                      plotOutput("campaign_completeness_plot", height = "700px")
                    ),
                    shiny.fluent::PivotItem(
                      headerText = "Couverture Campagne (Jour)",
                      plotOutput("campaign_completeness_plot_daily", height = "700px")
                    ),
                    shiny.fluent::PivotItem(
                      headerText = "Nb moyen d'enfants vaccinés/équipe",
                      plotOutput("campaign_urban_rural_plot", height = "700px")
                    ),
                    shiny.fluent::PivotItem(
                      headerText = "Récupérations",
                      plotOutput("campaign_recovery_plot", height = "700px")
                    )
                  )
                )
              )
            )
          )
        ),        
        tags$script(HTML("
             function toggleAccordion(button) {
              const item = button.closest('.fluent-accordion-item');
              const allItems = document.querySelectorAll('.fluent-accordion-item');  
                  allItems.forEach(otherItem => {
                            if (otherItem !== item) {
                            otherItem.classList.remove('is-open');
                          }
                   });
  
                 item.classList.toggle('is-open');
                setTimeout(function() {
                  $(item).find('.dataTable').each(function() {
                  $(this).DataTable().columns.adjust().draw();
                });
              }, 100);
           }
              $(document).on('shiny:idle', function() {
                 setTimeout(function() {
                 window.dispatchEvent(new Event('resize'));
                }, 500);
              });
            function toggleAccordion(button) {
              const item = button.closest('.fluent-accordion-item');
              const allItems = document.querySelectorAll('.fluent-accordion-item');
  
                allItems.forEach(otherItem => {
                  if (otherItem !== item) {
                    otherItem.classList.remove('is-open');
                  }
              });  
          item.classList.toggle('is-open');  
          setTimeout(function() {
          $(item).find('.dataTable').each(function() {
            if ($.fn.DataTable.isDataTable(this)) {
            $(this).DataTable().columns.adjust().draw();
          }
        });
    Shiny.bindAll(item);
      }, 300);
      }
          "
          )
      )
    )
}
