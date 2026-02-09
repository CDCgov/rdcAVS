ui_footer <- function() {

  tags$footer(
    style = "
      text-align: center;
      padding: 14px;
      font-size: 0.9em;
      color: #6c757d;
      border-top: 1px solid #ddd;
      margin-top: 40px;
      display: flex;
      flex-direction: column;
      align-items: center;
      gap: 6px;
    ",
    imageOutput("package_logo",height = "20px"),br(),
    tags$div(
      sprintf(
        "Developed by the CDC Polio Eradication Branch Surveillance, Innovation, and Research Team (2025)"
      )
    ),

    # App version
    tags$div(
      style = "font-size: 0.8em; color: #999;",
      sprintf("App version: %s", get_app_version())
    )
  )
}
