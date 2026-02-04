#' Fluent-style Card Container
#'
#' Creates a simple card container with Fluent UI
#'
#' @param ... UI elements to include inside the card.
#'
#' @return A \code{shiny.tag} \code{div} containing the wrapped content.
#' @export

fluent_card <- function(...) {
  div(
    style = "
      background:#ffffff;
      border:1px solid #edebe9;
      border-radius:14px;
      padding:16px;
      box-shadow:0 4px 14px rgba(0,0,0,0.06);
    ",
    ...
  )
}


#' Icon Button with Tooltip (Fluent Style)
#'
#' Creates a circular icon button with a tooltip.
#' The button color scheme changes depending on the selected \code{type}.
#'
#' @param id Input ID for the button.
#' @param icon Name of the icon to display.
#' @param tooltip Text shown when hovering over the button.
#' @param type Visual color of the button. One of \code{"default"},
#'   \code{"primary"}, \code{"success"}, or \code{"danger"}.
#'
#' @return A \code{shiny.tag}.
#' @export

icon_btn <- function(id, icon, tooltip, type = "default") {
  
  bg_color <- switch(type,
                     danger  = "#fde7e9",
                     primary = "#e5f1fb",
                     success = "#e6f4ea",
                     default = "#f3f2f1"
  )
  
  icon_color <- switch(type,
                       danger  = "#a80000",
                       primary = "#0f6cbd",
                       success = "#107c10",
                       default = "#323130"
  )  
  TooltipHost(
    content = tooltip,
    delay = 0,
    
    IconButton.shinyInput(
      inputId = id,
      iconProps = list(
        iconName = icon,
        styles = list(
          root = list(
            fontSize = 30
          )
        )
      ),
      styles = list(
        root = list(
          width = "44px",
          height = "44px",
          borderRadius = "50%",
          backgroundColor = bg_color,
          selectors = list(
            ":hover" = list(
              backgroundColor = "#edebe9"
            )
          )
        ),
        icon = list(
          color = icon_color
        )
      ),
      ariaLabel = tooltip
    )
  )
}
