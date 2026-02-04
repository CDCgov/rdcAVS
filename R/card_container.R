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
