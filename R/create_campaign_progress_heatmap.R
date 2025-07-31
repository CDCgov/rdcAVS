#' Creates a heatmap showing campaign progress
#'
#' @param summary `tibble` Summary table output of [get_campaign_progress()].
#'
#' @returns `ggplot2` ggplot object.
#' @export
#'
#' @examples
#' \dontrun{
#' ss_dribble <- googledrive::drive_get("spreadsheet_url")
#' summary <- get_campaign_progress(ss_dribble)
#' create_campaign_progress_heatmap(summary)
#' }
create_campaign_progress_heatmap <- function(summary) {
  ggplot2::ggplot(data = summary |>
                    mutate(aire_de_sante = str_to_title(aire_de_sante),
                           aire_de_sante = factor(aire_de_sante, levels = c(sort(unique(aire_de_sante), decreasing = TRUE))),
                           cat = case_when(
                             couverture_campaign_cumulative >= 40 & jour == "Jour 1" ~ "Bien",
                             couverture_campaign_cumulative >= 35 & couverture_campaign_cumulative < 40 & jour == "Jour 1" ~ "Risque",
                             couverture_campaign_cumulative >= 70 & jour == "Jour 2" ~ "Bien",
                             couverture_campaign_cumulative >= 65 & couverture_campaign_cumulative < 70 & jour == "Jour 2" ~ "Risque",
                             couverture_campaign_cumulative >= 100 & jour == "Jour 3" ~ "Bien",
                             couverture_campaign_cumulative >= 95 & couverture_campaign_cumulative < 100 & jour == "Jour 3" ~ "Risque",
                             couverture_campaign_cumulative >= 100 & jour == "Jour 4" ~ "Bien",
                             couverture_campaign_cumulative >= 95 & couverture_campaign_cumulative < 100 & jour == "Jour 4" ~ "Risque",
                             .default = "Objectif Non Atteint"
                           ),
                           cat = factor(cat, levels = c("Bien", "Risque", "Objectif Non Atteint")))) +
    ggplot2::geom_tile(ggplot2::aes(y =aire_de_sante, x = jour, fill = cat), show.legend = TRUE) +
    ggplot2::scale_fill_manual(values = c("Risque" = "#FFAA1C", "Bien" = "#006B3E", "Objectif Non Atteint" = "#ED2938"),
                               name = "Statut") +
    ggplot2::geom_text(ggplot2::aes(y = aire_de_sante, x = jour, label = couverture_campaign_cumulative),
                       size = 8) +
    ggplot2::theme_void() +
    ggplot2::ylab("Aire De Sante") +
    ggplot2::xlab("") +
    ggplot2::labs(subtitle = paste0("Objectif: Jour 1: 40%, Jour 2: 70%, Jour 3: 100%, Jour 4: 100%\n",
                                    "A risque si à moins de 5 % et en échec si à plus de 5 % de l'objectif")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14),
                   axis.text.y = ggplot2::element_text(size = 14),
                   axis.title.x = ggplot2::element_text(size = 14, face = "bold"),
                   axis.title.y = ggplot2::element_text(size = 14, face = "bold", angle = 90),
                   title = ggplot2::element_text(size = 18, face = "bold"),
                   legend.text = ggplot2::element_text(size = 16),
                   panel.background = ggplot2::element_rect(fill = "transparent", color = NA_character_)
                   )
}
