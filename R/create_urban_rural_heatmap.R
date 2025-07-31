create_urban_rural_heatmap <- function(summary) {

  plot_data <- summary |>
    mutate(aire_de_sante = str_to_title(aire_de_sante),
           aire_de_sante = factor(aire_de_sante, levels = c(sort(unique(aire_de_sante), decreasing = TRUE))),
           cat_rural = case_when(
             between(avg_vax_rural, 1, 99) ~ "Objectif Non Atteint",
             avg_vax_rural >= 100 ~ "Bien",
             .default = "Zéro"
           ),
           cat_rural = factor(cat_rural, levels = c("Bien", "Objectif Non Atteint", "Zéro")),
           cat_urban = case_when(
             between(avg_vax_urban, 1, 199) ~ "Objectif Non Atteint",
             avg_vax_urban >= 200 ~ "Bien",
             .default = "Zéro"
           ),
           cat_urban = factor(cat_urban, levels = c("Bien", "Objectif Non Atteint", "Zéro")),
           avg_vax_rural = round(avg_vax_rural),
           avg_vax_urban = round(avg_vax_urban)
    ) |>
    mutate(across(any_of(c("avg_vax_urban", "avg_vax_rural")), \(x) ifelse(is.na(x), 0, x)))

  rural_plot <- ggplot2::ggplot(data = plot_data) +
    ggplot2::geom_tile(ggplot2::aes(y =aire_de_sante, x = jour, fill = cat_rural), show.legend = TRUE) +
    ggplot2::scale_fill_manual(values = c("Zéro" = "lightgrey", "Bien" = "#006B3E", "Objectif Non Atteint" = "#ED2938"),
                               name = "Statut") +
    ggplot2::geom_text(ggplot2::aes(y = aire_de_sante, x = jour, label = avg_vax_rural), size = 8) +
    ggplot2::theme_void() +
    ggplot2::ylab("Aire De Sante") +
    ggplot2::xlab("") +
    ggplot2::labs(title = "Rural ref 100 enfants") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14),
                   axis.text.y = ggplot2::element_text(size = 14),
                   axis.title.x = ggplot2::element_text(size = 14, face = "bold"),
                   axis.title.y = ggplot2::element_text(size = 14, face = "bold", angle = 90),
                   title = ggplot2::element_text(size = 18, face = "bold"),
                   legend.text = ggplot2::element_text(size = 16),
                   panel.background = ggplot2::element_rect(fill = "transparent", color = NA_character_)
    )

  urban_plot <- ggplot2::ggplot(data = plot_data) +
    ggplot2::geom_tile(ggplot2::aes(y =aire_de_sante, x = jour, fill = cat_urban), show.legend = TRUE) +
    ggplot2::scale_fill_manual(values = c("Zéro" = "lightgrey", "Bien" = "#006B3E", "Objectif Non Atteint" = "#ED2938"),
                               name = "Statut") +
    ggplot2::geom_text(ggplot2::aes(y = aire_de_sante, x = jour, label = avg_vax_urban), size = 8) +
    ggplot2::theme_void() +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::labs(title = "Urban ref 200 enfants") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14),
                   axis.text.y = ggplot2::element_text(size = 14),
                   axis.title.x = ggplot2::element_text(size = 14, face = "bold"),
                   axis.title.y = ggplot2::element_text(size = 14, face = "bold", angle = 90),
                   title = ggplot2::element_text(size = 18, face = "bold"),
                   legend.text = ggplot2::element_text(size = 16),
                   panel.background = ggplot2::element_rect(fill = "transparent", color = NA_character_)
    )

  plot <- ggarrange(rural_plot, urban_plot, common.legend = T, legend = "bottom")

  return(plot)
}
