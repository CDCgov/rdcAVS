create_recovery_heatmap <- function(summary) {
  plot_data <- summary |>
    mutate(aire_de_sante = str_to_title(aire_de_sante),
           aire_de_sante = factor(aire_de_sante, levels = c(sort(unique(aire_de_sante), decreasing = TRUE))),
           cat_0_11 = case_when(
             recovery_0_11_cumulative > 0 ~ "Bien",
             .default = "Zéro"
           ),
           cat_12_23 = case_when(
             recovery_12_23_cumulative > 0 ~ "Bien",
             .default = "Zéro"

           ),
           cat_24_59 = case_when(
             recovery_24_59_cumulative > 0 ~ "Bien",
             .default = "Zéro"
           )
    ) |>
    dplyr::mutate(across(all_of(c("cat_0_11", "cat_12_23", "cat_24_59")),
                         \(x) factor(x, levels = c("Bien", "Zéro"))))

  plot_0_11 <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_tile(ggplot2::aes(y =aire_de_sante, x = jour, fill = cat_0_11), show.legend = TRUE) +
    ggplot2::scale_fill_manual(values = c("Zéro" = "lightgrey", "Bien" = "#006B3E"),
                               name = "Statut") +
    ggplot2::geom_text(ggplot2::aes(y = aire_de_sante, x = jour, label = recovery_0_11_cumulative), size = 8) +
    ggplot2::theme_void() +
    ggplot2::ylab("Aire De Sante") +
    ggplot2::xlab("") +
    ggplot2::labs(title = "0-11 mois") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14),
                   axis.text.y = ggplot2::element_text(size = 14),
                   axis.title.x = ggplot2::element_text(size = 14, face = "bold"),
                   axis.title.y = ggplot2::element_text(size = 14, face = "bold", angle = 90),
                   title = ggplot2::element_text(size = 18, face = "bold"),
                   legend.text = ggplot2::element_text(size = 16),
                   panel.background = ggplot2::element_rect(fill = "transparent", color = NA_character_)
    )

  plot_12_23 <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_tile(ggplot2::aes(y =aire_de_sante, x = jour, fill = cat_12_23), show.legend = TRUE) +
    ggplot2::scale_fill_manual(values = c("Zéro" = "lightgrey", "Bien" = "#006B3E"),
                               name = "Statut") +
    ggplot2::geom_text(ggplot2::aes(y = aire_de_sante, x = jour, label = recovery_12_23_cumulative), size = 8) +
    ggplot2::theme_void() +
    ggplot2::ylab("") +
    ggplot2::xlab("") +
    ggplot2::labs(title = "12-23 mois") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14),
                   axis.text.y = ggplot2::element_text(size = 14),
                   axis.title.x = ggplot2::element_text(size = 14, face = "bold"),
                   axis.title.y = ggplot2::element_text(size = 14, face = "bold", angle = 90),
                   title = ggplot2::element_text(size = 18, face = "bold"),
                   legend.text = ggplot2::element_text(size = 16),
                   panel.background = ggplot2::element_rect(fill = "transparent", color = NA_character_)
    )

  plot_24_59 <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_tile(ggplot2::aes(y =aire_de_sante, x = jour, fill = cat_24_59), show.legend = TRUE) +
    ggplot2::scale_fill_manual(values = c("Zéro" = "lightgrey", "Bien" = "#006B3E"),
                               name = "Statut") +
    ggplot2::geom_text(ggplot2::aes(y = aire_de_sante, x = jour, label = recovery_24_59_cumulative), size = 8) +
    ggplot2::theme_void() +
    ggplot2::ylab("") +
    ggplot2::xlab("") +
    ggplot2::labs(title = "24-59 mois") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14),
                   axis.text.y = ggplot2::element_text(size = 14),
                   axis.title.x = ggplot2::element_text(size = 14, face = "bold"),
                   axis.title.y = ggplot2::element_text(size = 14, face = "bold", angle = 90),
                   title = ggplot2::element_text(size = 18, face = "bold"),
                   legend.text = ggplot2::element_text(size = 16),
                   panel.background = ggplot2::element_rect(fill = "transparent", color = NA_character_)
    )

  plot <- ggpubr::ggarrange(plot_0_11, plot_12_23, plot_24_59, nrow = 1,
                            common.legend = TRUE,legend = "bottom")

  return(plot)

}
