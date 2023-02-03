#' Plot OCR / ECAR profile from a Seahorse analysis
#'
#' @param table tidy table with data comming from the analysis. Columns: time, genotype, mice, value
#' @param genotype_levels vector will all the genotypes all the analysis
#' @param genotype_labels name to be display in the legend. In markdown/html format.
#' @param x_lab X-axis label
#' @param y_lab y-axis label
#' @param title_lab title label
#' @param x_angle Angle to display the x axis
#' @param x_hjust Justificacion of the x axis
#' @param legend_position Legend position. Default to top. Could also be right, left or bottom
#' @param color_values color to be ploted. Same number as levels have genotype . For VHL paper table$VHL_palette_color
#' @param shape_values shape to be ploted. Same number as levels have genotype. For VHL paper table$VHL_palette_shape
#' @param fill_values fill color to be ploted. Same number as levels have genotype. For VHL paper table$VHL_palette_fill
#' @param lty_values line structure. to be ploted. Same number as levels have genotype. For VHL paper table$VHL_palette_lty
#' @param path_output full name of the generated plot including the path
#' (recommended path_output from path_builder())
#' @param w width of the output plot
#' @param h high of the output plot
#' @param print_plot boolean indicating if the plot is printed or not. Default to TRUE.
#'
#' @import here
#' @import tidyverse
#' @import ggthemes
#'
#'
#' @return plot file in data folder
#' @export
#'
#' @examples seahorse_curve(curve, title.i = "GGP Seahorse",
#' color_values = table$VHL_palette_color,
#' shape_values = table$VHL_palette_shape,
#' fill_values = table$VHL_palette_fill,
#' lty_values = table$VHL_palette_lty)
#'
#'
#'
#'
seahorse_curve <-
  function(table,
           genotype_levels = c("WT", "KO"),
           genotype_labels = genotype_levels,
           x_lab = "Time (minutes)",
           y_lab = "Oxygen Consumption Rate<br /> (pmol/min)",
           title_lab = "",
           x_angle = 0,
           x_hjust = 0.5,
           color_values = scales::hue_pal()(200),
           shape_values = rep(21, 200),
           fill_values = scales::hue_pal()(200),
           lty_values = rep("solid", 200),
           leyend_position = "top",
           path_output = "plot.png",
           w = 10,
           h = 5,
           save_plot = FALSE,
           print_plot = FALSE) {
    # used_genotypes <- c(unique(table$genotype))
    p <- table %>%
      mutate(genotype = factor(genotype,
                               levels = genotype_levels,
                               labels = genotype_labels)) %>%
      group_by(genotype, time, experiment) %>%
      dplyr::summarise(
        mean = mean(value),
        se   = sd(value) / sqrt(n()),
        .groups = "drop"
      ) %>%
      ggplot(aes(
        x = time,
        y = mean,
        shape = genotype,
        color = genotype,
        fill = genotype,
        lty = genotype
      )) +
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 7) +
      geom_line(size = 1) +
      geom_point(size = 2,
                 stroke = 0.5) +
      labs(x = x_lab, y = y_lab, title = title_lab) +
      scale_x_continuous() +
      theme_clean(base_family = "sans", base_size = 11) +
      theme(
        legend.position = leyend_position,
        legend.background = element_rect(colour = "transparent",
                                         fill = "transparent"),
        panel.grid.major.y = element_blank(),
        legend.title = element_markdown(face = "plain", size = 9),
        legend.text = element_markdown(size = 9),
        axis.text.x = element_markdown(angle = x_angle, hjust = x_hjust),
        plot.title = element_markdown(face = "plain", size = 10),
        plot.background = element_rect(colour = NA,
                                       fill = "transparent"),
        # strip.text.x = element_blank(),
        axis.title.y = element_markdown(),
        axis.title.x = element_markdown()
      ) +
      scale_shape_manual(values = shape_values,
                         drop = FALSE) +
      scale_color_manual(drop = FALSE,
                         values = color_values,) +
      scale_fill_manual(values = fill_values,
                        drop = FALSE) +
      scale_linetype_manual(values = lty_values,
                            drop = FALSE) +
      labs(
        shape = " ",
        fill = " ",
        lty = " ",
        color = " "
      )
    if (!is.null(path_output)) {
      ggsave(
        file = path_output,
        width = w,
        height = h,
        bg = "transparent"
      )
    }
    if (print_plot == TRUE)
      plot(p)
    return(p)
  }
