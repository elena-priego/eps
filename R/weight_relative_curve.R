#' Plot of relative weight by mice
#'
#' @param table tidy table with data comming from the analysis. Columns: time, mice, genotype, value, experiment
#' @param genotype_levels vector will all the genotypes all the analysis
#' @param x_lab X-axis label
#' @param y_lab y-axis label
#' @param title_lab title label
#' @param x_angle Angle to display the x axis
#' @param x_hjust Justificacion of the x axis
#' @param color_values color to be ploted. Same number as levels have genotype . For VHL paper table$VHL_palette_color
#' @param shape_values shape to be ploted. Same number as levels have genotype. For VHL paper table$VHL_palette_shape
#' @param fill_values fill color to be ploted. Same number as levels have genotype. For VHL paper table$VHL_palette_fill
#' @param lty_values line structure. to be ploted. Same number as levels have genotype. For VHL paper table$VHL_palette_lty
#' @param path_output ful name of the generated plot including the path
#' (recommended path_output from path_builder())
#' @param w width of the output plot
#' @param h high of the output plot
#' @param save_plot boolean indicating if the plot is saved or not. Default to FALSE.
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
#' @examples weight_relative_curve(table, title_lab = "GGP Seahorse",
#' color_values = table$VHL_palette_color,
#' shape_values = table$VHL_palette_shape,
#' fill_values = table$VHL_palette_fill,
#' lty_values = table$VHL_palette_lty)
#'
#'
#'
#'
weight_relative_curve <-
  function(table,
           genotype_levels = c("WT", "KO"),
           x_lab = "Time (days)",
           y_lab = "Relative weight",
           title_lab = "",
           x_angle = 0,
           x_hjust = 0.5,
           color_values = hue_pal()(200),
           shape_values = rep(21,200),
           fill_values = hue_pal()(200),
           lty_values = rep("solid", 200),
           path_output = "plot.png",
           w = 10,
           h = 5,
           save_plot = FALSE,
           print_plot = FALSE) {
    p <- table %>%
      drop_na() %>%
      mutate(genotype = factor(genotype, levels = genotype_levels)) %>%
      group_by(genotype, experiment, mice) %>%
      mutate(value = value / first(value, order_by = time) * 100) %>%
      ungroup() %>%
      group_by(genotype, experiment, time) %>%
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
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 1) +
      geom_line(size = 1) +
      geom_point(size = 2,
                 stroke = 0.5) +
      labs(x = x_lab, y = y_lab, title = title_lab) +
      scale_x_continuous() +
      theme_clean(base_family = "sans", base_size = 11) +
      theme(
        strip.text.x = element_blank(),
        legend.position = "top",
        legend.background = element_rect(colour = "transparent",
                                         fill = "transparent"),
        legend.title = element_text(face = "plain", size = 9),
        legend.text = element_text(size = 9),
        plot.title = element_text(face = "plain", size = 10),
        axis.text.x = element_text(angle = x_angle, hjust = x_hjust),
        plot.background = element_rect(colour = NA,
                                       fill = "transparent")
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
    if (save_plot == TRUE) {
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
