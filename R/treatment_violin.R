#' Violin plot by treatment
#'
#' @param table tidy table with data coming from the analysis. Columns:
#' time, mice, genotype, treatment, marker, stat, value, experiment
#' @param genotype_levels vector will all the genotypes all the analysis
#' @param x_lab X-axis label
#' @param y_lab y-axis label
#' @param title_lab title label
#' @param y_trans transformation of the y axis
#' @param y_label default to waiver. Could be scientific_format()
#' @param x_angle Angle to display the x axis
#' @param x_hjust Justificacion of the x axis
#' @param color_values color to be ploted. Same number as levels have genotype . For VHL paper table$VHL_palette_color
#' @param shape_values shape to be ploted. Same number as levels have genotype. For VHL paper table$VHL_palette_shape
#' @param fill_values fill color to be ploted. Same number as levels have genotype. For VHL paper table$VHL_palette_fill
#' @param path_output ful name of the generated plot including the path
#' (recommended path_output from path_builder())
#' @param w width of the output plot
#' @param h high of the output plot
#' @param save_plot Boolean indicating if the plot is saved or not. Default to FALSE.
#' @param print_plot Boolean indicating if the plot is printed or not. Default to TRUE.
#'
#' @import here
#' @import tidyverse
#' @import ggthemes
#'
#'
#' @return plot file in data folder
#' @export
#'
#' @examples
#' genotype_violin(genotype_levels = VHL_table$genotypes,
#' color_values = VHL_table$palette_color,
#' shape_values = VHL_table$palette_shape,
#' fill_values = VHL_table$palette_fill)
#'
treatment_violin <-
  function(table,
           genotype_levels = levels(table$genotype),
           x_lab = "",
           y_lab = "",
           title_lab = "",
           y_trans = "identity",
           y_label = waiver(),
           x_angle = NULL,
           x_hjust = NULL,
           color_values = hue_pal()(200),
           shape_values = rep(21, 200),
           fill_values = hue_pal()(200),
           path_output,
           w = 10,
           h = 5,
           save_plot = FALSE,
           print_plot = FALSE) {
    p <- table %>%
      mutate(genotype = factor(genotype, levels = genotype_levels)) %>%
      ggplot(aes(
        treatment,
        value,
        fill = genotype,
        color = genotype,
        shape = genotype
      )) +
      geom_violin(alpha = 0.5,
                  size = 0.5,
                  weight = 2) +
      geom_point(
        position = position_jitterdodge(jitter.width = 0.3),
        alpha = 0.7,
        size = 3,
        stroke = 0
      ) +
      # geom_point(size = 2,
      #            stroke = 0.5) +
      scale_y_continuous(trans = y_trans, labels = y_label, expand=c(0.1,0)) +
      labs(x = x_lab,
           y = y_lab,
           title = title_lab) +
      theme_clean(base_family = "sans", base_size = 11) +
      theme(
        legend.position = "top",
        legend.background = element_rect(colour = "transparent",
                                         fill = "transparent"),
        legend.title = element_text(face = "plain", size = 9),
        legend.text = element_text(size = 9),
        axis.text.x = element_text(angle = x_angle, hjust = x_hjust),
        plot.title = element_text(face = "plain", size = 10),
        plot.background = element_rect(colour = NA,
                                       fill = "transparent")
      ) +
      scale_shape_manual(values = shape_values,
                         drop = FALSE) +
      scale_color_manual(drop = FALSE,
                         values = color_values) +
      scale_fill_manual(values = fill_values,
                        drop = FALSE) +
      labs(shape = " ",
           fill = " ",
           color = " ")

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
