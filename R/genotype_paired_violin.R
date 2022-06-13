#' Paired violin plot by genotype
#'
#' @param table tidy table with at least the following columns: genotype, value and experiment
#' @param genotype_levels vector will all the genotypes all the analysis
#' @param genotype_labels name to be display in the legend
#' @param x_lab X-axis label
#' @param y_lab y-axis label
#' @param title_lab title label
#' @param color_values color to be ploted. Same number as levels have genotype . For VHL paper table$VHL_palette_color
#' @param shape_values shape to be ploted. Same number as levels have genotype. For VHL paper table$VHL_palette_shape
#' @param fill_values fill color to be ploted. Same number as levels have genotype. For VHL paper table$VHL_palette_fill
#' @param path_output full name of the generated plot including the path
#' (recommended path_output from path_builder())
#' @param w width of the output plot
#' @param h high of the output plot
#' @param save_plot Boolean indicating if the plot is saved or not. Default to FALSE.
#' @param print_plot Boolean indicating if the plot is printed or not. Default to TRUE.
#'
#' @import here
#' @import tidyverse
#' @import ggthemes
#' @import ggtext
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
genotype_paired_violin <-
  function(table,
           genotype_levels = c("WT", "KO"),
           genotype_labels = genotype_levels,
           y_value = value,
           x_lab = "",
           y_lab = "",
           title_lab = "",
           color_values = hue_pal()(200),
           shape_values = rep(21, 200),
           fill_values = hue_pal()(200),
           path_output,
           w = 10,
           h = 5,
           save_plot = FALSE,
           print_plot = FALSE) {
    p <- table %>%
      mutate(genotype = factor(genotype,
                               levels = genotype_levels,
                               labels = genotype_labels)) %>%
      ggplot(aes(
        genotype,
        value,
        fill = genotype,
        color = genotype,
        shape = genotype
      )) +
      geom_violin(alpha = 0.5,
                  size = 0.5,
                  weight = 2) +
      geom_point(size = 2,
                 stroke = 0.5) +
      geom_line(aes(group = experiment), size = 0.8)  +
      labs(x = x_lab,
           y = y_lab,
           title = title_lab) +
      theme_clean(base_family = "sans", base_size = 11) +
      theme(
        legend.position = "top",
        legend.background = element_rect(colour = "transparent",
                                         fill = "transparent"),
        legend.title = element_markdown(face = "plain", size = 9),
        legend.text = element_markdown(size = 9),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_markdown(face = "plain", size = 10),
        plot.background = element_rect(colour = NA,
                                       fill = "transparent")
      ) +
      scale_color_manual(drop = FALSE,
                         values = color_values) +
      scale_fill_manual(values = fill_values,
                        drop = FALSE) +
      scale_shape_manual(values = shape_values,
                         drop = FALSE) +
      labs(shape = " ",
           fill = " ",
           color = " ") +
      guides(fill = guide_legend(override.aes = aes(label = "")))

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
