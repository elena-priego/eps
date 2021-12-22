#' Plot OCR / ECAR profile from a Seahorse analysis
#'
#' @param table tidy table with data comming from the analysis. Columns: time, genotype, mice, value
#' @param x_lab X-axis label
#' @param y_lab y-axis label
#' @param title.i title label
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
#' @returnplot file in data folder
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
           x_lab = "Time (minutes)",
           y_lab = "Oxygen Consumption Rate (OCR) \n (pmol/min)",
           title.i = "",
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
           print_plot = TRUE) {
    used_genotypes <- c(levels(fct_drop(table$genotype)))
    p <- table %>% group_by(genotype, time) %>%
      dplyr::summarise(mean = mean(value),
                       se   = sd(value)/sqrt(n()),
                       .groups = "drop") %>%
      ggplot(aes(x = time, y = mean, shape = genotype, color = genotype, fill = genotype, lty=genotype)) +
      geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=0.1)+
      geom_line(size = 1)+
      geom_point(
        size = 3,
        stroke = 1
      ) +
      labs(x = x_lab, y = y_lab, title = title.i) +
      scale_x_continuous() +
      theme_clean(base_family = "sans", base_size = 11) +
      theme(
        strip.text.x = element_blank(),
        legend.position = "top",
        legend.background = element_rect(colour = "transparent",
                                         fill = "transparent"),
        legend.title = element_text(face = "plain", size = 10),
        legend.text = element_text(size = 10),
        axis.text.x = element_text(angle = x_angle, hjust = x_hjust),
        plot.background = element_rect(colour = NA,
                                       fill = "transparent")
      ) +
      scale_shape_manual(values = shape_values,
                         drop = FALSE,
                         breaks = used_genotypes
      ) +
      scale_color_manual(drop = FALSE,
                         breaks = used_genotypes,
                         values = color_values) +
      scale_fill_manual(values = fill_values,
                        drop = FALSE,
                        breaks = used_genotypes
      ) +
      scale_linetype_manual(values = lty_values,
                            drop = FALSE,
                            breaks = used_genotypes
      )+
      labs(shape = "Genotype:", fill = "Genotype:", lty="Genotype:", color = "Genotype:")
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
