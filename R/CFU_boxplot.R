#' CFU_boxplot
#'
#' @param table tidy table coming form CFU_tidytable_genotype
#' @param x_lab x-axis label
#' @param y_lab y-axis label
#' @param title_lab title label
#' @param x_angle angle of the labels of the x-axis. NULL for horizontal,
#' 45 for inclination
#' @param x_hjust horizontal justification of the labels of the x-axis
#' @param color_values 	a set of aesthetic values to map data values to.
#' The values will be matched in order (usually alphabetical).
#' @param color_breaks takes the limits as input and returns breaks as output
#' @param color_labels takes the breaks as input and returns labels as output
#' @param path_output ful name of the generated plot including the path
#' (recommended path_output from path_builder())
#' @param w width of the output plot
#' @param h high of the output plot
#' @param save_plot boolean indicating if the plot is saved or not. Default to TRUE.
#' @param print_plot boolean indicating if the plot is printed or not. Default to FALSE.
#'
#' @import here
#' @import tidyverse
#' @import ggthemes
#'
#' @return plot file in data folder
#' @export
#'
#' @examples
#' CFU_boxplot(table)
#'
#'
CFU_boxplot <-
  function(table,
           x_lab = "genotype",
           y_lab = "L.pneumophila CFU at day 1 (log2 scale)",
           title_lab = "",
           x_angle = 45,
           x_hjust = 1,
           color_values = ggthemes::tableau_color_pal("Classic Green-Orange 12")[1:12],
           color_breaks = waiver(),
           color_labels = waiver(),
           path_output,
           w = 10,
           h = 5,
           save_plot = FALSE,
           print_plot = FALSE) {
    p <- table %>%
      ggplot(aes(genotype, mean_CFU, fill = genotype, colour = genotype)) +
      geom_boxplot(outlier.shape = NA,
                   fill = "transparent",
                   size = 0.5) +
      geom_dotplot(binaxis = "y", stackdir = "center") +
      scale_y_continuous(trans = "log2") +
      labs(x = x_lab,

           y = y_lab,
           title = title_lab) +
      theme_clean(base_family = "sans", base_size = 11) +
      theme(
        strip.text.x = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = x_angle, hjust = x_hjust),
        plot.background = element_rect(colour = NA,
                                       fill = "transparent")
      ) +
      scale_fill_manual(values = color_values,
                        breaks = color_breaks,
                        labels = color_labels) +
      scale_colour_manual(values = color_values,
                          breaks = color_breaks,
                          labels = color_labels)

    if (save_plot == TRUE) {
      ggsave(
      file = path_output,
      width = w,
      height = h,
      bg = "transparent"
    )
    }
    if (print_plot == TRUE) plot(p)
    return(p)
  }
