#' Boxplot creation for cytometry data
#'
#' Boxplot generation for data created with facs_tidytable.
#' Design to work inside an apply function with all the possible combinations of
#' parameters in order to generate multiple plots (see examples).
#'
#' @param table tidy table coming form facs_tidytable
#' @param organ.i organ selected to plot (specimen in .fcs file)
#' @param stat.i statistic selected to plot
#' @param marker.i marker selected to plot
#' @param title.i title of the plot
#' @param x_lab x-axis label
#' @param y_lab y-axis label
#' @param y_limit inferior limit for y-axis
#' @param color_values 	a set of aesthetic values to map data values to.
#' The values will be matched in order (usually alphabetical).
#' @param color_breaks takes the limits as input and returns breaks as output
#' @param color_labels takes the breaks as input and returns labels as output
#' @param output name of the generated plot
#' @param path_output path were the plot will be saved. Recommended path_output
#' from path_builder()
#' @param w width of the output plot
#' @param h high of the output plot
#'
#' @import here
#' @import tidyverse
#' @import ggthemes
#'
#' @return plot file in data folder
#' @export
#'
#' @examples
#' comb <- as_tibble(unique(paste(table$organ, table$stat, table$marker))) %>%
#'   separate(value, into = c("organ", "stat", "marker"), sep = "\\s+") %>%
#'   mutate(output = paste0(organ, "_", stat, "_", marker, ".png"),
#'          y_lab = paste0(marker, " (", stat, ")")) %>% as.data.frame()
#' apply(comb, 1, function(x) facs_boxplot(table, organ.i = x[1], stat.i = x[2],
#'   marker.i = x[3], output = x[4], path_output = path_output, y_lab = x[5],
#'   title.i = x[1]))




facs_boxplot <-
  function(table,
           organ.i,
           stat.i,
           marker.i,
           title.i = "",
           x_lab = "",
           y_lab = "",
           y_limit = 0,
           color_values = RColorBrewer::brewer.pal(8, "Paired")[7:8],
           color_breaks = waiver(),
           color_labels = waiver(),
           output = "plot.png",
           path_output = path_output,
           w = 5,
           h = 5) {
    table.i %>%
      filter(organ == organ.i) %>%
      filter(stat == stat.i) %>%
      filter(marker == marker.i) %>%
      ggplot(aes(cell, value, colour = genotype)) +
      geom_boxplot(outlier.shape = NA,
                   fill = "transparent",
                   size = 0.5) +
      geom_point(
        position = position_jitterdodge(jitter.width = 0.3),
        alpha = 0.7,
        size = 3,
        stroke = 0
      ) +
      labs(x = x_lab, y = y_lab, title = title.i) +
      scale_y_continuous() +
      expand_limits(y = y_limit) +
      scale_x_discrete(labels = waiver()) +
      theme_clean(base_family = "sans", base_size = 18) +
      theme(
        strip.text.x = element_blank(),
        legend.position = "top",
        legend.background = element_rect(colour = "transparent",
                                         fill = "transparent"),
        legend.title = element_text(face = "plain", size = 15),
        legend.text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1,),
        plot.background = element_rect(colour = NA,
                                       fill = "transparent")
      ) +
      scale_color_manual(
        values = color_values,
        name = "Genotype:",
        breaks = color_breaks,
        labels = color_labels,
      ) +
      ggsave(
        file = path_output,
        width = w,
        height = h,
        bg = "transparent"
      )
  }
