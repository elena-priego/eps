#' weight_individual_plot
#' plot a weight-loss curve with the individual value of each mice by day
#'
#'
#' @param table tidy table coming form facs_tidytable
#' @param title.i title of the plot
#' @param x_lab x-axis label
#' @param y_lab y-axis label
#' @param y_limit inferior limit for y-axis
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
#' @import tidyverse
#' @import ggthemes
#'
#' @return plot file in data folder
#' @export
#'
#' @examples
#' weight_individual_plot(table_raw, y_limit = 10,
#' path_output = here(path_output, "individual_raw.png"))

weight_individual_plot <-
  function(table,
           title.i = "",
           x_lab = "",
           y_lab = "",
           y_limit = 0,
           color_values = RColorBrewer::brewer.pal(8, "Paired")[7:8],
           color_breaks = waiver(),
           color_labels = waiver(),
           path_output,
           w = 10,
           h = 5,
           save_plot = TRUE,
           print_plot = FALSE) {
    p <- table %>%
      mutate(day = lubridate::ymd(day)) %>%
      ggplot(aes(x = day, y = value, colour = genotype)) +
      geom_point(
        position = position_jitterdodge(jitter.width = 0.3),
        alpha = 0.7,
        size = 3,
        stroke = 0
      ) +
      stat_summary(
        fun = 'mean',
        geom = 'line',
        size = 2,
        lty = 2
      ) +
      scale_x_date(date_labels = "%d-%b") +
      geom_line(aes(group = mice), size = 0.5) +
      labs(x = x_lab, y = y_lab, title = title.i) +
      scale_y_continuous() +
      expand_limits(y = y_limit) +
      theme_clean(base_family = "sans", base_size = 15) +
      theme(
        strip.text.x = element_blank(),
        legend.position = "right",
        legend.background = element_rect(colour = "transparent",
                                         fill = "transparent"),
        legend.title = element_text(face = "plain", size = 15),
        legend.text = element_text(size = 10),
        plot.background = element_rect(colour = NA,
                                       fill = "transparent")
      ) +
      scale_color_manual(
        values = color_values,
        name = "Genotype:",
        breaks = color_breaks,
        labels = color_labels
      )
    if (save_plot == TRUE) {
      ggsave(
        file = path_output,
        width = w,
        height = h,
        bg = "transparent"
      )
    }
    if(print_plot == TRUE) plot(p)
    return(p)
  }
