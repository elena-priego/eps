#' weight_mean_plot
#' plot a weight-loss curve with the mean value of each genotype by day
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
#'
#' @import here
#' @import tidyverse
#' @import ggthemes
#'
#' @return plot file in data folder
#' @export
#'
#' @examples
#' weight_mean_plot(table_raw, y_limit = 10,
#' path_output = here(path_output, "mean_raw.png"))
#'
weight_mean_plot <-
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
           h = 5) {
    p <- table %>%
      mutate(day = lubridate::ymd(day)) %>%
      group_by(genotype, day) %>%
      summarise( N    = length(value),
                 mean = mean(value),
                 sd   = sd(value),
                 se   = sd / sqrt(N),
                 .groups = "drop") %>%
      mutate(genotype = factor(genotype,
                               levels = c("VHL-HIF2a-WT  ", "VHL-HIF2a-KO  "),
                               ordered = TRUE)) %>%
      ggplot(aes(y=mean, x=day, colour = genotype)) +
      geom_point(
        alpha = 0.7,
        size = 3,
        stroke = 0
      ) +
      geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=0.2)+
      geom_line(size = 0.5)+
      scale_x_date(date_labels = "%d-%b") +
      labs(x = x_lab, y = y_lab, title = title.i) +
      scale_y_continuous() +
      expand_limits(y = y_limit) +
      theme_clean(base_family = "sans", base_size = 18) +
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
    plot(p)
    ggsave(
      file = path_output,
      width = w,
      height = h,
      bg = "transparent"
    )
    return(p)
  }
