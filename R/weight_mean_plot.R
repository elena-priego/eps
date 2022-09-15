#' weight_mean_plot
#' plot a weight-loss curve with the mean value of each genotype by day
#'
#' @param table tidy table coming form facs_tidytable
#' @param organ.i optional organ selected to plot (specimen in .fcs file)
#' @param stat.i optional statistic selected to plot
#' @param marker.i optional marker selected to plot
#' @param cell.i optional cell selected to plot
#' @param time.i optional time selected to plot
#' @param treatment.i optional treatment selected to plot
#' @param title_lab title of the plot. Previously title.i
#' @param x_lab x-axis label
#' @param y_lab y-axis label
#' @param y_limit inferior limit for y-axis
#' @param leyend_position Legend position. Default to top. Could also be right, left or bottom
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
#' @import ggtext
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
           organ.i = NULL,
           experiment.i = NULL,
           stat.i = NULL,
           time.i = NULL,
           marker.i = NULL,
           cell.i = NULL,
           treatment.i = NULL,
           title_lab = "",
           x_lab = "",
           y_lab = "",
           y_limit = 0,
           leyend_position = "right",
           color_values = RColorBrewer::brewer.pal(8, "Paired")[7:8],
           color_breaks = waiver(),
           color_labels = waiver(),
           path_output,
           w = 10,
           h = 5,
           save_plot = TRUE,
           print_plot = FALSE) {
    p <- table %>%
      {if (!is.null(organ.i)) filter(., organ == organ.i) else .} %>%
      {if (!is.null(experiment.i)) filter(., experiment == experiment.i) else .} %>%
      {if (!is.null(time.i)) filter(., time == time.i) else .} %>%
      {if (!is.null(treatment.i)) filter(., treatment == treatment.i) else .} %>%
      {if (!is.null(stat.i)) filter(., stat == stat.i) else .} %>%
      {if (!is.null(marker.i)) filter(., marker == marker.i) else .} %>%
      {if (!is.null(cell.i)) filter(., cell == cell.i) else .} %>%
      drop_na() %>%
      mutate(day = lubridate::ymd(day)) %>%
      group_by(genotype, day) %>%
      summarise( N    = length(value),
                 mean = mean(value),
                 sd   = sd(value),
                 se   = sd / sqrt(N),
                 .groups = "drop") %>%
      ggplot(aes(y=mean, x=day, colour = genotype)) +
      geom_point(
        alpha = 0.7,
        size = 3,
        stroke = 0
      ) +
      geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=0.2)+
      geom_line(size = 0.5)+
      scale_x_date(date_labels = "%d-%b") +
      labs(x = x_lab, y = y_lab, title = title_lab) +
      scale_y_continuous(trans = y_trans,
                         expand = expansion(mult = c(0, .1))) +
      expand_limits(y = y_limit) +
      theme_clean(base_family = "sans", base_size = 11) +
      theme(
        strip.text.x = element_blank(),
        legend.position = leyend_position,
        legend.background = element_rect(colour = "transparent",
                                         fill = "transparent"),
        legend.title = element_markdown(face = "plain", size = 9),
        legend.text = element_markdown(size = 9),
        plot.background = element_rect(colour = NA,
                                       fill = "transparent"),
        plot.title = element_markdown(face = "plain", size = 10),
        axis.title.y = element_markdown(),
        axis.title.x = element_markdown()
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
