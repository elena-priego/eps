#' Curve with mean and SE
#' plot a weight-loss curve with the mean value of each genotype and genotype
#'
#' @param table tidy table coming form facs_tidytable
#' @param organ.i optional organ selected to plot (specimen in .fcs file)
#' @param stat.i optional statistic selected to plot
#' @param marker.i optional marker selected to plot
#' @param cell.i optional cell selected to plot
#' @param time.i optional time selected to plot
#' @param treatment.i optional treatment selected to plot
#' @param genotype_levels vector will all the genotypes all the analysis
#' @param genotype_labels name to be display in the legend. In markdown/html format.
#' @param x_value column name to plot
#' @param title_lab title of the plot.
#' @param x_lab x-axis label
#' @param y_lab y-axis label
#' @param y_limit inferior limit for y-axis
#' @param x_angle angle of the labels of the x-axis.
#' @param x_hjust justification of the labels of the x-axis
#' @param leyend_position Legend position. Default to top. Could also be right, left or bottom
#' @param color_values 	a set of aesthetic values to map data values to.
#' The values will be matched in order (usually alphabetical).
#' @param shape_values shape to be ploted. Same number as levels have genotype.
#' @param fill_values fill color to be ploted. Same number as levels have genotype.
#' @param lty_values color to fill the lines
#' @param color_breaks takes the limits as input and returns breaks as output
#' @param color_labels takes the breaks as input and returns labels as output
#' @param path_output ful name of the generated plot including the path
#' (recommended path_output from path_builder())
#' @param w width of the output plot
#' @param h high of the output plot

#' @param print_plot boolean indicating if the plot is printed or not. Default to FALSE.
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
mean_plot <-
  function(table,
           organ.i = NULL,
           experiment.i = NULL,
           stat.i = NULL,
           time.i = NULL,
           marker.i = NULL,
           cell.i = NULL,
           treatment.i = NULL,
           genotype_levels = c("WT", "KO"),
           genotype_labels = genotype_levels,
           x_value = "cell",
           title_lab = "",
           x_lab = "",
           y_lab = "",
           y_limit = 0,
           y_trans = "identity",
           x_angle = 45,
           x_hjust = 1,
           leyend_position = "right",
           color_values = colorRamps::primary.colors(),
           shape_values = rep(21, 200),
           fill_values = color_values,
           lty_values = color_values,
           color_labels = waiver(),
           path_output = NULL,
           w = 10,
           h = 5,
           print_plot = FALSE) {
    p <- table %>%
      {if (!is.null(organ.i)) filter(., organ == organ.i) else .} %>%
      {if (!is.null(experiment.i)) filter(., experiment == experiment.i) else .} %>%
      {if (!is.null(time.i)) filter(., time == time.i) else .} %>%
      {if (!is.null(treatment.i)) filter(., treatment == treatment.i) else .} %>%
      {if (!is.null(stat.i)) filter(., stat == stat.i) else .} %>%
      {if (!is.null(marker.i)) filter(., marker == marker.i) else .} %>%
      {if (!is.null(cell.i)) filter(., cell == cell.i) else .} %>%
      mutate(genotype = factor(genotype,
                               levels = genotype_levels,
                               labels = genotype_labels)) %>%
      # drop_na() %>% #problems when mice are NA
      group_by(genotype, time, treatment, marker, stat, experiment, cell) %>%
      summarise( N    = length(value),
                 mean = mean(value),
                 sd   = sd(value),
                 se   = sd / sqrt(N),
                 .groups = "drop") %>%
      ggplot(
        aes(get(x_value),
            y=mean,
            fill = genotype,
            color = genotype,
            shape = genotype,
            group = genotype)) +
      geom_point(
        alpha = 0.7,
        size = 3,
        stroke = 0
      ) +
      geom_line(size = 0.5)+
      geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=0.2)+
      geom_line(size = 0.5)+
      labs(x = x_lab, y = y_lab, title = title_lab) +
      scale_y_continuous(trans = y_trans,
                         expand = expansion(mult = c(0, .1))) +
      expand_limits(y = y_limit) +
      labs(
        shape = " ",
        fill = " ",
        color = " ",
        x = x_lab,
        y = y_lab,
        title = title_lab
      ) +
      theme_clean(base_family = "sans", base_size = 11) +
      theme(
        strip.text.x = element_blank(),
        legend.position = leyend_position,
        legend.background = element_rect(colour = "transparent",
                                         fill = "transparent"),
        panel.grid.major.y = element_blank(),
        legend.title = element_markdown(face = "plain", size = 9),
        legend.text = element_markdown(size = 9),
        plot.background = element_rect(colour = NA,
                                       fill = "transparent"),
        plot.title = element_markdown(face = "plain", size = 10),
        axis.title.y = element_markdown(),
        axis.title.x = element_markdown()
      )+
      scale_shape_manual(values = shape_values,
                         drop = FALSE) +
      scale_color_manual(drop = FALSE,
                         values = color_values) +
      scale_fill_manual(values = fill_values,
                        drop = FALSE) +
      scale_linetype_manual(values = lty_values,
                            drop = FALSE)
    if (!is.null(path_output)) {
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
