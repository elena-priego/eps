#' curve_mean_plot
#' plot a graph curve with the mean value of each genotype by one column.
#' Not finished, it lacks the conection lines
#'
#' @param table tidy table coming form facs_tidytable
#' @param organ.i optional organ selected to plot (specimen in .fcs file)
#' @param experiment.i optional experiment to select
#' @param stat.i optional statistic selected to plot
#' @param marker.i optional marker selected to plot
#' @param cell.i optional cell selected to plot
#' @param time.i optional time selected to plot
#' @param treatment.i optional treatment selected to plot
#' @param genotype_levels vector will all the genotypes all the analysis
#' @param genotype_labels name to be display in the legend. In markdown/html format.
#' @param x_value value to plot in x axis. Default to cell
#' @param error_width width of the error bar
#' @param x_lab X-axis label
#' @param y_lab y-axis label
#' @param title_lab title label
#' @param y_limit y-axis limit
#' @param y_trans y-axis transformation
#' @param legend_position Legend position. Default to top. Could also be right, left or bottom
#' @param color_values color to be ploted. Same number as levels have genotype . For VHL paper table$VHL_palette_color
#' @param shape_values shape to be ploted. Same number as levels have genotype. For VHL paper table$VHL_palette_shape
#' @param fill_values fill color to be ploted. Same number as levels have genotype. For VHL paper table$VHL_palette_fill
#' @param lty_values line structure. to be ploted. Same number as levels have genotype. For VHL paper table$VHL_palette_lty
#' @param path_output full name of the generated plot including the path
#' (recommended path_output from path_builder())
#' @param w width of the output plot
#' @param h high of the output plot
#' @param print_plot boolean indicating if the plot is printed or not. Default to TRUE.
#'
#' @return
#' @export
#'
#' @examples
curve_mean_plot <-
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
           error_width = 1,
           title_lab = "",
           x_lab = "",
           y_lab = "",
           y_limit = 0,
           y_trans = "identity",
           leyend_position = "right",
           color_values = scales::hue_pal()(200),
           shape_values = rep(21, 200),
           fill_values = scales::hue_pal()(200),
           lty_values = rep("solid", 200),
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
      drop_na() %>%
      mutate(genotype = factor(genotype,
                               levels = genotype_levels,
                               labels = genotype_labels)) %>%
      group_by(stat, cell, treatment, marker, time, genotype) %>%
      summarise(
        N    = length(value),
        mean = mean(value),
        sd   = sd(value),
        se   = sd / sqrt(N),
        .groups = "drop"
      ) %>%
      ggplot(aes(
        y = mean,
        x = get(x_value),
        fill = genotype,
        color = genotype,
        shape = genotype
      )) +
      geom_point(alpha = 0.7,
                 size = 3,
                 stroke = 0) +
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = error_width) +
      geom_line(size = 0.5) +
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
      scale_shape_manual(values = shape_values,
                         drop = FALSE) +
      scale_color_manual(drop = FALSE,
                         values = color_values) +
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
    if (!is.null(path_output)) {
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
