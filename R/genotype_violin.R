#' Violin plot by genotype
#'
#' Violin for tidy standard data by genotype
#'
#' @param table tidy table with time, mice, genotype, treatment, marker, stat, value, experiment and cell columns
#' @param organ.i optional organ selected to plot (specimen in .fcs file)
#' @param stat.i optional statistic selected to plot
#' @param marker.i optional marker selected to plot
#' @param cell.i optional cell selected to plot
#' @param time.i optional time selected to plot
#' @param treatment.i optional treatment selected to plot
#' @param genotype_levels vector will all the genotypes all the analysis
#' @param genotype_labels name to be display in the legend. In markdown/html format.
#' @param jitter_width width of the points
#' @param x_value column name to plot
#' @param plot_mean boolean to plot the mean
#' @param x_lab X-axis label
#' @param y_lab y-axis label
#' @param y_limit inferior limit for y-axis
#' @param title_lab title label
#' @param y_trans transformation of the y axis
#' @param y_label default to waiver. Could be scientific_format()
#' @param x_angle Angle to display the x axis
#' @param x_hjust Justificacion of the x axis
#' @param leyend_position Legend position. Default to top. Could also be right, left or bottom
#' @param color_values color to be ploted. Same number as levels have genotype.
#' @param shape_values shape to be ploted. Same number as levels have genotype.
#' @param fill_values fill color to be ploted. Same number as levels have genotype.
#' @param path_output Optional. Full file name desired (e.g. here(path_output, "plot.pdf"))
#' @param w width of the output plot
#' @param h high of the output plot
#' @param print_plot Boolean indicating if the plot is printed or not. Default to TRUE.
#'
#' @import here
#' @import tidyverse
#' @import ggtext
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
genotype_violin <-
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
           jitter_width = 0.3,
           x_value = "genotype",
           plot_mean = TRUE,
           x_lab = "",
           y_lab = "",
           title_lab = "",
           y_trans = "identity",
           y_limit = 0,
           x_angle = 90,
           x_hjust = 0.5,
           x_labels = waiver(),
           leyend_position = "top",
           color_values = colorRamps::primary.colors(),
           shape_values = rep(21, 200),
           fill_values = color_values,
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
      group_by(genotype, experiment) %>%
      ggplot(aes(
        get(x_value),
        value,
        fill = genotype,
        color = genotype,
        shape = genotype
      )) +
      geom_violin(alpha = 0.5,
                  linewidth = 0.5,
                  weight = 2) +
      geom_point(
        position = position_jitterdodge(jitter.width = jitter_width),
        alpha = 0.7,
        size = 3,
        stroke = 0
      ) +
      # ggrepel::geom_text_repel(aes(label = experiment))+ 3 to plot experiment
      scale_y_continuous(trans = y_trans,
                         expand = expansion(mult = c(0, .1))) +
      expand_limits(y = y_limit) +
      scale_x_discrete(labels = x_labels) +
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
        legend.position = leyend_position,
        legend.background = element_rect(colour = "transparent",
                                         fill = "transparent"),
        panel.grid.major.y = element_blank(),
        legend.title = element_markdown(face = "plain", size = 9),
        legend.text = element_markdown(size = 9),
        axis.text.x = element_markdown(angle = x_angle, hjust = x_hjust),
        plot.title = element_markdown(face = "plain", size = 10, hjust = 0.5),
        plot.background = element_rect(colour = NA,
                                       fill = "transparent"),
        axis.title.y = element_markdown(),
        axis.title.x = element_markdown()
      ) +
      scale_shape_manual(values = shape_values,
                         drop = FALSE) +
      scale_color_manual(drop = FALSE,
                         values = color_values) +
      scale_fill_manual(values = fill_values,
                        drop = FALSE)
    if (plot_mean == TRUE) {
      p <- p +
        stat_summary(fun = "mean", geom = "crossbar", linewidth = 0.2,
                     position = position_jitterdodge(jitter.width = jitter_width))
    }
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
