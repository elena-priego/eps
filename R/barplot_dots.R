#' Bar plot with dots
#' Plot by column having WT and KO next on the same bar and all the dots
#'
#' @param table tidy table with data comming from the analysis. Columns: genotype, value and experiment (time, mice, treatment, marker, stat, cell)
#' @param identity_bar Position to plot the bar graph. "identity" to pile, "dodge" to put next to each other. Default to dodge. Depending on the value, the points represented are pilled up by strain or not
#' @param x_lab  X-axis label
#' @param y_lab y-axis label
#' @param title_lab title label
#' @param y_trans transformation of the y axis
#' @param y_label default to waiver. Could be scientific_format()
#' @param x_angle Angle to display the x axis
#' @param x_hjust Justificacion of the x axis
#' @param color_values color to be ploted. Same number as levels have genotype . For VHL paper table$VHL_palette_color
#' @param shape_values shape to be ploted. Same number as levels have genotype. For VHL paper table$VHL_palette_shape
#' @param fill_values fill color to be ploted. Same number as levels have genotype. For VHL paper table$VHL_palette_fill
#' @param path_output ful name of the generated plot including the path
#' (recommended path_output from path_builder())
#' @param w width of the output plot
#' @param h high of the output plot
#' @param print_plot Boolean indicating if the plot is printed or not. Default to TRUE.
#' @param organ.i optional organ selected to plot (specimen in .fcs file)
#' @param stat.i optional statistic selected to plot
#' @param marker.i optional marker selected to plot
#' @param cell.i optional cell selected to plot
#' @param time.i optional time selected to plot
#' @param treatment.i optional treatment selected to plot
#' @param genotype_levels vector will all the genotypes all the analysis
#' @param genotype_labels name to be display in the legend. In markdown/html format.
#' @param x_value column name to plot
#' @param y_limit inferior limit for y-axis
#' @param x_labels labels for x axis
#' @param leyend_position Legend position. Default to top. Could also be right, left or bottom
#'
#' @import here
#' @import tidyverse
#' @import ggthemes
#' @import ggtext
#'
#'
#' @return plot file in data folder
#' @export
#'
#' @examples
#'
#'table %>%
#'  barplot_dots(x_value = "marker") +
#'  facet_wrap( ~ treatment)+
#'  stat_compare_means(
#'    method = "t.test",
#'    aes(label = ifelse(p < 0.05, p.signif, round(as.numeric(..p.format..), 2))),
#'    label.x = 1.5,
#'    size = 3
#'  )
#'
#'
barplot_dots <-
  function(table,
           organ.i = NULL,
           stat.i = NULL,
           time.i = NULL,
           marker.i = NULL,
           cell.i = NULL,
           treatment.i = NULL,
           genotype_levels = c("WT", "KO"),
           genotype_labels = genotype_levels,
           identity_bar = "dodge",
           x_value = "cell",
           x_lab = "",
           y_lab = "",
           y_limit = 0,
           title_lab = "",
           x_labels = waiver(),
           y_trans = "identity",
           y_label = waiver(),
           leyend_position = "top",
           x_angle = 0,
           x_hjust = .5,
           color_values = colorRamps::primary.colors(),
           shape_values = rep(21, 200),
           fill_values = colorRamps::primary.colors(),
           path_output = NULL,
           w = 10,
           h = 5,
           print_plot = FALSE) {
    dg <- ifelse(identity_bar == "identity", 0, 1)

    p <- table %>%
      {
        if (!is.null(organ.i))
          filter(., organ == organ.i)
        else
          .
      } %>%
      {
        if (!is.null(time.i))
          filter(., time == time.i)
        else
          .
      } %>%
      {
        if (!is.null(treatment.i))
          filter(., treatment == treatment.i)
        else
          .
      } %>%
      {
        if (!is.null(stat.i))
          filter(., stat == stat.i)
        else
          .
      } %>%
      {
        if (!is.null(marker.i))
          filter(., marker == marker.i)
        else
          .
      } %>%
      {
        if (!is.null(cell.i))
          filter(., cell == cell.i)
        else
          .
      } %>%
      mutate(genotype = factor(genotype,
                               levels = genotype_levels,
                               labels = genotype_labels)) %>%
      ggplot(aes(
        get(x_value),
        value,
        fill = genotype,
        color = genotype,
        shape = genotype,
        group = genotype
      )) +
      geom_bar(
        position = identity_bar,
        stat = "summary",
        alpha = .3,
        fun = mean
      ) +
      geom_errorbar(
        stat = "summary",
        fun.data = mean_sd,
        position = position_dodge(dg),
        width = 0.4
      ) +
      geom_point(
        aes(x = get(x_value)),
        size = 2,
        stroke = 0.5,
        position =
          position_jitterdodge(
            jitter.width = .8,
            jitter.height = 0,
            dodge.width = dg
          )
      ) +
      scale_y_continuous(trans = y_trans,
                         labels = y_label,
                         expand = expansion(mult = c(0, .1))) +
      scale_x_discrete(labels = x_labels) +
      expand_limits(y = y_limit) +
      ggthemes::theme_clean(base_family = "sans", base_size = 11) +
      theme(
        legend.position = leyend_position,
        legend.background = element_rect(colour = "transparent",
                                         fill = "transparent"),
        legend.title = element_markdown(face = "plain", size = 9),
        legend.text = element_markdown(size = 9),
        axis.text.x = element_markdown(angle = x_angle, hjust = x_hjust),
        plot.title = element_markdown(face = "plain", size = 10),
        plot.background = element_rect(colour = NA,
                                       fill = "transparent"),
        axis.title.y = element_markdown(),
        axis.title.x = element_markdown()
      ) +
      scale_shape_manual(values = shape_values, drop = FALSE) +
      scale_color_manual(values = color_values, drop = FALSE) +
      scale_fill_manual(values =  fill_values, drop = FALSE) +
      labs(
        shape = " ",
        fill = " ",
        color = " ",
        x = x_lab,
        y = y_lab,
        title = title_lab
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
