#' Boxplot plot by genotype
#'
#' Boxplot to plot tidy standard data by genotype
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
#' @param title_lab title of the plot
#' @param x_lab x-axis label
#' @param y_lab y-axis label
#' @param y_limit inferior limit for y-axis
#' @param x_angle angle of the labels of the x-axis. NULL for horizontal,
#' 45 for inclination
#' @param x_hjust horizontal justification of the labels of the x-axis
#' @param color_values color to be ploted. Same number as levels have genotype .
#' @param shape_values shape to be ploted. Same number as levels have genotype. For
#' @param fill_values fill color to be ploted. Same number as levels have genotype.
#' @param path_output Optional. Full file name desired (e.g. here(path_output, "plot.pdf"))
#' @param w width of the output plot
#' @param h high of the output plot
#' @param print_plot boolean indicating if the plot is printed or not. Default to TRUE.


#'
#' @import here
#' @import tidyverse
#' @import ggtext
#'
#' @return plot file in data folder
#' @export
#'
#' @examples
#' comb <- as_tibble(unique(paste(table$organ, table$stat, table$marker,
#'                                sep = "_-_"))) %>%
#'         separate(value, into = c("organ", "stat", "marker"), sep = "_-_") %>%
#'         mutate(output = here(path_output,
#'                              paste0(organ, "_", stat, "_", marker, ".png")),
#'                y_lab = paste0(marker, " (", stat, ")")) %>% as.data.frame()
#'
#' apply(comb, 1, function(x) facs_boxplot(table, organ.i = x[1], stat.i = x[2],
#'   marker.i = x[3], path_output = x[4], y_lab = x[5],
#'   title.i = x[1]))



facs_boxplot <-
  function(table ="",
           organ.i = NULL,
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
           color_values = colorRamps::primary.colors(),
           shape_values = rep(21, 200),
           fill_values = color_values,
           path_output = NULL,
           w = 10,
           h = 5,
           print_plot = TRUE) {
    p <- table %>%
      {if (!is.null(organ.i)) filter(., organ == organ.i) else .} %>%
      {if (!is.null(time.i)) filter(., time == time.i) else .} %>%
      {if (!is.null(treatment.i)) filter(., treatment == treatment.i) else .} %>%
      {if (!is.null(stat.i)) filter(., stat == stat.i) else .} %>%
      {if (!is.null(marker.i)) filter(., marker == marker.i) else .} %>%
      {if (!is.null(cell.i)) filter(., cell == cell.i) else .} %>%
      mutate(genotype = factor(genotype,
                               levels = genotype_levels,
                               labels = genotype_labels)) %>%
      ggplot(
        aes(get(x_value),
            value,
            fill = genotype,
            color = genotype,
            shape = genotype)) +
      geom_boxplot(outlier.shape = NA,
                   fill = "transparent",
                   size = 0.5) +
      geom_point(
        position = position_jitterdodge(jitter.width = 0.3),
        alpha = 0.7,
        size = 3,
        stroke = 0
      ) +
      scale_y_continuous(trans = y_trans,
                         expand = expansion(mult = c(0, .1))) +
      expand_limits(y = y_limit) +
      scale_x_discrete(labels = waiver()) +
      theme_clean(base_family = "sans", base_size = 11)  +
      labs(
        shape = " ",
        fill = " ",
        color = " ",
        x = x_lab,
        y = y_lab,
        title = title_lab
      ) +
      theme(
        strip.text.x = element_blank(),
        legend.position = "right",
        legend.background = element_rect(colour = "transparent",
                                         fill = "transparent"),
        legend.title = element_markdown(face = "plain", size = 9),
        legend.text = element_markdown(size = 9),
        axis.text.x = element_markdown(angle = x_angle, hjust = x_hjust),
        plot.title = element_markdown(face = "plain", size = 10),
        plot.background = element_rect(colour = NA,
                                       fill = "transparent"),
        axis.title.y = element_markdown()
      ) +
      scale_shape_manual(values = shape_values,
                         drop = FALSE) +
      scale_color_manual(drop = FALSE,
                         values = color_values) +
      scale_fill_manual(values = fill_values,
                        drop = FALSE)
    if (!is.null(path_output)) {
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
