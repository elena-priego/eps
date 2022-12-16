#' Volcano plot 
#'
#' @param table table with at least one column with data coding for the color to plot, one for -log10(pvalue) and other with log2FoldChange
#' @param organ.i optional organ selected to plot (specimen in .fcs file)
#' @param stat.i optional statistic selected to plot
#' @param marker.i optional marker selected to plot
#' @param cell.i optional cell selected to plot
#' @param time.i optional time selected to plot
#' @param treatment.i optional treatment selected to plot
#' @param x_value column containing log2 fold change data
#' @param y_value column containing -log10(pvalue) data
#' @param x_lab X-axis label
#' @param y_lab y-axis label
#' @param title_lab title label
#' @param leyend_position Legend position. Default to right. Could also be top, left or bottom
#' @param color_levels column containing the levels to color with
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
#' @return
#' @export
#'
#' @examples
#' volcano_plot(
#' metabotable2,
#' cell.i = "BM_cDC1",
#' x_value = "FC2",
#' y_value = "pvalue10",
#' color_levels = "compound",
#' color_values = c("#990F26","#99600F", "#54990F", "#3D0F99","#0F8299","#333333"),
#' x_lab = "log2FoldChange",
#' y_lab = "-log10(pvalue)"
#' )+
#'   geom_vline(xintercept=c(-0.6, 0.6), col="darkgrey", lty = 2) +
#'   geom_hline(yintercept=-log10(0.05), col="darkgrey", lty = 2)
#' 
volcano_plot <- function(
    table,
    organ.i = NULL,
    experiment.i = NULL,
    stat.i = NULL,
    time.i = NULL,
    marker.i = NULL,
    cell.i = NULL,
    treatment.i = NULL,
    x_value, 
    y_value,
    x_lab = "",
    y_lab = "",
    title_lab = "",
    leyend_position = "right",
    color_levels,
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
    ggplot(aes(
      get(x_value),
      get(y_value),
      fill = get(color_levels),
      color = get(color_levels),
      shape = get(color_levels)
    )) +
    geom_point(
      alpha = 0.7,
      size = 2,
      stroke = 0
    ) +
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
      legend.title = element_markdown(face = "plain", size = 9),
      legend.text = element_markdown(size = 9),
      plot.title = element_markdown(face = "plain", size = 10),
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
