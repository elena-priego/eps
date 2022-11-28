#' PCA plot 
#' Dot plot to represent PCA data (from metabolomic data )
#'
#' @param table  tidy table coming form facs_tidytable
#' @param organ.i optional organ selected to plot (specimen in .fcs file)
#' @param stat.i optional statistic selected to plot
#' @param marker.i optional marker selected to plot
#' @param cell.i optional cell selected to plot
#' @param time.i optional time selected to plot
#' @param treatment.i optional treatment selected to plot
#' @param genotype_levels vector will all the genotypes all the analysis
#' @param genotype_labels name to be display in the legend. In markdown/html format.
#' @param x_lab x-axis label
#' @param y_lab y-axis label
#' @param title_lab title of the plot.
#' @param color_values 	a set of aesthetic values to map data values to.
#' @param fill_values fill color to be ploted. Same number as levels have genotype.
#'
#' @return plot 
#' @export
#'
#' @examples
#' 
#'
pca_plot <- function(table,
                     organ.i = NULL,
                     experiment.i = NULL,
                     stat.i = NULL,
                     time.i = NULL,
                     marker.i = NULL,
                     cell.i = NULL,
                     treatment.i = NULL,
                     genotype_levels = c("WT", "KO"),
                     genotype_labels = genotype_levels,
                     title_lab = "",
                     x_lab = "",
                     y_lab = "",
                     color_values = colorRamps::primary.colors(),
                     fill_values = color_values){
  table %>%
    {if (!is.null(organ.i)) filter(., organ == organ.i) else .} %>%
    {if (!is.null(experiment.i)) filter(., experiment == experiment.i) else .} %>%
    {if (!is.null(time.i)) filter(., time == time.i) else .} %>%
    {if (!is.null(treatment.i)) filter(., treatment == treatment.i) else .} %>%
    {if (!is.null(stat.i)) filter(., stat == stat.i) else .} %>%
    {if (!is.null(marker.i)) filter(., marker == marker.i) else .} %>%
    {if (!is.null(cell.i)) filter(., cell == cell.i) else .} %>%
    mutate(
      genotype = factor(genotype, levels = genotype_levels, labels = genotype_labels))%>%
    ggplot(aes(x=PC1, y=PC2, fill = genotype, color = genotype, shape = genotype)) + 
    geom_point(alpha = 0.7, size = 3, stroke = 0) + 
    labs(shape = " ", fill = " ", color = " ", x = x_lab, y = y_lab, title = title_lab) + 
    theme_clean(base_family = "sans", base_size = 11) + 
    theme(legend.position = "none", 
          legend.background = element_rect(colour = "transparent", fill = "transparent"),
          legend.title = element_markdown(face = "plain", size = 9), 
          legend.text = element_markdown(size = 9), 
          plot.title = element_markdown(face = "plain", size = 10), 
          plot.background = element_rect(colour = NA, fill = "transparent"), 
          axis.title.y = element_markdown(),
          panel.grid.major.y = element_blank()) + 
    scale_shape_manual(values = c(21, 21), drop = FALSE) + 
    scale_color_manual(drop = FALSE, values = color_values) + 
    scale_fill_manual(values = fill_values, drop = FALSE)
}