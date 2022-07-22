#' Bar plot by strains
#' Plot by column having WT and KO next on the same bar
#'
#' @param table tidy table with data comming from the analysis. Columns: genotype, value and experiment (time, mice, treatment, marker, stat, cell)
#' @param genotype_levels vector will all the genotypes all the analysis
#' @param strain_levels ordered levels to plot. Default to VHL groups
#' @param group_diff text to remove from the genotype column to generate the strain by which the relativation groups will be generated. Default to "-WT|-KO"
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
#' @param save_plot Boolean indicating if the plot is saved or not. Default to FALSE.
#' @param print_plot Boolean indicating if the plot is printed or not. Default to TRUE.
#'
#' @import here
#' @import tidyverse
#' @import ggthemes
#'
#'
#' @return plot file in data folder
#' @export
#'
#' @examples
#'
#' plot %>%
#' strain_bar(genotype_levels = VHL_table$genotypes,
#' color_values = VHL_table$palette_color,
#' shape_values = VHL_table$palette_shape,
#' fill_values = VHL_table$palette_fill)
#'
#'
strain_bar <-
  function(table,
           genotype_levels = c("WT", "KO"),
           strain_levels =  c("VHL", "VHL-HIF1a", "VHL-HIF2a", "VHL-HIF1a-HIF2a"),
           group_diff = "-WT|-KO",
           identity_bar = "dodge",
           x_lab = "",
           y_lab = "",
           title_lab = "",
           y_trans = "identity",
           y_label = waiver(),
           # x_angle = NULL,
           # x_hjust = NULL,
           color_values = hue_pal()(200),
           shape_values = rep(21, 200),
           fill_values = hue_pal()(200),
           path_output,
           w = 10,
           h = 5,
           save_plot = FALSE,
           print_plot = FALSE) {
    dg <- ifelse(identity_bar == "identity", 0, 1)

    p <- table %>%
      mutate(strain = factor(str_replace_all(genotype, group_diff, ""))) %>%
      mutate(strain = factor(strain,
                             levels = strain_levels,
                             ordered = TRUE)) %>%
      mutate(genotype = factor(genotype,
                               levels = genotype_levels,
                               ordered = TRUE)) %>%
      ggplot(aes(
        strain,
        value,
        fill = genotype,
        color = genotype,
        shape = genotype
      )) +
      geom_bar(
        position = identity_bar,
        stat = "summary",
        alpha = .3,
        fun = mean
      ) +
      geom_errorbar(stat = "summary",
                    position = "dodge",
                    width = 0.25) +
      geom_point(
        aes(x = strain),
        size = 2,
        stroke = 0.5,
        position =
          position_jitterdodge(
          jitter.width = 2.5,
          jitter.height = 0,
          dodge.width = dg
        )
      ) +
      scale_y_continuous(trans = y_trans,
                         labels = y_label,
                         expand = expansion(mult = c(0,.1))) +
      theme_clean(base_family = "sans", base_size = 11) +
      theme(
        legend.position = "top",
        legend.background = element_rect(colour = "transparent",
                                         fill = "transparent"),
        legend.title = element_text(face = "plain", size = 9),
        legend.text = element_text(size = 9),
        axis.title.x = element_blank(),
        # axis.text.x = element_blank(angle = x_angle, hjust = x_hjust),
        axis.ticks.x = element_blank(),
        plot.title = element_text(face = "plain", size = 10),
        plot.background = element_rect(colour = NA,
                                       fill = "transparent")
      ) +
      scale_shape_manual(values = shape_values, drop = FALSE) +
      scale_color_manual(values = color_values, drop = FALSE) +
      scale_fill_manual(values =  fill_values, drop = FALSE) +
      labs(shape = " ",
           fill = " ",
           color = " ",
           x = x_lab,
           y = y_lab,
           title = title_lab)

    if (save_plot == TRUE) {
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
