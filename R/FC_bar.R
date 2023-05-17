#' Bar plot representing FC
#' Plot by column showing FC (KO/WT)
#'
#' @param table tidy table with data coming from the analysis. Columns: genotype, value and experiment (time, mice, treatment, marker, stat, cell)
#' @param genotype_levels vector with all the genotypes all the analysis
#' @param genotype_labels vector with all the labels of the genotypes all the analysis. Default to genotype_levels
#' @param strain_levels ordered levels to plot. Default to VHL groups
#' @param group_diff text to remove from the genotype column to generate the strain by which the relativation groups will be generated. Default to "-WT|-KO"
#' @param group_control text that identify the group to relativize. Should be included inside group_diff. Default to "WT"
#' @param group_plot text that identify the group to plot. Should be included inside group_diff. Default to "KO"
#' @param x_lab  X-axis label
#' @param y_lab y-axis label
#' @param title_lab title label
#' @param y_trans transformation of the y axis
#' @param y_label default to waiver. Could be scientific_format()
#' @param color_values color to be ploted. Same number as levels have genotype . For VHL paper table$VHL_palette_color
#' @param shape_values shape to be ploted. Same number as levels have genotype. For VHL paper table$VHL_palette_shape
#' @param fill_values fill color to be ploted. Same number as levels have genotype. For VHL paper table$VHL_palette_fill
#' @param plot_stat Boolean indicating if include the stat. Default to TRUE.
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
#' @import ggtext
#'
#'
#' @return plot file in data folder
#' @export
#'
#' @examples
#'
#' plot %>%
#' FC_bar(genotype_levels = VHL_table$genotypes,
#' color_values = VHL_table$palette_color,
#' shape_values = VHL_table$palette_shape,
#' fill_values = VHL_table$palette_fill)
#'
#'
FC_bar <-
  function(table,
           genotype_levels = c("WT", "KO"),
           genotype_labels = genotype_levels,
           strain_levels =  c("VHL", "VHL-HIF1a", "VHL-HIF2a", "VHL-HIF1a-HIF2a"),
           group_diff = "-WT|-KO",
           group_control = "WT",
           group_plot = "KO",
           identity_bar = "dodge",
           x_lab = "",
           y_lab = "FC (WT/KO)",
           title_lab = "",
           y_trans = "identity",
           y_label = waiver(),
           color_values = hue_pal()(200),
           shape_values = rep(21, 200),
           fill_values = hue_pal()(200),
           plot_stat = TRUE,
           path_output = here(),
           w = 10,
           h = 5,
           save_plot = FALSE,
           print_plot = FALSE) {
    dg <- ifelse(identity_bar == "identity", 0, 1)

    table1 <- table %>%
      relative_data(., group_diff, group_control) %>%
      mutate(strain = factor(str_replace_all(genotype, group_diff, ""))) %>%
      mutate(strain = factor(strain,
                             levels = strain_levels,
                             ordered = TRUE)) %>%
      mutate(genotype = factor(genotype,
                               levels = genotype_levels,
                               labels = genotype_labels,
                               ordered = TRUE))

    p <- table1 %>%
      filter(str_detect(genotype, group_plot)) %>%
      ggplot(aes(
        strain,
        value,
        fill = genotype,
        color = genotype,
        shape = genotype
      )) +
      geom_hline(aes(yintercept = 1), color = "darkgrey")+
      geom_bar(
        position = identity_bar,
        stat = "summary",
               alpha = .3,
               fun = mean) +
      geom_errorbar(stat = "summary",
                    width = 0.5,
                    position = position_dodge(1)) +
      geom_point(
        aes(x = strain),
        size = 2,
        stroke = 0.5,
        position =
          position_jitterdodge(
            jitter.width = 1,
            jitter.height = 0,
            dodge.width = dg
          )
      ) +
      scale_y_continuous(trans = y_trans,
                         labels = y_label,
                         expand = expansion(mult = c(0, .1))) +
      theme_clean(base_family = "sans", base_size = 11) +
      theme(
        legend.position = "top",
        legend.background = element_rect(colour = "transparent",
                                         fill = "transparent"),
        legend.title = element_markdown(face = "plain", size = 9),
        legend.text = element_markdown(size = 9),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_markdown(face = "plain", size = 10),
        plot.background = element_rect(colour = NA,
                                       fill = "transparent")
      ) +
      scale_shape_manual(values = shape_values, drop = FALSE) +
      scale_color_manual(values = color_values, drop = FALSE) +
      scale_fill_manual(values =  fill_values, drop = FALSE) +
      scale_y_continuous(expand = expansion(mult = c(0, .1))) +
      labs(
        shape = " ",
        fill = " ",
        color = " ",
        x = x_lab,
        y = y_lab,
        title = title_lab
      )


    if (plot_stat == TRUE){
    stats <-  compare_means(
      value ~ genotype,
      group.by = c("marker", "strain"),
      data = table1,
      method = "t.test"
    )

    y_value <-
      table1 %>%
      group_by(marker) %>%
      top_n(1, value) %>%
      pull(value) * 1.1

    p <- p +
      geom_text(
        data = stats,
        aes(
          x = strain,
          y = rep(y_value, each = 4),
            label = p.signif),
        position = "stack",
        inherit.aes = FALSE
      )
    }

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
