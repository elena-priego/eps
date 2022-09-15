#' Bar plot representing cell composition
#' Plot stacked columns by strain with error bars
#'
#' @param table tidy table with data coming from the analysis. Columns: genotype, value and experiment (time, mice, treatment, marker, stat, cell)
#' @param plot_cell cell to plot. Default to "AMs
#' @param genotype_levels vector with all the genotypes all the analysis
#' @param strain_levels vector with all the strains to be generated for the analysis
#' @param origin_diff text to remove that specify different origin inside one chimera
#' @param last_origin name of origin that makes the 100% in cumsum
#' @param leyend_position legend position. Default to top
#' @param group_diff text to remove from the genotype column to generate the strain by which the relativation groups will be generated. Default to "-WT|-KO"
#' @param x_lab  X-axis label
#' @param y_lab y-axis label
#' @param title_lab title label
#' @param y_trans transformation of the y axis
#' @param y_label default to waiver. Could be scientific_format()
#' @param color_values color to be ploted. Same number as levels have genotype . For VHL paper table$VHL_palette_color
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
composition_bar <-
  function(table,
           plot_cell = "AMs",
           genotype_levels = c("WT", "KO"),
           strain_levels =  c("VHL", "VHL-HIF1a", "VHL-HIF2a", "VHL-HIF1a-HIF2a"),
           origin_diff = "-host|-transplant|-SJ",
           group_diff = "-WT|-KO",
           last_origin = "transplant",
           leyend_position = "top",
           x_lab = "",
           y_lab = "Freq. of BAL AMs (%)",
           title_lab = "",
           y_trans = "identity",
           y_label = waiver(),
           color_values = hue_pal()(200),
           fill_values = hue_pal()(200),
           plot_stat = TRUE,
           path_output = NULL,
           w = 10,
           h = 5,
           save_plot = FALSE,
           print_plot = FALSE) {
    # Calculate mean and SD from data to plot

    ## Reduce table to select interesting data and generate strain variable
    perc <-
      table  %>%
      filter(marker == "freq" & stat == "Freq." & cell == plot_cell) %>%
      drop_na(treatment) %>%
      mutate(
        strain = factor(
          str_replace_all(genotype, origin_diff, ""),
          levels = strain_levels,
          ordered = TRUE
        ),
        genotype = factor(genotype,
                          levels = genotype_levels,
                          ordered = TRUE)
      )

    ## Generate strain2 variable with the same names to be compared in stat test
    stat.test <-
      perc %>%
      filter(treatment == last_origin) %>%
      mutate(strain2 = factor(str_replace_all(genotype, group_diff, ""))) %>%
      as.data.frame()

    ## Perform t-test by cell type and grouping by strain2
    stat.test <-
      compare_means(
        value ~ strain,
        data = stat.test,
        method = "t.test",
        group.by = c("cell", "strain2")
      )

    ## Obtain the mean of each group and calculate the total percentage in each group
    cummulative_perc <-
      perc %>%
      group_by(mice, stat, cell, treatment, marker, time, genotype) %>%
      summarize(value = mean(value),
                .groups = "keep") %>%
      group_by(mice, stat, cell, marker, time) %>%
      mutate(total = cumsum(value)) %>%
      filter(treatment == last_origin) %>%
      select(mice, stat, cell, marker, time, total)

    ## Relativize groups to make 100% total sum in each chimera group
    perc <-
      left_join(perc, cummulative_perc) %>%
      mutate(value = value * 100 / total)

    ## Calculate mean, sd and cumsum from each genotype
    table_mean <-
      perc %>%
      group_by(mice, stat, cell, treatment, marker, time, genotype, strain) %>%
      summarize(SD = sd(value),
                value = mean(value),
                .groups = "keep") %>%
      group_by(mice, stat, cell, marker, time, strain) %>%
      mutate(SDpos = cumsum(value))  %>%
      mutate(genotype = factor(genotype,
                               levels = genotype_levels,
                               ordered = TRUE))

    ## Bar plot of cumulative frequences
    p <- table_mean %>%
      mutate(genotype = factor(
        genotype,
        ### Sort levels in reverse order to plot correctly the cumulative sum
        ### TODO make genotype levels order it more general
        levels = c(
          "VHL-WT-transplant",
          "VHL-KO-transplant",
          "VHL-HIF1a-WT-transplant",
          "VHL-HIF1a-KO-transplant",
          "VHL-HIF2a-WT-transplant",
          "VHL-HIF2a-KO-transplant",
          "VHL-HIF1a-HIF2a-WT-transplant",
          "VHL-HIF1a-HIF2a-KO-transplant",
          "VHL-WT-host",
          "VHL-KO-host",
          "VHL-HIF1a-WT-host",
          "VHL-HIF1a-KO-host",
          "VHL-HIF2a-WT-host",
          "VHL-HIF2a-KO-host",
          "VHL-HIF1a-HIF2a-WT-host",
          "VHL-HIF1a-HIF2a-KO-host",
          "VHL-WT-SJ",
          "VHL-KO-SJ",
          "VHL-HIF1a-WT-SJ",
          "VHL-HIF1a-KO-SJ",
          "VHL-HIF2a-WT-SJ",
          "VHL-HIF2a-KO-SJ",
          "VHL-HIF1a-HIF2a-WT-SJ",
          "VHL-HIF1a-HIF2a-KO-SJ"
        ),
        labels = c(
          "Control-<i>Vhl</i><sup>fl/fl</sup>",
          "CD11c\u0394<i>Vhl</i>",
          "Control-<i>Vhl</i><sup>fl/fl</sup><i>Hif1a</i><sup>fl/fl</sup>",
          "CD11c\u0394<i>Vhl</i>\u0394<i>Hif1a</i>",
          "Control-<i>Vhl</i><sup>fl/fl</sup><i>Hif2a</i><sup>fl/fl</sup>",
          "CD11c\u0394<i>Vhl</i>\u0394<i>Hif2a</i>",
          "Control-<i>Vhl</i><sup>fl/fl</sup><i>Hif1a</i><sup>fl/fl</sup><i>Hif2a</i><sup>fl/fl</sup>",
          "CD11c\u0394<i>Vhl</i>\u0394<i>Hif1a</i>\u0394<i>Hif2a</i>",
          "Control-WT",
          "Control-WT",
          "Control-WT",
          "Control-WT",
          "Control-WT",
          "Control-WT",
          "Control-WT",
          "Control-WT",
          "Host",
          "Host",
          "Host",
          "Host",
          "Host",
          "Host",
          "Host",
          "Host"
        )
      )) %>%
      ggplot(aes(
        fct_rev(strain),
        value,
        fill = genotype,
        color = genotype,
        shape = genotype
      )) +
      geom_bar(
        position = "stack",
        stat = "summary",
        alpha = .3,
        fun = mean
      ) +
      scale_y_continuous(
        trans = "identity",
        breaks = c(0, 25, 50, 75, 100),
        # labels = y_label,
        expand = expansion(mult = c(0, .1))
      ) +
      theme_clean(base_family = "sans", base_size = 11) +
      theme(
        legend.position = leyend_position,
        legend.background = element_rect(colour = "transparent",
                                         fill = "transparent"),
        legend.title = element_markdown(face = "plain", size = 9),
        legend.text = element_markdown(size = 9),
        # axis.text.x = element_markdown(angle = x_angle, hjust = x_hjust),
        plot.title = element_markdown(face = "plain", size = 10),
        plot.background = element_rect(colour = NA,
                                       fill = "transparent"),
        axis.title.y = element_markdown(),
        axis.title.x = element_markdown()
      ) +

      ### TODO make genotype levels order it more general
      scale_color_manual(values = VHL_table$palette_color[
        c(10, 12, 14, 16, 18, 20, 22, 24, 9, 35)],
                         drop = FALSE) +
      scale_fill_manual(values =  VHL_table$palette_fill[
        c(10, 12, 14, 16, 18, 20, 22, 24, 9, 35)],
                        drop = FALSE) +
      geom_errorbar(aes(ymin = SDpos, ymax = SDpos + SD),
                    width = 0.4,
                    show.legend = FALSE) +

      ### TODO selections more general
      geom_text(
        aes(label = ifelse(
          genotype %in% c(
            "Control-<i>Vhl</i><sup>fl/fl</sup>",
            "CD11c\u0394<i>Vhl</i>",
            "Control-<i>Vhl</i><sup>fl/fl</sup><i>Hif1a</i><sup>fl/fl</sup>",
            "CD11c\u0394<i>Vhl</i>\u0394<i>Hif1a</i>",
            "Control-<i>Vhl</i><sup>fl/fl</sup><i>Hif2a</i><sup>fl/fl</sup>",
            "CD11c\u0394<i>Vhl</i>\u0394<i>Hif2a</i>",
            "Control-<i>Vhl</i><sup>fl/fl</sup><i>Hif1a</i><sup>fl/fl</sup><i>Hif2a</i><sup>fl/fl</sup>",
            "CD11c\u0394<i>Vhl</i>\u0394<i>Hif1a</i>\u0394<i>Hif2a</i>"
          ),
          paste0(round(value, 0), "%"),
          NA
        )),
        position = position_stack(vjust = 0.5),
        show.legend = FALSE,
        # fill = "white",
        color = "black",
        size = 3
      ) +
      labs(
        shape = " ",
        fill = " ",
        color = " ",
        x = x_lab,
        y = y_lab
      )

    ## Plot text from statistic
    p <- p +
      stat_compare_means(
        comparisons = list(
          c("VHL-WT", "VHL-KO"),
          c("VHL-HIF1a-WT", "VHL-HIF1a-KO"),
          c("VHL-HIF2a-WT", "VHL-HIF2a-KO"),
          c("VHL-HIF1a-HIF2a-WT", "VHL-HIF1a-HIF2a-KO")
        ),
        hide.ns = TRUE,
        step.increase = 0,
        show.legend = FALSE,
        size = 0
      ) +
      geom_text(
        x = 1.5,
        y = 123,
        label = stat.test$p.signif[4],
        show.legend = FALSE,
        color = "black",
        check_overlap = TRUE
      ) +
      geom_text(
        x = 3.5,
        y = 123,
        label = stat.test$p.signif[3],
        show.legend = FALSE,
        color = "black",
        check_overlap = TRUE
      ) +
      geom_text(
        x = 5.5,
        y = 123,
        label = stat.test$p.signif[2],
        show.legend = FALSE,
        color = "black",
        check_overlap = TRUE
      ) +
      geom_text(
        x = 7.5,
        y = 123,
        label = stat.test$p.signif[1],
        show.legend = FALSE,
        color = "black",
        check_overlap = TRUE
      ) +
      ##Frip from vertical to horizontal
      coord_flip()

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
