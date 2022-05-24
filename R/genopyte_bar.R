data.frame(Reduce(rbind, all_data[str_subset(fig3, "BrdU")])) %>%
  filter(experiment != "1803") %>%
  relative_data(.) %>%
  mutate(strain = gsub('.{3}$', '', genotype)) %>%
  # filter(genotype %in% str_subset(genotype, "KO")) %>%
  mutate(strain = factor(
    strain,
    levels = c("VHL", "VHL-HIF1a", "VHL-HIF2a", "VHL-HIF1a-HIF2a"),
    ordered = TRUE
  )) %>% 
  mutate(genotype = factor(genotype, levels = VHL_table$genotypes[1:8], ordered = TRUE)) %>%
  ggplot(aes(strain, value, fill = genotype, color = genotype, shape = genotype)) +
  geom_bar(
    position = "identity",
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
    position = position_jitterdodge(jitter.width = 2.5, jitter.height=0, 
                                    dodge.width=0)
    )+
  theme_clean(base_family = "sans", base_size = 11) +
  theme(
    legend.position = "top",
    legend.background = element_rect(colour = "transparent",
                                     fill = "transparent"),
    legend.title = element_text(face = "plain", size = 9),
    legend.text = element_text(size = 9),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(face = "plain", size = 10),
    plot.background = element_rect(colour = NA,
                                   fill = "transparent")
  ) +
  scale_shape_manual(values = VHL_table$palette_shape[1:8], drop = FALSE) +
  scale_color_manual(values = VHL_table$palette_color[1:8], drop = FALSE) +
  scale_fill_manual(values =  VHL_table$palette_fill[1:8], drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0,.1)))  + 
  # geom_text(data = stats, aes(x = strain, y = 3, label = p.signif), size = 10) +
  facet_wrap(~ factor(treatment, levels = c("RPMI", "MCSF", "GMCSF")), strip.position = "top")



stats <- compare_means(value ~ genotype, group.by = c("treatment", "strain"), data = my_data, method = "t.test")
