#' ttest analysis form qPCR
#'
#' @param table table with the information. Should have the columns 
#' "Cq", "Target", "mice", "Mean_Cq", "Cq_SD", "genotype", "Delta", "Expression"
#' @param genotype_levels Select 2 levels to perform the t.test
#' @param name name of the output files (before -t-test.CSV)
#' @param excluded_genes name of the genes to be excluded in the analysis 
#' #know the present genes in each sample to exclude in the t.test analysis the ones that doesn't appear 
#' table_target <- table %>% 
#'   group_by(Target, genotype) %>%  
#'   summarise(n=n()) %>%
#'   pivot_wider(id_cols=Target, names_from = genotype, values_from = n, values_fill = 0)
#'
#' @return
#' @export
#'
#' @examples
#' qPCR_ttest(table, c("VHL-WT  ", "VHL-KO  "), "VHL")
#' 
qPCR_ttest <- function (table, genotype_levels, name, excluded_genes = c("Actin", "empty", "H2O"))
{
  stats <- table %>% filter(genotype %in% genotype_levels) %>%
    mutate(genotype = factor(genotype, genotype_levels))  %>%
    filter(!Target %in% excluded_genes) %>%
    select(-c("Mean_Cq", "Cq_SD", "mice", "Cq")) %>% 
    nest(data = c(genotype, Delta, Expression))   %>%
    mutate(ttest_Delta = map(data, ~ t.test(.$Delta ~ .$genotype))) %>%
    mutate(ttest_Expression = map(data, ~ t.test(.$Expression ~ .$genotype))) %>%
    mutate(ttest_Delta = map(ttest_Delta, ~ tidy(.))) %>%
    mutate(ttest_Expression = map(ttest_Expression, ~ tidy(.)))  %>%
    unnest(cols = c(ttest_Delta, ttest_Expression),
           names_sep = "_")   %>%
    select(Target, ttest_Delta_p.value, ttest_Expression_p.value)
  write.table(
    stats,
    row.names = FALSE,
    file = here(path_output,
                paste0(name, "t-test.csv")),
    sep = ","
  )
  if (nrow(stats[which(stats$ttest_Delta_p.value < 0.05 | stats$ttest_Expression_p.value < 0.05),]) > 0) {
    significative <-
      stats[which(stats$ttest_Delta_p.value < 0.05 | stats$ttest_Expression_p.value < 0.05 ), c("Target",
                                                                                                "ttest_Delta_p.value",
                                                                                                "ttest_Expression_p.value")]
    write.table(
      significative,
      row.names = FALSE,
      file = here(path_output,
                  paste0(name, "significant-t-test.csv")),
      sep = ","
    )
    print(significative)
    significant_values <<- significative
  }
}

