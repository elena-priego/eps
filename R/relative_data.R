#' Make relative data to genotype
#' Make relative data to WT samples from each experiment. representing this as 1.
#'
#' @param df input dataframe with at least genotype and value
#'
#' @return
#' @export
#'
#' @examples
#' my_data %>% relative_data(.) %>% genotype_violin()
#'
relative_data <- function(df) {

  df_groups <- colnames(df) %>% setdiff(., c("genotype", "value"))

  table1 <- df %>%
    mutate(strain = factor(str_replace_all(genotype, "-WT|-KO", ""))) %>%
    group_by_at(df_groups) %>%
    filter(str_detect(genotype, "WT")) %>%
    summarize(mean = sum(value) / n(), .groups = "keep")

  table2 <- df %>%
    pivot_wider(names_from = genotype,
                values_from = value,
                values_fn = list)

  full_table <- left_join(table1, table2)

  table3 <- full_table %>%
    pivot_longer(
      names_to = "genotype",
      cols = c("genotype", "value"),
      values_drop_na = TRUE
    ) %>%
    unnest(cols = everything()) %>%
    mutate(raw_value = value,
           value = raw_value / mean)

  return(table3)
}
