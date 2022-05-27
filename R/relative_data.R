#' Make relative data to genotype
#' Make relative data to WT samples from each experiment. representing this as 1.
#'
#' @param df input dataframe with at least genotype and value
#' @param group_diff text to remove from the genotype column to generate the strain by which the relativation groups will be generated. Default to "-WT|-KO"
#' @param group_control text that identify the control group to relative by. Should be included inside group_diff. Default to "WT"
#'
#' @return
#' @export
#'
#' @examples
#' my_data %>% relative_data(.) %>% genotype_violin()
#'
relative_data <- function(df, group_diff = "-WT|-KO", group_control = "WT") {

  df_groups <- colnames(df) %>% setdiff(., c("genotype", "value", "mice"))

  table1 <- df %>%
    mutate(strain = factor(str_replace_all(genotype, group_diff, "")),
           experiment = as.character(experiment)) %>%
    group_by_at(df_groups) %>%
    filter(str_detect(genotype, group_control)) %>%
    summarize(mean = sum(value) / n(), .groups = "keep")

  table2 <- df %>%
    mutate(experiment = as.character(experiment)) %>%
    pivot_wider(names_from = genotype,
                values_from = value,
                values_fn = list)

  full_table <- left_join(table1, table2)

  table3 <- full_table %>%
    pivot_longer(
      names_to = "genotype",
      cols = -c("mean", "mice", all_of(df_groups)),
      values_drop_na = TRUE
    ) %>%
    unnest(cols = everything()) %>%
    mutate(raw_value = value,
           value = raw_value / mean)

  return(table3)
}
