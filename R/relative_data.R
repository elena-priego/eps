#' Make relative data to genotype
#' Make relative data to 1 for plotting data in VHL paper. To improve: allow column selection to group_by and pivot_lonter
#'
#' @param df input dataframe
#'
#' @return
#' @export
#'
#' @examples
relative_data <- function(df) {
  
  table1 <- df %>%
    group_by(marker, experiment) %>%
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
      cols = -c(marker, experiment, mean),
      values_drop_na = TRUE
    ) %>%
    unnest(cols = everything()) %>%
    mutate(raw_value = value,
           value = raw_value / mean)
  
  return(table3)
}
