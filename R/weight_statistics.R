#' weight_statistics
#' Function to perform the statistic analysis of the weight-loss curve experiments.
#'
#' @param table_tidy input table in tidy format with columns for genotype, day, value and mice.
#' @param path_to_save path where the output txt will be saved (path_output from the path_builder function).
#' @param file_name Name of the output file. "statistic.txt" by default.
#'
#' @import rstatix
#'
#' @return
#' @export
#'
#' @examples
#'weight_statistics(table_raw, path_to_save = path_output)
#'

weight_statistics <-
  function(table_tidy,
           path_to_save = path_output,
           file_name = "statistic.txt") {
    output <- here::here(path_to_save, file_name)

    outliers <- table_tidy %>%
      group_by(genotype, day) %>%
      identify_outliers(value)
    cat("Outliers:\n")
    print(outliers)
    cat("Outliers:\n", file = output) #first in the file, without append = TRUE
    suppressWarnings(
      write.table(
        outliers,
        output,
        quote = FALSE,
        sep = "\t",
        append = TRUE,
        row.names = FALSE
      )
    )

    anova <- table_tidy %>%
      mutate(day = as.factor(day)) %>%
      aov(value ~ genotype * day, data = .)
    anova <- summary(anova)
    cat("\n\nAnova test (genotype * day):\n")
    print(anova)
    cat("\n\n\nAnova:\n", file = output, append = TRUE)
    capture.output(anova, file = output, append = TRUE)

    one.way <- table_tidy %>%
      filter(value != 100) %>% #to allow the anova with the relative weight on day 1
      mutate(day = as.factor(day)) %>%
      group_by(day) %>%
      anova_test(value ~ genotype) %>%
      get_anova_table() %>%
      adjust_pvalue(method = "bonferroni")
    cat("\n\nPost-hoc test(Bonferroni):\n")
    print(one.way)
    cat("\n\n\nPost-hoc test(Bonferroni):\n",
        file = output,
        append = TRUE)
    suppressWarnings(
      write.table(
        one.way,
        output,
        quote = FALSE,
        sep = "\t\t",
        append = TRUE,
        row.names = FALSE
      )
    )
  }
