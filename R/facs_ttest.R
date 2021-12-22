#' t-test analysis for facs data
#'
#' DEPRECATED- use ggpubr::compare_means() instead. Later ggplot+stat_compare_means(label =  "p.signif", label.x = 1.5))
#'
#' t-test analysis for data coming in a tidy format from facs_tidytable
#' function.
#'
#' @param table name of the table to be analyzed. Filter to make that genotype
#' column contains only 2 factors
#' @param path_output path were the output will be stored
#' @param file1 name of the file containing all the analysis
#' @param file2 name of the file containing only the significant analysis
#'
#' @import here
#' @import tidyverse
#' @import broom
#'
#' @return print in the screen the significant values
#' @return significant_values data table with the significant samples and their
#' p-value
#' @return t-test.csv file with the whole analysis in output folder
#' @return significant-t-test.csv file with the samples that show a p-value
#' lower than 0.05 in output folder
#' @export
#'
#' @examples
#' table1 <- filter(genotype %in% c("VHL-HIF1a-KO  ", "VHL-HIF1a-WT  "))
#' facs_ttest(table1,
#'            path_output = path_output,
#'            file1 = "HIF1a-DKO-t.test.csv",
#'            file2 = "HIF1a-DKO-significant-t-test.csv")
facs_ttest <-
  function(table,
           path_output = path_output,
           file1 = "t-test.csv",
           file2 = "significant-t-test.csv") {
    stats <- table %>%
      nest(data = c(mice, value, genotype))  %>%
      mutate(ttest = map(data, ~ t.test(.$value ~ .$genotype)))  %>%
      mutate(ttest = map(ttest, ~ tidy(.))) %>%
      unnest(cols = c(ttest)) %>%
      select(organ, cell, stat, marker, estimate, estimate1, estimate2, p.value)
    write.table(
      stats,
      row.names = FALSE,
      file = here(path_output, file1),
      sep = ","
    )
    if (nrow(stats[which(stats$p.value < 0.05),]) > 0) {
      significative <-
        stats[which(stats$p.value < 0.05),
              c("organ", "cell", "stat", "marker", "p.value")]
      write.table(
        significative,
        row.names = FALSE,
        file = here(path_output, file2),
        sep = ","
      )
      print(significative)
      significant_values <<- significative
    }
  }
