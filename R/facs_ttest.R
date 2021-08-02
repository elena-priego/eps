#' t-test analysis for facs data
#'
#' t-test analysis for data coming in a tidy format from facs_tidytable
#' function.
#'
#' @param table name of the table to be analyzed
#' @param file1 name of the file containing all the analysis
#' @param file2 name of the file containing only the significant analysis
#'
#' @import here
#' @import tidyverse
#'
#' @return print in the screen the significant values
#' @return significant_values data table with the significant samples and their
#' p-value
#' @return t-test.csv file with the whole analysis in result folder
#' @return significant-t-test.csv file with the samples that show a p-value
#' lower than 0.05 in result folder
#' @export
#'
#' @examples facs_ttest(table)
facs_ttest <-
  function(table,
           file1 = "t-test.csv",
           file2 = "significant-t-test.csv") {
    stats <- table %>%
      group_by(organ, cell, stat, marker)  %>%
      summarise(ttest = list(t.test(value ~ genotype)))  %>%
      mutate(ttest = map(ttest, tidy)) %>%
      unnest(cols = c(ttest)) %>%
      select(organ, cell, stat, marker, estimate, estimate1, estimate2, p.value)
    write.table(
      stats,
      row.names = FALSE,
      file = here(path_result, file1),
      sep = ","
    )
    if (nrow(stats[which(stats$p.value < 0.05),]) > 0) {
      significative <-
        stats[which(stats$p.value < 0.05),
              c("organ", "cell", "stat", "marker", "p.value")]
      write.table(
        significative,
        row.names = FALSE,
        file = here(path_result, file2),
        sep = ","
      )
      print(significative)
      significant_values <<- significative
    }
  }
