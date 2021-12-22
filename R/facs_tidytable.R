####facs_tidytable####

#' Prepare the data in a tidy format from the data obtained in Flowjo
#'
#'
#' Generation of a tidytable from the .xls generated from Flowjo.
#' It's important to have the tubes correctly labelled: specimen should have a
#' descriptive name without using "_" and each tube should be named using ONLY
#' the full name of the mice.
#'
#' @param file .xls generated from Flowjo with cell percentages and fluorescent
#'  intensities
#' @param path_file path where file is located. Usually path_output from
#' path_builder()
#' @param gate_pattern named list with the replacements desired for the gates.
#' Load from gate_pattern data included in the package.
#' Common ones are: c("Freq. of Parent" = "Freq.",
#' "Freq. of Grandparent" = "Freq.",
#' "Geometric Mean" = "GMFI", "Median" = "MdFI", "\\)" = "")

#'
#' @import readxl
#' @import tidyverse
#' @import here
#'
#' @return a tibble with the tidy format
#' @export
#'
#' @examples
#' data(gate_pattern)
#' facs_tidytable("table.xls", path_file = path_output,
#'     gate_pattern = gate_pattern)
#'
#'
facs_tidytable <-
  function(file = c("^Table", "$csv"),
           path_file = path_output,
           gate_pattern) {
    file <- list.files(file, path = path_file)
    file <- here::here(path_file, file)
    table <-
      do.call("rbind", lapply(
        file,
        FUN = function(files) {
          tidy <- readxl::read_excel(files)
          tidy <- sapply(tidy[], function(y)
            as.character(y))
          tidy <- as_tibble(tidy)
          tidy <- tidy[!(tidy[, 1] == "Mean" | tidy[, 1] == "SD"), ]
          freq <- grep("(.*) Freq. (.*)", names(tidy))
          tidy[freq] <- lapply(tidy[freq], function(y)
            sub("%", "", y))
          names(tidy) <- str_replace_all(names(tidy), gate_pattern)
          tidy <- tidy %>%  pivot_longer(cols = -"...1",
                                         names_to = "statistic",
                                         values_to = "value") %>%
            separate("...1",
                     into = c("organ", "mice"),
                     sep = "_") %>%
            separate("statistic",
                     into = c("cell", "stat2"),
                     sep = "\\|") %>%
            separate("stat2", into = c("stat", "marker"), sep = "\\(") %>%
            mutate(marker = replace_na(marker, "freq")) %>%
            mutate(mice = str_replace_all(mice, ".fcs", "")) %>%
            mutate(cell = sub(".*/", "", cell)) %>%
            mutate_all(trimws)
        }))
    return(table)
  }

