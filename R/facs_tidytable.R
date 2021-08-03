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
#' @param path_data path where file is located. Usually path_data from
#' path_builder()
#' @param animalario_file raw csv downloaded from animalario with mice used in
#'  the experiment
#' @param gate_pattern named list with the replacements desired for the gates.
#' Load from gate_pattern data included in the package.
#' Common ones are: c("Lymphocytes/Single Cells/Single Cells/CD452/" = "",
#' "Freq. of Parent" = "Freq.", "Freq. of Grandparent" = "Freq.",
#' "Geometric Mean" = "GMFI", "Median" = "MdFI", "\\)" = "")
#' @param path_raw path where animalario file to obtain the genotypes is
#' located. usually path_raw from path_builder.
#' @param micecode named list with the replacement for the genotypes.
#' Load from micecode data included in the package.

#'
#' @import readxl
#' @import tidyverse
#' @import usethis
#' @import here
#'
#' @return a tibble with the tidy format
#' @export
#'
#' @examples
#' data(gate_pattern)
#' data(micecode)
#' facs_tidytable("table.xls", path_data, "animalario.csv",
#'     gate_pattern = gate_pattern, micecode = micecode)
#'
#'
facs_tidytable <-
  function(file,
           path_data,
           animalario_file,
           gate_pattern,
           path_raw,
           micecode) {
    file <- "Table.xls"
    file <- here::here(path_data, file)
    tidy <- readxl::read_excel(file)
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
      mutate_all(trimws) %>%
      mutate(
        value = as.numeric(value),
        organ = as.factor(organ),
        mice = as.factor(mice),
        cell = as.factor(cell),
        stat = as.factor(stat),
        marker = as.factor(marker)
      )
    genotype <- get_genotype(animalario_file, path_raw, micecode)
    tidy <- left_join(tidy, genotype, by = "mice")
    return(tidy)
  }

