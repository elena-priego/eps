####facs_tidytable_genotype####

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
#' @param animalario_file raw csv downloaded from animalario with mice used in
#'  the experiment
#' @param gate_pattern named list with the replacements desired for the gates.
#' Load from gate_pattern data included in the package.
#' Common ones are: c("Freq. of Parent" = "Freq.",
#' "Freq. of Grandparent" = "Freq.",
#' "Geometric Mean" = "GMFI", "Median" = "MdFI", "\\)" = "")
#' @param path_mice path where animalario file to obtain the genotypes is
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
#' facs_tidytable_genotype("table.xls", path_data, "animalario.csv",
#'     gate_pattern = gate_pattern, micecode = micecode)
#'
#'
facs_tidytable_genotype <-
  function(file = c("^Table", "$csv"),
           path_file = path_output,
           animalario_file = c("^animalario", "$csv"),
           gate_pattern,
           path_mice = path_raw,
           micecode,
           animalario_sep = ",") {
    tidy <- facs_tidytable(file, path_file = path_file,
         gate_pattern = gate_pattern)
    genotype <- get_genotype(file_name = animalario_file,
                             path_raw = path_mice,
                             micecode = micecode,
                             csv_sep = animalario_sep)
    tidy <- left_join(tidy, genotype, by = "mice")
    tidy <- tidy %>%
      mutate(
        value = as.numeric(value),
        organ = as.factor(organ),
        mice = as.factor(mice),
        cell = as.factor(cell),
        stat = as.factor(stat),
        marker = as.factor(marker),
        genotype = as.factor(genotype)
      )
    return(tidy)
  }

