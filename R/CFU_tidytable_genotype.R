#' CFU_tidytable_genotype
#'
#' @param file .csv with CFU counts. Header should be label with Code for mice
#' column followed by the different dilutions included in the experiment written
#' written in the format "Dil-1/dilution"
#' @param path_file path where file is located. Usually path_output from
#' path_builder()
#' @param animalario_file raw csv downloaded from animalario with mice used in
#'  the experiment
#' @param path_mice path where animalario file to obtain the genotypes is
#' located. usually path_raw from path_builder.
#' @param micecode named list with the replacement for the genotypes.
#' Load from micecode data included in the package.
#' @param animalario_sep separator for the animalario csv file. Default to ","
#'
#' @import tidyverse
#' @import here
#'
#' @return a tibble with the tidy format of the CFU
#' @export
#'
#' @examples
#' data(micecode)
#' CFU_tidytable_genotype("CFU.csv",  path_file = path_raw, animalario_file = "animalario.csv",
#'     gate_pattern = gate_pattern, micecode = micecode)
#'
#'
CFU_tidytable_genotype <-
  function(file = c("^CFU", "$csv"),
           path_file = path_raw,
           animalario_file = c("^animalario", "$csv"),
           path_mice = path_raw,
           micecode = micecode,
           animalario_sep = ",") {
    file <- list.files(file, path = path_file)
    file <- here::here(path_file, file)
    table <-
      do.call("rbind", lapply(
        file,
        FUN = function(files) {
          table <- read_csv(file)
          table <- sapply(table[], function(y)
            as.character(y))
          table <- as_tibble(table)
          table_full <- table %>%
            pivot_longer(cols = -"Code",
                         names_to = "dilution",
                         values_to = "value") %>%
            mutate(value = str_replace_all(value, "0", "")) %>%
            mutate(dilution = lapply(dilution, function(y)
              sub("Dil-", "", y)),
              value = as.double(value)) %>%
            mutate(dilution = lapply(dilution, function(y)
              gsub(",", "", y))) %>%
            mutate(dilution = lapply(dilution, function(y)
              sub("1/", "", y))) %>%
            drop_na() %>%
            mutate(CFU = as.double(dilution) * value) %>%
            pivot_wider(id_cols = Code,
                        names_from = dilution,
                        values_from = CFU)
          table_full %>%
            mutate(mean_CFU = rowMeans(table_full[,-1], na.rm = TRUE)) %>%
            select(Code, mean_CFU) %>%
            rename(mice = Code)
        }
      ))
    genotypes <-
      get_genotype(
        file_name = animalario_file,
        path_raw = path_mice,
        micecode,
        csv_sep = animalario_sep
      )
    table <- left_join(table, genotypes, by = "mice") %>%
      mutate(mice = as.factor(mice),
             genotype = as.factor(genotype))
    return(table)
  }
