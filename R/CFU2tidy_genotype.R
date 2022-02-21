#' CFU to tidy
#'
#' Function to use when the dilution is already calculated. Standard output. Can read multiple csv at the same time, but is better to do it separately inicating the different times. When multiple, write _M_. When no colonies, write 0.
#'
#' @param file .csv with CFU counts. Header should be label with Code for mice
#' column followed by the different dilutions included in the experiment written
#' written in the format "Dil-dilution". Example: in dil 1/100 (in 2mL) will be 200
#' Write NA in raw file when can no count
#' @param path_file path where file is located. Usually path_output from
#' path_builder()
#' @param animalario_file raw csv downloaded from animalario with mice used in
#'  the experiment
#' @param path_mice path where animalario file to obtain the genotypes is
#' located. usually path_raw from path_builder.
#' @param micecode named list with the replacement for the genotypes.
#' Load from micecode data included in the package.
#' @param animalario_sep separator for the animalario csv file. Default to ","
#' @param multiple Number to replace when the CFU where incontable. Represented by M in the original csv
#' @param marker usually CFU (by deffault)
#' @param cell Am. Indicate if they are from lung, BAL, ex vivo...
#' @param time Time after infection. Default to 0h
#' @param treatment Moi of infection
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
CFU2tidy_genotype <-
  function(file = c("^CFU", "$csv"),
           path_file = path_raw,
           animalario_file = c("^animalario", "$csv"),
           path_mice = path_raw,
           micecode = micecode,
           animalario_sep = ",",
           multiple = "100",
           marker = "CFU",
           cell = "AM",
           time = "0h",
           treatment = "lp moi") {
    file <- list.files(file, path = path_file)
    file <- here::here(path_file, file)
    table <-
      do.call("rbind", lapply(
        file,
        FUN = function(files) {
          table <- read_csv(file, show_col_types = FALSE)
          table <- sapply(table[], function(y)
            as.character(y))
          table <- as_tibble(table) %>%
            pivot_longer(cols = -"Code",
                         names_to = "stat",
                         values_to = "raw_CFU") %>%
            mutate(raw_CFU = str_replace_all(raw_CFU, "M", multiple)) %>%
            mutate(dilution = lapply(stat, function(y)
              sub("Dil-", "", y)),
              raw_CFU = as.double(raw_CFU)) %>%
            mutate(value = as.double(dilution) * raw_CFU) %>%
            select(-c(raw_CFU, dilution)) %>%
            pivot_wider(id_cols = Code,
                        names_from = stat,
                        values_from = value) %>%
            mutate(mean_CFU = rowMeans(select(., -Code), na.rm = TRUE)) %>%
            pivot_longer(cols = -"Code",
                         names_to = "stat",
                         values_to = "value") %>%
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
      mutate(time = as.factor(time),
             mice = as.factor(mice),
             genotype = as.factor(genotype),
             treatment = as.factor(treatment),
             marker = as.factor(marker),
             stat = as.factor(stat),
             value = as.double(value),
             experiment = as.factor(str_extract(path_output, "\\d{2}.\\d{2}")),
             cell = as.factor(cell)) %>%
      drop_na()
    return(table)
  }
