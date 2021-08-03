#' Generate an object matching miceID with their genotype
#'
#' Extract mice number and genotype from multiple files
#' animalario*.csv pattern as default download
#' micecode as union from Mote with Genotipado -> stored in data
#'
#' @param file_name Input file
#' @param path_raw path were the file is located. Usually in the experiment
#' subfolder inside raw
#' @param micecode Named chr list containing the replacement chr for the
#' genotype. BBV, CCT, DCX and DCW strains can be loaded
#' with \code{data(micecode)}
#'
#' @import here
#'
#' @return data.table with two column: one for the miceID and the other with
#' their genotype
#'
#' @export
#'
#'
#' @examples
#' data(micecode)
#' get_genotype("Animalario-VHL2101.csv", micecode)
#'



get_genotype <-
  function(file_name = c("^animalario", "$csv"),
           path_raw,
           micecode) {
    filenames <- list.files(pattern = c("^animalario", "$csv"), path = path_raw)
    filenames <- here::here(path_raw, filenames)
    dataset <-
      do.call("rbind", lapply(
        filenames,
        FUN = function(files) {
          read.delim(files, sep = ",", fileEncoding = "latin1")
        }
      ))
    dataset <-
      dataset[!apply(is.na(dataset) | dataset == "", 1, all), ]
    filtered_dataset <- dataset  %>%
      mutate(
        mice = Código,
        full_genotype = paste(Mote, Genotipado),
        genotype = str_replace_all(full_genotype, micecode)
      ) %>%
      select(mice, genotype)
    return(filtered_dataset)
  }
