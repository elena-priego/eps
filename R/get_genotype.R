#' Generate an object matching miceID with their genotype
#'
#' Extract mice number and genotype from multiple files
#' animalario*.csv pattern as default download in english layout
#' NOTE: take a look to determine if the first row is indicating sep=";"
#' and remove in that case.
#' NOTE: if an error ocurrs remove the accent mark.
#' micecode as union from Nickname with Genotyping -> stored in data
#' Return a data.table with two column: one for the miceID and the other with
#' their genotype
#'
#' @param file_name Input file
#' @param path_raw path were the file is located. Usually in the experiment
#' subfolder inside raw
#' @param micecode Named chr list containing the replacement chr for the
#' genotype. BBV, CCT, DCX and DCW strains can be loaded
#' with \code{data(micecode)}
#' @param csv_sep separator for the csv file. Default to ","
#'
#' @import here
#' @return
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
           micecode,
           csv_sep = ",") {
    filenames <- list.files(file_name, path = path_raw)
    filenames <- here::here(path_raw, filenames)
    dataset <-
      do.call("rbind", lapply(
        filenames,
        FUN = function(files) {
          read.delim(files, sep = csv_sep)
        }
      ))
    if (ncol(dataset) == 1)
      table <- lapply(
        filenames,
        FUN = function(file) {
          tidy <- read_lines(file) %>%
            str_replace_all('"', '') %>%
            str_replace("sep=;", "") %>%
            str_replace_all(";", ",")
          tidy <- tidy[tidy != ""]
          write_lines(tidy, file)
        }
      )
    dataset <-
      do.call("rbind", lapply(
        filenames,
        FUN = function(files) {
          read.delim(files, sep = csv_sep)
        }
      ))
    dataset <-
      dataset[!apply(is.na(dataset) | dataset == "", 1, all),]
    filtered_dataset <- dataset  %>%
      mutate(
        mice = Code,
        full_genotype = paste(Nickname, Genotyping),
        genotype = str_replace_all(full_genotype, micecode)
      ) %>%
      select(mice, genotype)
    return(filtered_dataset)
  }
