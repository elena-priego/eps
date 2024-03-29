#' weight_tidytable
#'
#' @param csv_file name of the csv file with the introduced weights.
#' Usually "weight-curve.csv"
#' @param path_csv path were csv file is located. Usually path_output
#' @param relative bool indicating if the final output will be raw number or
#' the relative porcentage to day 1
#' @param csv_sep sep parameter for read.delim function. Default ","
#' @param date_format format parameter of strftime function. Character string.
#' The default for the format methods is "%d.%b" that is for format "01.jul".
#' Other common format are %d-%m
#' @param animalario_file raw csv downloaded from animalario with mice used in
#'  the experiment
#' @param mice_genotype ordered list containing the levels of the mice to be
#' included. Can be obtain from micecode data (followed by /t).
#' For example c("VHL-HIF2a-WT  ", "VHL-HIF2a-KO  ")
#' @param path_mice path where animalario file to obtain the genotypes is
#' located. usually path_raw from path_builder.
#' @param micecode Named chr list containing the replacement chr for the
#' genotype. BBV, CCT, DCX and DCW strains can be loaded
#' with \code{data(micecode)}
#'
#' @import tidyverse
#' @import here
#'
#' @return
#' @export
#'
#' @examples
#' data(micecode)
#' weight_tidytable("weight-curve.csv", path_output,
#' "Animalario-VHL2101.csv", micecode)
#'

weight_tidytable <- function(csv_file,
                             path_csv,
                             animalario_file,
                             path_mice,
                             micecode,
                             mice_genotype,
                             relative = FALSE,
                             csv_sep = ",",
                             date_format = "%d.%b") {
  tidy_table <-
    weight_csv_read(
      csv_file,
      path_csv = path_csv,
      relative = relative,
      csv_sep = csv_sep,
      date_format = date_format
    )
  genotype_table <-
    get_genotype(animalario_file, path_mice, micecode)
  joined_table <-
    left_join(tidy_table, genotype_table, by = "mice") %>%
    mutate(
      mice = as.factor(mice),
      genotype = factor(genotype,
                        levels = mice_genotype,
                        ordered = TRUE)
    )
  return(joined_table)
}



