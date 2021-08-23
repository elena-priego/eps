#' weight_csv_read
#' function to read the csv were the mice weight is storage. Mice are
#' represented in rows whereas days are represented in columns. To use inside
#' weight_tidytable function to also get the genotype and make ID as factors.
#'
#'
#' @param csv_file name of the csv file with the introduced weights.
#' Usually "weight-curve.csv"
#' @param path_csv path were csv file is located. Usually path_output
#' @param csv_sep sep parameter for read.delim function. Default ","
#' @param date_format format parameter of strftime function. Character string.
#' The default for the format methods is "%d.%b" that is for format "01.jul".
#' Other common format are %d-%m
#'
#' @import tidyverse
#' @import here
#'
#' @return
#' @export
#'
#' @examples weight_csv_read("weight-curve.csv", path_output)
#'
#'
#'
weight_csv_read <-
  function(csv_file,
           path_csv,
           csv_sep = ",",
           date_format = "%d.%b") {
    path_file <- here::here(path_csv, csv_file)
    table <- read.delim(file = path_file, sep = csv_sep) %>%
      pivot_longer(cols = -"ID",
                   names_to = "day",
                   values_to = "value") %>%
      mutate(day = str_replace_all(day, "X", "")) %>%
      mutate(day = strptime(day, date_format))
  }
