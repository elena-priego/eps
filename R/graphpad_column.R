#' graphpad_column
#'
#' read a pzfx file containing column displayed data and convert it into a tidy table for further representation
#'
#' @param name_graphpad name of the file to be open. By default select the files with pzfx extension
#' @param sheet_n number of the sheet to be read
#' @param to_pivot columns selected to pivot
#' @param folder folder to look in. Partial pathway from here()
#' @param output_name name to be written the tidy_file. In folder data
#'
#' @import here
#' @import tidyverse
#' @import pzfx
#'
#' @return tidy table
#' @export csv file into data folder
#'
#' @examples
#' table <- graphpad_column(file)
#'
graphpad_column <-
  function(name_graphpad = "pzfx",
           sheet_n = 1,
           to_pivot = c(1, 2),
           folder = "",
           output_name = "table") {
    file <-
      list.files(name_graphpad,
                 path = here(folder),
                 full.names = TRUE)
    table.i <-
      pzfx::read_pzfx(file, table = sheet_n)
    table.i <-
      table.i[, to_pivot] %>%
      pivot_longer(everything(),
                   names_to = c("genotype"),
                   values_to = "value") %>%
      drop_na()
    write.csv(table.i,
              file = here(folder, data, paste0(output_name, ".csv")),
              row.names = FALSE)
    return(table.i)
  }



