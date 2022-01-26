#' graphpad_grouped
#'
#' Return a csv file into data folder
#'
#' @param name_graphpad name of the file to be open. By default select the files with pzfx extension
#' @param sheet_n number of the sheet to be read
#' @param folder folder to look in. Partial pathway from here()
#' @param output_name name to be written the tidy_file. In folder data
#' @param save conditional to decide if save the file
#'
#' @import here
#' @import tidyverse
#' @import pzfx
#'
#' @return tidy table
#' @export
#'
#' @examples
#' table <- graphpad_grouped(file)
#'
graphpad_grouped <- function(name_graphpad = "pzfx",
                             sheet_n = 1,
                             folder = "",
                             save = FALSE,
                             output_name = "table") {
  file <-
    list.files(name_graphpad,
               path = here(folder),
               full.names = TRUE)
  table.i <- pzfx::read_pzfx(file, table = sheet_n)
  tryCatch(
    expr = {
      table.i <- table.i %>%
        mutate_all(as.character) %>%
        pivot_longer(
          cols = -ROWTITLE,
          names_to = c("genotype", "mice"),
          names_pattern = "(.*)\\_(.*)",
          values_to = "value"
        ) %>%
        mutate(value = str_replace_all(value, ",", ".")) %>%
        mutate(
          mice = factor(mice),
          genotype = factor(genotype),
          value = as.double(value)
        ) %>%
        drop_na()
    },
    finally = {
      if (save == TRUE) {
        write.csv(
          table.i,
          quote = FALSE,
          file = here(folder, paste0(output_name, ".csv")),
          row.names = FALSE
        )
      }
    }
    ,
    error = function(cond) {
      message("There were an error. Tidying and saving must be done by hand. Error message:")
      message(cond)
    }
  )
  return(table.i)
}

