#' save_tidy
#'
#' @param table final raw table
#' @param path_output generated with eps::path_builder()
#'
#' @return
#' @export
#'
#' @examples
#' save_tidy(table, path_output)
#'
save_tidy <- function(table, path_output = path_output) {
  experiment <- str_extract(path_output, "\\d{2}.\\d{2}")
  write_csv(table, paste0(path_output, "/tidy", experiment, ".csv"))
}
