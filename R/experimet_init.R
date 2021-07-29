#' Initialize the experiment
#'
#' Generate the folders associated to the experiment in data, raw and result and
#' start an Rmd to write the protocol and the analysis. Should be used directly
#' in console within the project folder
#'
#' @param experiment_name Full name of the experiment
#'
#' @return
#' @export
#'
#' @examples experiment_init("VHL-2101_experiment")
#'
#'
experiment_init <- function(experiment_name) {
  dir.create(here::here("data", experiment_name), showWarnings = FALSE)
  dir.create(here::here("raw", experiment_name), showWarnings = FALSE)
  dir.create(here::here("result", experiment_name), showWarnings = FALSE)
  yalm <- paste(
    "---",
    paste0("title: ", experiment_name),
    paste0("date: ", Sys.Date()),
    "---",
    "\n",
    paste0("path_builder(", experiment_name, "),"),
    "# Heading 1",
    sep = "\n"
  )
  write(yalm,
        file = here::here("doc", paste0(experiment_name, ".Rmd")),
        append = TRUE)
}
