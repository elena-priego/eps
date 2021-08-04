#' Initialize the experiment creating the needed pathways
#'
#' Generate the pathways to save the files of the projects and create specific
#' folders in data, raw and output. The pathways are saved in R environment to
#' be used along the analysis
#'
#'
#' @param experiment_name Full name of the experiment
#'
#' @import here
#'
#' @return
#' @export
#'
#' @examples path_builder("VHL-2101_experiment")
#'
#'
path_builder <- function(experiment_name){
  path_data <<- here::here("data", experiment_name)
  dir.create(path_data, showWarnings = FALSE)
  path_doc <<- here::here("doc")
  path_raw <<- here::here("raw", experiment_name)
  dir.create(path_raw, showWarnings = FALSE)
  path_output <<- here::here("output", experiment_name)
  dir.create(path_output, showWarnings = FALSE)
  path_src <<- here::here("src")
}

