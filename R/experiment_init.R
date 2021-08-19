#' Initialize the experiment
#'
#' Generate the folders associated to the experiment in data, raw and output and
#' creation of the cover page in an Rmd to write the protocol and the analysis.
#' Should be used directly in the console within the project folder
#'
#' @param experiment_name Full name of the experiment. Should have the following
#' structure: project, "hash", year, experiment number, "underscore",
#' experiment title
#'
#' @return
#' @export
#'
#' @examples experiment_init("VHL-2101_experiment")
#'
#'
experiment_init <- function(experiment_name) {
  eps::path_builder(experiment_name)
  eps::cover_init(experiment_name)
}
