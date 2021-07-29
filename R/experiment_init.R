#' Initialize the experiment
#'
#' Generate the folders associated to the experiment in data, raw and result.
#' Creation of the cover page in an Rmd to write the protocol and the analysis.
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
  dir.create(here::here("data", experiment_name), showWarnings = FALSE)
  dir.create(here::here("raw", experiment_name), showWarnings = FALSE)
  dir.create(here::here("result", experiment_name), showWarnings = FALSE)
  yalm <- paste(
    "---",
    paste0("title: ", experiment_name),
    "author: Elena Priego Saiz",
    paste0("date: ", Sys.Date()),
    "Short conclusion: ",
    "output: html_document",
    "---",
    "\n",
    paste0("# ", experiment_name),
    "\n",
    "## Aim",
    "\n",
    "## Method",
    "\n",
    "## Results",
    "\n",
    "```{r setup, include=FALSE}",
    "\n",
    "## global options",
    'options(encoding = "UTF-8")',
    "\n",
    "## packages that will be used",
    'library("eps")',
    "\n",
    "## paths",
    paste0('eps::path_builder("', experiment_name, '")'),
    "\n",
    "## rmarkdown settings",
    "knitr::opts_chunk$set(",
    "   message = FALSE,",
    "   warning = FALSE,",
    '   fig.align = "center",',
    '   fig.path = file.path(path_result, "plots"),',
    '   fig.pos = "H",',
    "   dpi = 300",
    ")",
    "\n",
    "```",
    "\n",
    "\n",
    "## Conclusions",
    sep = "\n"
  )
  cover <- paste0(here::here("doc", experiment_name), ".Rmd")
  write(yalm,
        file = cover,
        append = TRUE)
  file.edit(cover)
}

