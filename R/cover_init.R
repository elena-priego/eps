#' Generation of the experiment cover in Rmd
#'
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
#' @examples cover_init("VHL-2101_experiment")
#'
#'
cover_init <- function(experiment_name) {
  yalm <- paste(
    "---",
    paste0("title: ", experiment_name),
    "author: Elena Priego Saiz",
    paste0("date: ", Sys.Date()),
    "output:",
    "  prettydoc::html_pretty:",
    "   theme: cayman",
    "   highlight: github",
    "   toc: true",
    'toc-title: "Table of Contents"',
    "editor_options:",
    "  chunk_output_type: console",
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
    'library(eps)',
    "library(here)",
    "library(broom)",
    "library(tidyverse)",
    "library(cowplot)",
    "library(ggthemes)",
    "library(openxlsx)",
    "library(cowplot)",
    "library(scales)",
    "library(ggpubr)",
    "\n",
    "## paths",
    paste0('eps::path_builder("', experiment_name, '")'),
    "\n",
    "## rmarkdown settings",
    "knitr::opts_chunk$set(",
    "   message = FALSE,",
    "   warning = FALSE,",
    '   fig.align = "center",',
    '   fig.path = "data/",',
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
  cover <-
    paste0(here::here(experiment_name, paste0(experiment_name, ".Rmd")))
  write(yalm,
        file = cover,
        append = TRUE)
  file.edit(cover)
}

