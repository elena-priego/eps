% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/experiment_init.R
\name{experiment_init}
\alias{experiment_init}
\title{Initialize the experiment}
\usage{
experiment_init(experiment_name)
}
\arguments{
\item{experiment_name}{Full name of the experiment. Should have the following
structure: project, "hash", year, experiment number, "underscore",
experiment title}
}
\description{
Generate the folders associated to the experiment in data, raw and output and
creation of the cover page in an Rmd to write the protocol and the analysis.
Should be used directly in the console within the project folder
}
\examples{
experiment_init("VHL-2101_experiment")


}
