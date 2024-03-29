% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/facs_tree.R
\name{facs_tree}
\alias{facs_tree}
\title{Generate a gatting tree from a FlowJo Analysis}
\usage{
facs_tree(file_pattern = "*.wsp", path_data = path_data, group = "All Samples")
}
\arguments{
\item{file_pattern}{Name of the .wps file. As default all the analysis are
taken}

\item{path_data}{Path where the .wsp file locates.}

\item{group}{analysis group to be plotted. As defaul all Samples (gate 1)}
}
\description{
Plot showing the gatting hierarchy
}
\examples{
facs_tree("C:/Users/elena/Desktop/working on", "analysis.wps")


}
