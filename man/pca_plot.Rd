% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pca_plot.R
\name{pca_plot}
\alias{pca_plot}
\title{PCA plot
Dot plot to represent PCA data (from metabolomic data )}
\usage{
pca_plot(
  table,
  organ.i = NULL,
  experiment.i = NULL,
  stat.i = NULL,
  time.i = NULL,
  marker.i = NULL,
  cell.i = NULL,
  treatment.i = NULL,
  legend.i = "none",
  genotype_levels = c("WT", "KO"),
  genotype_labels = genotype_levels,
  title_lab = "",
  x_lab = "",
  y_lab = "",
  color_values = colorRamps::primary.colors(),
  fill_values = color_values
)
}
\arguments{
\item{table}{tidy table coming form facs_tidytable}

\item{organ.i}{optional organ selected to plot (specimen in .fcs file)}

\item{stat.i}{optional statistic selected to plot}

\item{time.i}{optional time selected to plot}

\item{marker.i}{optional marker selected to plot}

\item{cell.i}{optional cell selected to plot}

\item{treatment.i}{optional treatment selected to plot}

\item{legend.i}{possition of parameter legend}

\item{genotype_levels}{vector will all the genotypes all the analysis}

\item{genotype_labels}{name to be display in the legend. In markdown/html format.}

\item{title_lab}{title of the plot.}

\item{x_lab}{x-axis label}

\item{y_lab}{y-axis label}

\item{color_values}{a set of aesthetic values to map data values to.}

\item{fill_values}{fill color to be ploted. Same number as levels have genotype.}
}
\value{
plot
}
\description{
PCA plot
Dot plot to represent PCA data (from metabolomic data )
}
