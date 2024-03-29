% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mean_plot.R
\name{mean_plot}
\alias{mean_plot}
\title{Curve with mean and SE
plot a weight-loss curve with the mean value of each genotype and genotype}
\usage{
mean_plot(
  table,
  organ.i = NULL,
  experiment.i = NULL,
  stat.i = NULL,
  time.i = NULL,
  marker.i = NULL,
  cell.i = NULL,
  treatment.i = NULL,
  genotype_levels = c("WT", "KO"),
  genotype_labels = genotype_levels,
  x_value = "cell",
  title_lab = "",
  x_lab = "",
  y_lab = "",
  y_limit = 0,
  y_trans = "identity",
  x_angle = 45,
  x_hjust = 1,
  leyend_position = "right",
  color_values = colorRamps::primary.colors(),
  shape_values = rep(21, 200),
  fill_values = color_values,
  lty_values = color_values,
  color_labels = waiver(),
  path_output = NULL,
  w = 10,
  h = 5,
  print_plot = FALSE
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

\item{genotype_levels}{vector will all the genotypes all the analysis}

\item{genotype_labels}{name to be display in the legend. In markdown/html format.}

\item{x_value}{column name to plot}

\item{title_lab}{title of the plot.}

\item{x_lab}{x-axis label}

\item{y_lab}{y-axis label}

\item{y_limit}{inferior limit for y-axis}

\item{x_angle}{angle of the labels of the x-axis.}

\item{x_hjust}{justification of the labels of the x-axis}

\item{leyend_position}{Legend position. Default to top. Could also be right, left or bottom}

\item{color_values}{a set of aesthetic values to map data values to.
The values will be matched in order (usually alphabetical).}

\item{shape_values}{shape to be ploted. Same number as levels have genotype.}

\item{fill_values}{fill color to be ploted. Same number as levels have genotype.}

\item{lty_values}{color to fill the lines}

\item{color_labels}{takes the breaks as input and returns labels as output}

\item{path_output}{ful name of the generated plot including the path
(recommended path_output from path_builder())}

\item{w}{width of the output plot}

\item{h}{high of the output plot}

\item{print_plot}{boolean indicating if the plot is printed or not. Default to FALSE.}

\item{color_breaks}{takes the limits as input and returns breaks as output}
}
\value{
plot file in data folder
}
\description{
Curve with mean and SE
plot a weight-loss curve with the mean value of each genotype and genotype
}
\examples{
weight_mean_plot(table_raw, y_limit = 10,
path_output = here(path_output, "mean_raw.png"))

}
