% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/facs_boxplot.R, R/genotype_boxplot.R
\name{facs_boxplot}
\alias{facs_boxplot}
\title{Boxplot creation for cytometry data}
\usage{
facs_boxplot(
  table = "",
  organ.i = NULL,
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
  x_labels = waiver(),
  leyend_position = "top",
  color_values = colorRamps::primary.colors(),
  shape_values = rep(21, 200),
  fill_values = color_values,
  path_output = NULL,
  w = 10,
  h = 5,
  print_plot = TRUE
)

facs_boxplot(
  table = "",
  organ.i = NULL,
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
  x_labels = waiver(),
  leyend_position = "top",
  color_values = colorRamps::primary.colors(),
  shape_values = rep(21, 200),
  fill_values = color_values,
  path_output = NULL,
  w = 10,
  h = 5,
  print_plot = TRUE
)
}
\arguments{
\item{table}{tidy table with time, mice, genotype, treatment, marker, stat, value, experiment and cell columns}

\item{organ.i}{optional organ selected to plot (specimen in .fcs file)}

\item{stat.i}{optional statistic selected to plot}

\item{time.i}{optional time selected to plot}

\item{marker.i}{optional marker selected to plot}

\item{cell.i}{optional cell selected to plot}

\item{treatment.i}{optional treatment selected to plot}

\item{genotype_levels}{vector will all the genotypes all the analysis}

\item{genotype_labels}{name to be display in the legend. In markdown/html format.}

\item{x_value}{column name to plot}

\item{title_lab}{title of the plot}

\item{x_lab}{x-axis label}

\item{y_lab}{y-axis label}

\item{y_limit}{inferior limit for y-axis}

\item{y_trans}{transformation of y axis}

\item{x_angle}{angle of the labels of the x-axis.}

\item{x_hjust}{justification of the labels of the x-axis}

\item{x_labels}{labels for x axis}

\item{leyend_position}{Legend position. Default to top. Could also be right, left or bottom}

\item{color_values}{color to be ploted. Same number as levels have genotype .}

\item{shape_values}{shape to be ploted. Same number as levels have genotype.}

\item{fill_values}{fill color to be ploted. Same number as levels have genotype.}

\item{path_output}{Optional. Full file name desired (e.g. here(path_output, "plot.pdf"))}

\item{w}{width of the output plot}

\item{h}{high of the output plot}

\item{print_plot}{boolean indicating if the plot is printed or not. Default to TRUE.}

\item{jitter_width}{width of the points}

\item{plot_mean}{boolean to plot the mean}

\item{y_label}{default to waiver. Could be scientific_format()}
}
\value{
plot file in data folder

plot file in data folder
}
\description{
Boxplot generation for data created with facs_tidytable.
Design to work inside an apply function with all the possible combinations of
parameters in order to generate multiple plots (see examples).

Boxplot for tidy standard data by genotype
}
\examples{
comb <- as_tibble(unique(paste(table$organ, table$stat, table$marker,
                               sep = "_-_"))) \%>\%
        separate(value, into = c("organ", "stat", "marker"), sep = "_-_") \%>\%
        mutate(output = here(path_output,
                             paste0(organ, "_", stat, "_", marker, ".png")),
               y_lab = paste0(marker, " (", stat, ")")) \%>\% as.data.frame()

apply(comb, 1, function(x) facs_boxplot(table, organ.i = x[1], stat.i = x[2],
  marker.i = x[3], path_output = x[4], y_lab = x[5],
  title.i = x[1]))
genotype_violin(genotype_levels = VHL_table$genotypes,
color_values = VHL_table$palette_color,
shape_values = VHL_table$palette_shape,
fill_values = VHL_table$palette_fill)
}
