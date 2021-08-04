#' Generate a gatting tree from a FlowJo Analysis
#'
#' Plot showing the gatting hierarchy
#'
#' @param file_pattern Name of the .wps file. As default all the analysis are
#' taken
#' @param path_data Path where the .wsp file locates.
#' @param group analysis group to be plotted. As defaul all Samples (gate 1)
#'
#'
#' @import CytoML
#' @import flowWorkspace
#'
#' @return
#' @export
#'
#' @examples facs_tree("C:/Users/elena/Desktop/working on", "analysis.wps")
#'
#'
facs_tree <- function(file_pattern = "*.wsp",
                      path_data = path_data,
                      group = "All Samples"
) {
  wsfile <- list.files(path_data, pattern=file_pattern, full=TRUE)
  ws <- open_flowjo_xml(wsfile)
  #execute = FALSE prevent for looking into .fcs files
  gs <- flowjo_to_gatingset(ws, name = group, execute = FALSE)
  plot(gs)
}

