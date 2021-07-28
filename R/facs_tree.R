#' Generate a gatting tree from a FlowJo Analysis
#'
#' Plot showing the gatting hierarchy
#'
#' @param file_name Name of the .wps file.
#' @param path Path where the .wps file locates.
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
facs_tree <- function(file_name, path = here::here()) {
  dataDir <- path
  wsfile <- list.files(dataDir, pattern=file_name, full=TRUE)
  ws <- open_flowjo_xml(wsfile)
  #execute = FALSE prevent for looking into .fcs files
  gs <- flowjo_to_gatingset(ws, execute = FALSE) 
  plot(gs)
}

