#' @title Shiny App for \code{\link{SemNeT}}
#'
#' @description Launches an interactive Shiny application for semantic
#' network analysis, covering network estimation, comparison, bootstrap and
#' permutation testing, random walk simulation, and spreading activation.
#' Results are retrieved via the app's own download buttons (as \code{.rds}/
#' \code{.csv}/image files), not returned to the R session that launched it --
#' each browser session keeps its own results independently, so the app can
#' be run locally or shared with multiple simultaneous users
#'
#' @return Invisibly returns \code{NULL}. Launches the app as a side effect;
#' see the app's own "Export" tab for how to retrieve results
#'
#' @examples
#' if(interactive()){
#'   SemNeTShiny()
#' }
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Launches the SemNeT Shiny app
# Updated 07.09.2026
SemNeTShiny <- function()
{

  # The Shiny app's dependencies are all `Suggests`, not `Imports` -- the
  # core package must keep loading without any of them installed, so they're
  # only checked here, right before the app that actually needs them starts
  required_packages <- c(
    "shiny", "shinyjs", "shinyalert", "DT", "promises",
    "shinyMatrix", "shinyBS", "spreadr", "qgraph"
  )

  missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

  if(length(missing_packages) > 0){
    stop(
      "In `SemNeTShiny()`: the following packages are required to run the Shiny app but aren't installed: ",
      paste0(missing_packages, collapse = ", "), ".\n",
      "Install them with `install.packages(c(", paste0("\"", missing_packages, "\"", collapse = ", "), "))`.",
      call. = FALSE
    )
  }

  shiny::runApp(appDir = system.file("Shiny", package = "SemNeT"))

  return(invisible(NULL))

}
