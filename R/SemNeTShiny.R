#' @title Shiny App for \code{\link{SemNeT}}
#' 
#' @description An interactive Shiny application for running \code{\link{SemNeT}} analysis.
#' 
#' @return A list called \code{resultShiny} containing:
#' 
#' \item{data}{The data imported into \code{\link[SemNeT]{SemNetShiny}}}
#' 
#' \item{group}{The grouping variable imported into \code{\link[SemNeT]{SemNetShiny}}}
#' 
#' \item{network}{The networks generated during \code{\link[SemNeT]{SemNetShiny}} session.
#' The networks are labelled using the provided grouping variable}
#' 
#' \item{measures}{Network measures ASPL (Average Shortest Path Lengths),
#' CC (Clustering Coefficient), and Q (Modularity) for the networks generated
#' during \code{\link[SemNeT]{SemNetShiny}}}
#' 
#' \item{comparePlot}{A visualization of the networks generated during \code{\link[SemNeT]{SemNetShiny}}}
#' 
#' \item{randomTest}{Statistical results from the Random Network Test in \code{\link[SemNeT]{SemNetShiny}}
#' (see \code{\link[SemNeT]{randnet.test}})}
#' 
#' \item{bootstrap}{Results from the Bootstrap Network Analysis in \code{\link[SemNeT]{SemNetShiny}}
#' (see \code{\link[SemNeT]{bootSemNeT}})}
#' 
#' \item{bootstrapTest}{Statistical results from the Bootstrap Network Analysis
#' (see \code{\link[SemNeT]{test.bootSemNeT}})}
#' 
#' \item{bootstrapPlot}{Plots of the statistical results from the Bootstrap Network Analysis
#' (see \code{\link[SemNeT]{plot.bootSemNeT}})}
#' 
#' @examples
#' 
#' if(interactive())
#' {SemNeTShiny()}
#' 
#' 
#' @export
# SemNeT Shiny App----
# Updated 19.04.2020
SemNeTShiny <- function()
{
  shiny::runApp(appDir = system.file("Shiny", package="SemNeT"))
}