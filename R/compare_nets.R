#' Plots Networks for Comparison
#' 
#' @description Uses \code{\link[SemNeT]{plot.SemNeT}} to plot networks.
#' Accepts any number of networks and will organize the plots
#' in the number of side-by-side columns using the heuristic of taking the square root of the number of 
#' input and rounding down to the nearest integer (i.e., \code{floor(sqrt(length(input)))}).
#' 
#' \strong{How plots will be automatically organized}
#' \itemize{
#' \item{3 networks:}
#' {1 x 3}
#' \item{6 networks:}
#' {2 x 3}
#' \item{9 networks:}
#' {3 x 3}
#' }
#' 
#' @param ... Matrices or data frames of network adjacency matrices
#' 
#' @param input_list List.
#' Bypasses \code{...} argument in favor of using a list
#' as an input
#' 
#' @param title Character vector.
#' Characters denoting titles of plots
#' 
#' @param plot.args List.
#' Arguments to be passed along to \code{\link[SemNeT]{plot.SemNeT}}
#' 
#' @return Plots networks
#' 
#' @examples
#' # Simulate Datasets
#' one <- sim.fluency(10)
#' two <- sim.fluency(10)
#' 
#' # Compute similarity matrix
#' cos1 <- similarity(one, method = "cosine")
#' cos2 <- similarity(two, method = "cosine")
#' 
#' # Compute networks
#' net1 <- TMFG(cos1)
#' net2 <- TMFG(cos2)
#' 
#' # Compare networks
#' compare_nets(
#'    net1, net2, title = c("One", "Two")
#' )
#' 
#' # Change node colors
#' compare_nets(
#'    net1, net2, title = c("One", "Two"),
#'    plot.args = list(node_color = "blue") 
#' )
#' 
#' # Change edge colors
#' compare_nets(
#'    net1, net2, title = c("One", "Two"),
#'    plot.args = list(edge_color = "blue") 
#' )
#' 
#' @references 
#' Epskamp, S., Cramer, A. O. J., Waldorp, L. J., Schmittmann, V. D., & Borsboom, D. (2012).
#' qgraph: Network visualizations of relationships in psychometric data.
#' \emph{Journal of Statistical Software}, \emph{48}, 1-18.
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#' 
# Compare Graphs
# Updated 21.04.2022
compare_nets <- function (
    ...,
    input_list = NULL,
    title = NULL,
    plot.args = list()
)
{
  
  # Check for input list
  if(is.null(input_list)){
    
    #Get names of networks
    name <- as.character(substitute(list(...)))
    name <- name[-which(name=="list")]
    
    #Create list of input
    datalist <- list(...)
    
  }else{
    
    # Get names of networks
    name <- names(input_list)
    
    if(is.null(name)){
      name <- 1:length(input_list)
    }
    
    # Create list of input
    datalist <- input_list
    
  }
  
  # Set for 'ggpubr' arrangement
  if(length(datalist) == 2){
    
    # Number of columns
    cols <- 2
    
    # Number of rows
    rows <- 1
    
  }else if(length(datalist) > 2){
    
    # Number of columns
    cols <- floor(sqrt(length(datalist)))
    
    # Number of rows
    rows <- ceiling(length(datalist) / len)

  }
  
  # Obtain default arguments for plot
  default.args <- formals(plot.SemNeT)
  
  # Remove "..."
  default.args <- default.args[-length(default.args)]
  
  # Check for plot arguments
  args.found <- names(plot.args) %in% names(default.args)
  
  if(any(args.found)){
    
    # Replace arguments
    default.args[names(plot.args)[args.found]] <- plot.args[args.found]
    
    # Check for layout
    if(length(default.args$layout) != 1){
      default.args$layout <- "qgraph_spring"
    }
    
  }
  
  # Make sure producing individual plots are not produced
  default.args$produce <- FALSE
  
  # Plot the networks
  network_plots <- lapply(datalist, function(x){
    
    # Set up `do.call`
    default.args$A <- x
    
    # Call `do.call`
    do.call(
      what = "plot.SemNeT",
      args = default.args
    )
    
  })
  
  # Set up title
  if(is.null(title)){
    title <- name
  }
  
  # Produce plot
  ggpubr::ggarrange(
    plotlist = network_plots,
    nrow = rows, ncol = cols,
    labels = title,
    label.x = 0.3
  )
  
}
