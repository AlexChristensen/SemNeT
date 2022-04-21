#' Plots a Single Network
#' 
#' @description Uses \code{\link[GGally]{ggnet2}} to plot networks
#' 
#' @param x Matrix or data frame.
#' An adjacency matrix of a network
#' 
#' @param weighted Boolean.
#' Should networks be plotted with weights?
#' Defaults to \code{FALSE}.
#' Set to \code{TRUE} to plot networks with weights
#' corresponding to association strength
#' 
#' @param node_size Numeric.
#' Size of the nodes in the network.
#' Defaults to \code{7}
#' 
#' @param node_color Character or hex code.
#' Color of the nodes in the network.
#' Defaults to \code{"#EFB0A1"}
#' 
#' @param labels Boolean.
#' Should node labels be used?
#' Defaults to \code{FALSE}.
#' Labels may add noise and distract from understanding
#' the broader network structures.
#' Set to \code{TRUE} to plot labels.
#' Use other label arguments to get them the way you want
#' 
#' @param label_size Numeric.
#' Size of labels.
#' Defaults to \code{4.5}
#' 
#' @param edge_size Numeric.
#' Size of edges in the network.
#' Defaults to \code{1}
#' 
#' @param edge_color Character or hex code.
#' Color of the edges in the network.
#' Defaults to \code{"grey"}
#' 
#' @param positive_edge_color Character or hex code.
#' If \code{weighted = TRUE}, then positive and negative
#' edges will be different colors. This argument sets
#' the color of the positively signed edges.
#' Defaults to \code{"green"}
#' 
#' @param negative_edge_color Character or hex code.
#' If \code{weighted = TRUE}, then positive and negative
#' edges will be different colors. This argument sets
#' the color of the negatively signed edges.
#' Defaults to \code{"red"}
#' 
#' @param layout Character.
#' Changes the layout of the nodes in the network.
#' Defaults to \code{"qgraph_spring"}, which uses
#' a version of the Fruchterman-Reingold algorithm 
#' (see \code{\link[qgraph]{qgraph.layout.fruchtermanreingold}}).
#' Other options are taken from \code{\link[sna]{gplot.layout}}
#' 
#' @param layout_args List.
#' Takes in arguments for \code{layout}.
#' See \code{\link[sna]{gplot.layout}} for more information
#' 
#' @param expand_plot Positive numbers.
#' Sometimes plots will cut of node labels. This argument
#' can "expand" the plot so that node labels are not cut-off.
#' Defaults to \code{0} or no adjustment.
#' Use positive numbers to increase the space available for the
#' network to be plot
#' 
#' @param seed Integer.
#' Sets seed to have reproducible layouts.
#' Defaults to \code{1234}.
#' Use different numbers to switch up the layout
#' (might not change some layouts)
#' 
#' @param produce Boolean.
#' Should plot be produced?
#' Defaults to \code{TRUE}.
#' Used mainly for other functions (e.g., \code{\link{compare_nets}})
#' 
#' @param ... Additional arguments for \code{\link[GGally]{ggnet2}}
#' 
#' @return Plots networks using \code{\link[GGally]{ggnet2}}
#' 
#' @examples
#' # Simulate Datasets
#' one <- sim.fluency(10)
#' 
#' # Compute similarity matrix
#' cos1 <- similarity(one, method = "cosine")
#' 
#' # Compute networks
#' net1 <- TMFG(cos1)
#' 
#' # Compare networks
#' plot(net1)
#' 
#' # Change node size
#' plot(net1, node_size = 14)
#' 
#' @references 
#' Epskamp, S., Cramer, A. O. J., Waldorp, L. J., Schmittmann, V. D., & Borsboom, D. (2012).
#' qgraph: Network visualizations of relationships in psychometric data.
#' \emph{Journal of Statistical Software}, \emph{48}, 1-18.
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom graphics layout
#' 
#' @export
# Plot SemNeT
# Updated 21.04.2022
plot.SemNeT <- function(
    x, weighted = FALSE,
    node_size = 7, node_color = "#EFB0A1",
    labels = FALSE, label_size = 4.5,
    edge_size = 1, edge_color = "grey",
    positive_edge_color = "green",
    negative_edge_color = "red",
    layout = c(
      "qgraph_spring", "eigen",
      "fruchtermanreingold",
      "geodist", "mds",
      "princoord", "spring",
      "springrepulse"
    ),
    layout_args = list(),
    expand_plot = 0, seed = 1234,
    produce = TRUE,
    ...
)
{
  
  # Set default for layout
  if(missing(layout)){
    layout <- "qgraph_spring"
  }else{
    layout <- tolower(match.arg(layout))
  }
  
  # Insignificant values (keeps ggnet2 from erroring out)
  x <- ifelse(abs(as.matrix(x)) <= .00001, 0, as.matrix(x))
  
  # Convert to 'network' object
  A_network <- network::network(
    x, ignore.eval = FALSE,
    names.eval = "weights", directed = FALSE
  )
  
  # Set node names
  network::set.vertex.attribute(A_network, attrname= "Names", value = network::network.vertex.names(A_network))
  
  # Set node color
  if(isTRUE(weighted)){
    
    # Set positive and negative edge weight colors
    network::set.edge.attribute(
      A_network, "color",
      ifelse(
        network::get.edge.value(A_network, "weights") > 0,
        positive_edge_color,
        negative_edge_color
      )
    )
    
  }else{
    
    # Set edge weight color to black
    network::set.edge.attribute(A_network, "color", edge_color)
    
    # Binarize weights
    x <- binarize(x)
    
  }
  
  # Set edge values
  network::set.edge.value(A_network, attrname = "AbsWeights", value = abs(x))
  
  # Scale edge weight size
  network::set.edge.value(
    A_network, attrname = "ScaledWeights",
    value = matrix(
      rescale.edges(x, edge_size),
      nrow = nrow(x),
      ncol = ncol(x)
    )
  )
  
  # Set seed for spring
  set.seed(seed)
  
  # Set layout to "spring"
  if(layout == "qgraph_spring"){
    
    graph <- convert2igraph(x)
    edge_list <- igraph::as_edgelist(graph)
    graph_layout <- qgraph::qgraph.layout.fruchtermanreingold(
      edgelist = edge_list,
      weights = abs(igraph::E(graph)$weight / max(abs(igraph::E(graph)$weight)))^2,
      vcount = ncol(x)
    )
    
  }else{
    
    # Obtain layout function
    layout_function <- switch(
      layout,
      "eigen" = sna::gplot.layout.eigen,
      "fruchtermanreingold" = sna::gplot.layout.fruchtermanreingold,
      "geodist" = sna::gplot.layout.geodist,
      "mds" = sna::gplot.layout.mds,
      "princoord" = sna::gplot.layout.princoord,
      "spring" = sna::gplot.layout.spring,
      "springrepulse" = sna::gplot.layout.springrepulse
    )
    
    # Assign adjacency matrix to layout arguments
    sna_layout <- list()
    sna_layout$d <- x
    sna_layout$layout.par <- layout_args
    
    # Obtain layout
    graph_layout <- do.call(
      "layout_function",
      sna_layout
    )
    
  }
  
  # Set edge alpha
  lower <- abs(x[lower.tri(x)])
  non_zero <- sqrt(lower[lower != 0])
  
  # Set up plot
  semnet_plot <- GGally::ggnet2(
    net = A_network, mode = graph_layout,
    alpha = .80, color = "white",
    shape = 19, node.size = 0,
    edge.color = "color", edge.size = "ScaledWeights",
    edge.alpha = non_zero, label = colnames(x),
    node.label = rep("", ncol(x)), label.size = label_size,
    layout.exp = expand_plot, ...
  ) +
    ggplot2::theme(legend.title = ggplot2::element_blank())
  
  # Custom nodes: transparent insides and dark borders
  semnet_plot <- semnet_plot +
    ggplot2::geom_point(
      size = node_size,
      color = "#333333", fill = node_color,
      shape = 21, stroke = 1.5
    )
  
  # Check whether labels should be there
  if(isTRUE(labels)){
    
    semnet_plot <- semnet_plot +
      ggplot2::geom_text(
        ggplot2::aes(label = colnames(x)),
        size = label_size
      )
    
  }
  
  # Reset seed
  set.seed(NULL)
  
  # Check whether plot should be produced
  if(isTRUE(produce)){
    plot(semnet_plot)
  }
  
  # Return plot
  return(semnet_plot)
  
}
