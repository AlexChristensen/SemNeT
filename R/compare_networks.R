#' @title Plot Networks for Comparison
#'
#' @description Uses \code{\link[qgraph]{qgraph}} to plot networks side by
#' side. Accepts any number of networks and arranges the plots using
#' \code{floor(sqrt(length(...)))} rows (e.g. 3 networks: 1 row of 3; 6 networks:
#' 2 rows of 3; 9 networks: 3 rows of 3)
#'
#' @param ... Named matrices or data frames.
#' At least two network adjacency matrices, e.g.
#' \code{compare_networks(low = low_net, high = high_net)}
#'
#' @param title List.
#' Titles for each plot, in the same order as \code{...}.
#' Defaults to the names given in \code{...}
#'
#' @param config Character.
#' Defaults to \code{"spring"}.
#' See \code{\link[qgraph]{qgraph}} for more options
#'
#' @param placement Character.
#' How should nodes be placed when comparing networks?
#' \itemize{
#' \item \code{"default"} --- places nodes in the default \code{config} positions
#' \item \code{"match"} --- places nodes in the same position across every
#' network (requires every network to have the same nodes)
#' }
#' Defaults to \code{"default"}
#'
#' @param weighted Boolean.
#' Should networks be plotted with weights?
#' Defaults to \code{FALSE}. Unweighted networks are often more aesthetically
#' representative of the network's structure
#'
#' @param qgraph_args List.
#' Additional arguments passed on to \code{\link[qgraph]{qgraph}}.
#' Defaults to \code{list()}
#'
#' @return Plots networks side by side using \code{\link[qgraph]{qgraph}};
#' returns \code{NULL} invisibly
#'
#' @examples
#' # Simulate datasets
#' one <- simulate_fluency(10)
#' two <- simulate_fluency(10)
#'
#' # Compute similarity matrices
#' cos1 <- similarity(one, method = "cosine")
#' cos2 <- similarity(two, method = "cosine")
#'
#' # Compute networks
#' net1 <- suppressWarnings(tmfg(cos1))
#' net2 <- suppressWarnings(tmfg(cos2))
#'
#' # Compare networks
#' compare_networks(one = net1, two = net2, config = "spring")
#'
#' # Change edge colors
#' compare_networks(
#'   one = net1, two = net2, config = "spring",
#'   qgraph_args = list(edge.color = "blue")
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
# Compare networks ----
# Updated 06.09.2026
compare_networks <- function(
  ..., title, config = "spring",
  placement = c("default", "match"),
  weighted = FALSE, qgraph_args = list()
)
{

  # Argument errors
  validated <- compare_networks_errors(list(...), title, config, placement, weighted, qgraph_args)

  # Get networks
  networks <- validated$networks

  # Get titles
  title <- validated$title

  # Get {qgraph} arguments
  qgraph_args <- validated$qgraph_args

  # Set placement
  placement <- match.arg(placement)

  # Get network names
  network_names <- names(networks)

  # Get number of networks
  n_networks <- length(networks)

  # Initialize layouts
  layouts <- vector("list", n_networks)

  # Initialize labels
  labels <- vector("list", n_networks)

  # Loop over networks
  for(i in seq_len(n_networks)){

    # Get network
    network <- networks[[i]]

    # Check for unweighted
    if(!weighted){

      # Binarize network
      network <- binarize(network)

    }

    # Remove self-loops
    diag(network) <- 0

    # Get layout without plotting
    layouts[[i]] <- qgraph::qgraph(network, DoNotPlot = TRUE, layout = config)

    # Get node labels
    labels[[i]] <- as.factor(colnames(network))

  }

  # Store graphical parameters and restore them on exit
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(suppressWarnings(graphics::par(old_par)), add = TRUE)

  # Check for two networks
  if(n_networks == 2){

    # Plot side by side
    layout(t(1:2))

  }else if(n_networks > 2){

    # Get number of rows
    grid_rows <- floor(sqrt(n_networks))

    # Get number of columns
    grid_columns <- ceiling(n_networks / grid_rows)

    # Plot in grid
    layout(matrix(seq_len(grid_rows * grid_columns), nrow = grid_rows, ncol = grid_columns, byrow = TRUE))

  }

  # Loop over networks
  for(i in seq_len(n_networks)){

    # Turn plotting back on
    layouts[[i]]$Arguments$DoNotPlot <- FALSE

  }

  # Check for matched placement
  if(placement == "match"){

    # Average layouts
    shared_layout <- qgraph::averageLayout(layouts)

  }else{

    # Use configuration
    shared_layout <- config

  }

  # Set layout
  qgraph_args$layout <- shared_layout

  # Get argument names
  qgraph_argument_names <- names(qgraph_args)

  # Check for node size
  if(!"vsize" %in% qgraph_argument_names){

    # Set node size
    qgraph_args$vsize <- 4

  }

  # Check for label proportion
  if(!"label.prop" %in% qgraph_argument_names){

    # Set label proportion
    qgraph_args$label.prop <- 1

  }

  # Check for aspect
  if(!"aspect" %in% qgraph_argument_names){

    # Set aspect
    qgraph_args$aspect <- FALSE

  }

  # Check for repulsion
  if(config == "spring" && !"repulsion" %in% qgraph_argument_names){

    # Set repulsion
    qgraph_args$repulsion <- 1.15

  }

  # Check for edge color
  if(!weighted && !"edge.color" %in% qgraph_argument_names){

    # Set edge color
    qgraph_args$edge.color <- "black"

  }

  # Loop over networks
  for(i in seq_len(n_networks)){

    # Set network
    qgraph_args$input <- layouts[[i]]

    # Set title
    qgraph_args$title <- title[[i]]

    # Set labels
    qgraph_args$labels <- labels[[i]]

    # Plot network
    do.call(qgraph::qgraph, args = qgraph_args)

  }

  # Return nothing
  return(invisible(NULL))

}

#' @noRd
# Errors ----
# Updated 13.10.2026
compare_networks_errors <- function(networks, title, config, placement, weighted, qgraph_args)
{

  # '...' errors
  if(length(networks) < 2){

    # Send error
    stop("In `compare_networks()`: at least two networks must be provided via `...`.", call. = FALSE)

  }

  # Check for unnamed networks
  if(is.null(names(networks)) || any(names(networks) == "")){

    # Send error
    stop(
      "In `compare_networks()`: every network passed via `...` must be named, e.g. `compare_networks(low = low_net, high = high_net)`.",
      call. = FALSE
    )

  }

  # Check for duplicate names
  if(anyDuplicated(names(networks)) > 0){

    # Send error
    stop("In `compare_networks()`: every network passed via `...` must have a unique name.", call. = FALSE)

  }

  # Check each network
  networks <- lapply(networks, function(network) adjacency_matrix_error(network, "compare_networks"))

  # 'title' errors
  if(missing(title)){

    # Use network names
    title <- as.list(names(networks))

  }else{

    # Check for list
    object_error(title, "list", "compare_networks")

    # Check for one title per network
    length_error(title, length(networks), "compare_networks")

  }

  # 'config' errors
  length_error(config, 1, "compare_networks")
  typeof_error(config, "character", "compare_networks")

  # 'placement' errors
  length_error(placement, c(1, 2), "compare_networks")
  typeof_error(placement, "character", "compare_networks")

  # Check for matched placement
  if(length(placement) == 1 && placement == "match"){

    # Get number of nodes of each network
    node_counts <- vapply(networks, ncol, numeric(1))

    # Check for different numbers of nodes
    if(length(unique(node_counts)) > 1){

      # Send error
      stop(
        "In `compare_networks()`: `placement = \"match\"` requires every network to have the same number of nodes.",
        call. = FALSE
      )

    }

    # Get node names of each network
    node_names <- lapply(networks, colnames)

    # Check for named networks with different nodes
    if(!any(vapply(node_names, is.null, logical(1))) && !all(vapply(node_names, setequal, logical(1), y = node_names[[1]]))){

      # Send error
      stop(
        "In `compare_networks()`: `placement = \"match\"` requires every network to have the same nodes (column names).",
        call. = FALSE
      )

    }

  }

  # 'weighted' errors
  length_error(weighted, 1, "compare_networks")
  typeof_error(weighted, "logical", "compare_networks")

  # 'qgraph_args' errors
  object_error(qgraph_args, "list", "compare_networks")

  # Return validated input (in case of data frames or default 'title')
  return(list(networks = networks, title = title, qgraph_args = qgraph_args))

}
