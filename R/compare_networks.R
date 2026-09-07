#' @title Plot Networks for Comparison
#'
#' @description Uses \code{\link[qgraph]{qgraph}} to plot networks side by
#' side. Accepts any number of networks and arranges the plots using
#' \code{floor(sqrt(length(...)))} side-by-side columns (e.g. 3 networks: 1
#' row of 3; 6 networks: 2 rows of 3; 9 networks: 3 rows of 3)
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
#' @importFrom graphics layout
#'
#' @export
# Compare networks side by side
# Updated 06.09.2026
compare_networks <- function(
  ..., title, config = "spring",
  placement = c("default", "match"),
  weighted = FALSE, qgraph_args = list()
)
{

  # Validate and coerce input before any computation begins
  validated <- compare_networks_errors(list(...), title, config, placement, weighted, qgraph_args)
  networks <- validated$networks
  title <- validated$title
  qgraph_args <- validated$qgraph_args

  # `match.arg()` picks the *first* listed choice when 'placement' is
  # missing, which is why "default" is listed first above
  placement <- match.arg(placement)

  network_names <- names(networks)
  n_networks <- length(networks)

  # Build each network's own {qgraph} layout without plotting it yet --
  # `averageLayout()` below (for `placement = "match"`) needs every layout
  # already computed before it can average them into one shared arrangement
  layouts <- vector("list", n_networks)
  labels <- vector("list", n_networks)

  for(i in seq_len(n_networks)){

    network <- networks[[i]]

    if(!weighted){
      network <- binarize(network)
    }

    diag(network) <- 0

    layouts[[i]] <- qgraph::qgraph(network, DoNotPlot = TRUE, layout = config)
    labels[[i]] <- as.factor(colnames(network))

  }

  # Arrange the plotting device into side-by-side panels
  if(n_networks == 2){

    layout(t(1:2))

  }else if(n_networks > 2){

    grid_columns <- floor(sqrt(n_networks))
    grid_rows <- ceiling(n_networks / grid_columns)
    layout(t(matrix(seq_len(grid_columns * grid_rows), nrow = grid_rows, ncol = grid_columns)))

  }

  # Every layout was built with `DoNotPlot = TRUE` above purely so
  # `averageLayout()` could use its coordinates without rendering anything --
  # flip that back now that the plotting device is actually ready
  for(i in seq_len(n_networks)){
    layouts[[i]]$Arguments$DoNotPlot <- FALSE
  }

  # A shared spatial layout (same node positions in every panel), or each
  # network's own independently-computed layout
  if(placement == "match"){
    shared_layout <- qgraph::averageLayout(layouts)
  }else{
    shared_layout <- config
  }
  qgraph_args$layout <- shared_layout

  # Apply this function's own sensible defaults, without overriding
  # anything the caller already set explicitly
  qgraph_argument_names <- names(qgraph_args)

  if(!"vsize" %in% qgraph_argument_names){
    qgraph_args$vsize <- 4
  }
  if(!"label.prop" %in% qgraph_argument_names){
    qgraph_args$label.prop <- 1
  }
  if(!"aspect" %in% qgraph_argument_names){
    qgraph_args$aspect <- FALSE
  }
  if(config == "spring" && !"repulsion" %in% qgraph_argument_names){
    qgraph_args$repulsion <- 1.15
  }
  if(!weighted && !"edge.color" %in% qgraph_argument_names){
    qgraph_args$edge.color <- "black"
  }

  # Plot every network in turn, into the panel layout set up above
  for(i in seq_len(n_networks)){

    qgraph_args$input <- layouts[[i]]
    qgraph_args$title <- title[[i]]
    qgraph_args$labels <- labels[[i]]

    do.call(qgraph::qgraph, args = qgraph_args)

  }

  return(invisible(NULL))

}

#' @noRd
# Argument errors for `compare_networks()`
# Updated 06.09.2026
compare_networks_errors <- function(networks, title, config, placement, weighted, qgraph_args)
{

  # At least two networks are required to compare anything
  if(length(networks) < 2){
    stop("In `compare_networks()`: at least two networks must be provided via `...`.", call. = FALSE)
  }

  # Every network must be named -- required in place of the original's
  # `substitute()`-based name-sniffing, and works correctly under `do.call()`
  if(is.null(names(networks)) || any(names(networks) == "")){
    stop(
      "In `compare_networks()`: every network passed via `...` must be named, e.g. `compare_networks(low = low_net, high = high_net)`.",
      call. = FALSE
    )
  }

  # Every network's name must be unique -- unlike the other `...`-based
  # functions in this package, nothing here is looked up by name internally
  # (every network is indexed positionally), so a duplicate name can't cause
  # silent data loss the way it can elsewhere; it would only produce two
  # identically-titled panels by default, which this still catches clearly
  if(anyDuplicated(names(networks)) > 0){
    stop("In `compare_networks()`: every network passed via `...` must have a unique name.", call. = FALSE)
  }

  # Every network must be a square, numeric adjacency matrix
  networks <- lapply(networks, function(network) adjacency_matrix_error(network, "compare_networks"))

  # 'title' defaults to the names given in `...`; otherwise must be a list
  # with exactly one title per network
  if(missing(title)){
    title <- as.list(names(networks))
  }else{

    object_error(title, "list", "compare_networks")
    length_error(title, length(networks), "compare_networks")

  }

  # 'config' must be a single character string
  object_error(config, "vector", "compare_networks")
  typeof_error(config, "character", "compare_networks")
  length_error(config, 1, "compare_networks")

  # 'placement' must be one (and only one) of the documented options
  length_error(placement, c(1, 2), "compare_networks")

  # `placement = "match"` averages every network's layout together, which
  # only makes sense when every network actually has the same nodes
  if(length(placement) == 1 && placement == "match"){

    node_counts <- vapply(networks, ncol, numeric(1))

    if(length(unique(node_counts)) > 1){
      stop(
        "In `compare_networks()`: `placement = \"match\"` requires every network to have the same number of nodes.",
        call. = FALSE
      )
    }

  }

  # 'weighted' must be a single logical value
  object_error(weighted, "vector", "compare_networks")
  typeof_error(weighted, "logical", "compare_networks")
  length_error(weighted, 1, "compare_networks")

  # 'qgraph_args' must be a list
  object_error(qgraph_args, "list", "compare_networks")

  return(list(networks = networks, title = title, qgraph_args = qgraph_args))

}
