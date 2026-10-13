#' @title Test Against Random Networks
#'
#' @description Tests global measures of semantic networks against the
#' distribution of the same measures in random networks of equivalent size and
#' density (Erdos-Renyi) and in random networks that also keep every node's
#' degree
#'
#' @param ... Named matrices or data frames.
#' Semantic networks to be compared against random networks, e.g.
#' \code{random_network_test(low = low_net, high = high_net)}
#'
#' @param iter Numeric.
#' Number of random networks to generate per input network.
#' Defaults to \code{1000}
#'
#' @param cores Numeric.
#' Number of computer processing cores to use.
#' Defaults to \code{1}
#'
#' @param seed Numeric.
#' A seed for reproducible results (including the community detection behind
#' \code{q}), the same whatever \code{cores} is. Each random network gets its
#' own seed derived from it.
#' Defaults to \code{NULL} (not reproducible; \code{0} is random too)
#'
#' @details Every network is binarized (any nonzero edge becomes 1) before its
#' measures are computed, and its edge count is the number of nonzero
#' off-diagonal entries (a weighted network's weights are not summed).
#'
#' Two null models are used, because they answer different questions. An
#' Erdos-Renyi network has the observed node and edge counts but otherwise random
#' edges, so any structure (including the degree distribution that nearly every
#' semantic network has) distinguishes the observed network from it. A
#' degree-preserving network is the observed network after many random edge
#' swaps (\code{\link[igraph]{rewire}}), so it keeps every node's degree and
#' asks whether the measure differs beyond what the degrees imply.
#'
#' \emph{p}-values are Monte Carlo \emph{p}-values, with no normality
#' assumption: \eqn{(1 + \#\{|x_i - \bar{x}| \ge |x_{obs} - \bar{x}|\}) / (iter + 1)},
#' where \eqn{x_i} are the \code{iter} random-network values and \eqn{\bar{x}}
#' their mean. They cannot be smaller than \code{1 / (iter + 1)} (and are
#' reported to 4 significant digits, so that they are not rounded to 0). Neither null
#' model guarantees a connected network, and \code{aspl} ignores unreachable
#' pairs, so a sparse random network can differ from a connected observed one
#' for that reason alone. The tests are not adjusted for the number of measures.
#'
#' @return Returns a named list (one entry per input network) of data frames
#' with, for each of \code{aspl}, \code{cc}, and \code{q}, the Monte Carlo
#' \emph{p}-value and the mean and standard deviation of the random-network
#' distribution: \code{p_value}, \code{random_mean}, and \code{random_sd} for
#' Erdos-Renyi networks, and \code{p_value_degree}, \code{random_mean_degree},
#' and \code{random_sd_degree} for degree-preserving networks
#'
#' @examples
#' # Get openness data
#' one <- open.clean[which(open.group == "Low"), ]
#' two <- open.clean[which(open.group == "High"), ]
#'
#' \donttest{
#' # Compute networks
#' net_one <- cn(one)
#' net_two <- cn(two)
#'
#' # Perform random networks test
#' random_network_test(low = net_one, high = net_two, iter = 100, cores = 2)
#' }
#'
#' @references
#' Erdos, P., & Renyi, A. (1960).
#' On the evolution of random graphs.
#' \emph{Publications of the Mathematical Institute of the Hungarian Academy of Sciences}, \emph{5}, 17-61.
#'
#' Maslov, S., & Sneppen, K. (2002).
#' Specificity and stability in topology of protein networks.
#' \emph{Science}, \emph{296}, 910-913.
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Random network test ----
# Updated 13.10.2026
random_network_test <- function(..., iter = 1000, cores = 1, seed = NULL)
{

  # Argument errors
  networks <- random_network_test_errors(list(...), iter, cores, seed)

  # Send message
  message("Generating random networks and measures...")

  # Loop over networks
  results <- lapply(networks, function(observed){

    # Binarize network
    observed <- binarize(observed)

    # Remove self-loops
    diag(observed) <- 0

    # Get number of nodes
    nodes <- ncol(observed)

    # Get number of edges
    edges <- sum(observed) / 2

    # Get seeds of iterations
    seeds <- if(random_seed(seed)) NULL else reproducible_seeds(iter, seed)

    # Generate random networks and compute measures
    batch <- parallel_process(
      iter,
      function(i){

        # Get seed of iteration
        iteration_seed <- if(is.null(seeds)) NULL else seeds[i]

        # Compute measures of both random networks
        with_local_seed(iteration_seed, c(
          semantic_network_measures(generate_random_network(nodes = nodes, edges = edges), seed = iteration_seed),
          semantic_network_measures(generate_degree_network(observed), seed = iteration_seed)
        ))

      },
      cores = cores,
      progress = TRUE
    )

    # Simplify to matrix
    batch <- simplify2array(batch)

    # Return observed measures versus random measures
    random_network_significance(
      semantic_network_measures(observed, seed = seed), batch[1:3, , drop = FALSE], batch[4:6, , drop = FALSE]
    )

  })

  # Return results
  return(results)

}

# Random network significance ----
# Updated 13.10.2026
random_network_significance <- function(observed_measures, er_measures, degree_measures)
{

  # Summarize null distribution
  summarize_null <- function(batch_measures){

    # Get mean of null
    random_mean <- rowMeans(batch_measures)

    # Get standard deviation of null
    random_sd <- apply(batch_measures, 1, sd)

    # Compute p-values
    p_value <- vapply(seq_along(observed_measures), function(i){

      # Identify draws at least as far from mean as observed
      extreme <- abs(batch_measures[i, ] - random_mean[i]) >= abs(observed_measures[i] - random_mean[i]) - 1e-12

      # Return p-value (observed counted as a draw)
      (1 + sum(extreme, na.rm = TRUE)) / (sum(!is.na(batch_measures[i, ])) + 1)

    }, numeric(1))

    # Return summary
    list(p_value = p_value, random_mean = random_mean, random_sd = random_sd)

  }

  # Summarize random networks
  er <- summarize_null(er_measures)

  # Summarize degree-preserving networks
  degree <- summarize_null(degree_measures)

  # Return results
  data.frame(
    p_value = signif(er$p_value, 4), random_mean = round(er$random_mean, 4), random_sd = round(er$random_sd, 4),
    p_value_degree = signif(degree$p_value, 4), random_mean_degree = round(degree$random_mean, 4),
    random_sd_degree = round(degree$random_sd, 4),
    row.names = names(observed_measures)
  )

}

# Generate degree-preserving network ----
# Updated 13.10.2026
generate_degree_network <- function(observed)
{

  # Convert to {igraph}
  graph <- igraph::graph_from_adjacency_matrix(observed, mode = "undirected", diag = FALSE)

  # Rewire edges keeping degrees
  rewired <- igraph::rewire(graph, igraph::keeping_degseq(loops = FALSE, niter = 10 * max(igraph::ecount(graph), 1)))

  # Return network
  return(as.matrix(igraph::as_adjacency_matrix(rewired, sparse = FALSE)))

}

# Generate random network ----
# Updated 06.09.2026
generate_random_network <- function(nodes, edges)
{

  # Initialize complete network
  full <- matrix(1, nrow = nodes, ncol = nodes)

  # Remove self-loops
  diag(full) <- 0

  # Get possible edges
  possible_edges <- which(upper.tri(full) == TRUE)

  # Choose edges at random
  chosen_edges <- possible_edges[sample(length(possible_edges))][seq_len(edges)]

  # Initialize random network
  random <- matrix(0, nrow = nodes, ncol = nodes)

  # Add chosen edges
  random[chosen_edges] <- 1

  # Make network symmetric
  random <- random + t(random)

  # Return network
  return(random)

}

#' @noRd
# Errors ----
# Updated 13.10.2026
random_network_test_errors <- function(networks, iter, cores, seed)
{

  # '...' errors
  if(length(networks) < 1){

    # Send error
    stop("In `random_network_test()`: at least one network must be provided via `...`.", call. = FALSE)

  }

  # Check for unnamed networks
  if(is.null(names(networks)) || any(names(networks) == "")){

    # Send error
    stop(
      "In `random_network_test()`: every network passed via `...` must be named, e.g. `random_network_test(low = low_net, high = high_net)`.",
      call. = FALSE
    )

  }

  # Check for duplicate names
  if(anyDuplicated(names(networks)) > 0){

    # Send error
    stop("In `random_network_test()`: every network passed via `...` must have a unique name.", call. = FALSE)

  }

  # Check each network
  networks <- lapply(networks, function(net) adjacency_matrix_error(net, "random_network_test", require_symmetric = TRUE))

  # 'iter' errors
  length_error(iter, 1, "random_network_test")
  typeof_error(iter, "numeric", "random_network_test")
  range_error(iter, c(1, Inf), "random_network_test")

  # 'cores' errors
  length_error(cores, 1, "random_network_test")
  typeof_error(cores, "numeric", "random_network_test")
  range_error(cores, c(1, Inf), "random_network_test")

  # 'seed' errors
  if(!is.null(seed)){
    length_error(seed, 1, "random_network_test")
    typeof_error(seed, "numeric", "random_network_test")
    range_error(seed, c(0, Inf), "random_network_test")
  }

  # Return 'networks' (in case of data frames)
  return(networks)

}
