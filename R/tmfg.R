#' @title Triangulated Maximally Filtered Graph
#'
#' @description Applies the Triangulated Maximally Filtered Graph (TMFG)
#' filtering method (\strong{please see and cite Massara et al., 2016}). The
#' TMFG method uses a structural constraint that limits the number of
#' zero-order correlations included in the network (3\emph{n} - 6, where
#' \emph{n} is the number of variables). Construction begins by forming a
#' tetrahedron from the four nodes with the largest strength (the sum of their
#' absolute associations that exceed the matrix's average absolute association,
#' diagonal included), then iteratively adds each remaining node to the
#' connected set of three nodes (a triangle) it is most strongly associated with,
#' until every node is included. Edges are chosen on absolute association
#' (a strong negative association is a strong edge); the signed values are kept
#' in the returned network.
#'
#' @param data Matrix or data frame.
#' A square association matrix (e.g. a correlation or similarity matrix)
#'
#' @param depend Boolean.
#' Is \code{data} an asymmetric dependency matrix? Without it, \code{data} must
#' be symmetric.
#' Defaults to \code{FALSE}. With \code{TRUE}, each edge is placed using the
#' larger of its two asymmetric values, but the returned network's links are
#' still symmetric (both cells of a retained edge are filled, each with its own
#' value from \code{data}), so it is not a directed network
#'
#' @return Returns a TMFG-filtered adjacency matrix
#'
#' @details The TMFG method applies a structural constraint on the network,
#' which restrains the network to retain a certain number of edges
#' (3\emph{n} - 6). The network is composed of 3- and 4-node cliques (a
#' triangle and a tetrahedron, respectively). Notably, TMFG can use any
#' association measure and does not assume the data is multivariate normal.
#'
#' @examples
#' # Pearson's correlation only for CRAN checks
#' fluency_data <- matrix(sample(0:1, 1000, replace = TRUE), nrow = 100, ncol = 10)
#' A <- suppressWarnings(tmfg(similarity(fluency_data, method = "cor")))
#'
#' @references
#' Christensen, A. P., Kenett, Y. N., Aste, T., Silvia, P. J., & Kwapil, T. R. (2018).
#' Network structure of the Wisconsin Schizotypy Scales-Short Forms: Examining psychometric network filtering approaches.
#' \emph{Behavior Research Methods}, \emph{50}, 2531-2550.
#'
#' Massara, G. P., Di Matteo, T., & Aste, T. (2016).
#' Network filtering for big data: Triangulated maximally filtered graph.
#' \emph{Journal of Complex Networks}, \emph{5}, 161-178.
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Triangulated Maximally Filtered Graph ----
# Updated 06.09.2026
tmfg <- function(data, depend = FALSE)
{

  # Argument errors
  data <- tmfg_errors(data, depend)

  # Get number of nodes
  n <- ncol(data)

  # Keep signed values
  signed_matrix <- data

  # Use absolute values for filtering
  cormat <- abs(data)

  # Check for too few nodes
  if(n < 9){

    # Send warning
    warning("`tmfg()`: fewer than 9 nodes is smaller than recommended for stable TMFG estimation.", call. = FALSE)

  }

  # Get average connection
  average_connection <- mean(cormat)

  # Get strength of each node
  strength <- rowSums(cormat * (cormat > average_connection))

  # Get four strongest nodes
  inserted <- order(strength, decreasing = TRUE)[1:4]

  # Get nodes not yet inserted
  remaining <- setdiff(1:n, inserted)

  # Initialize triangles
  triangles <- matrix(nrow = (2 * n) - 4, ncol = 3)

  # Set first face of tetrahedron
  triangles[1, ] <- inserted[1:3]

  # Set second face of tetrahedron
  triangles[2, ] <- inserted[2:4]

  # Set third face of tetrahedron
  triangles[3, ] <- inserted[c(1, 2, 4)]

  # Set fourth face of tetrahedron
  triangles[4, ] <- inserted[c(1, 3, 4)]

  # Initialize edges
  edges <- matrix(nrow = (3 * n) - 6, ncol = 3)

  # Get pairs of tetrahedron nodes
  initial_pairs <- combn(4, 2)

  # Loop over pairs
  for(pair in seq_len(ncol(initial_pairs))){

    # Get first node
    from <- inserted[initial_pairs[1, pair]]

    # Get second node
    to <- inserted[initial_pairs[2, pair]]

    # Check for direction
    if(!depend || cormat[from, to] > cormat[to, from]){

      # Add edge from first to second
      edges[pair, ] <- c(from, to, 1)

    }else{

      # Add edge from second to first
      edges[pair, ] <- c(to, from, 1)

    }

  }

  # Initialize gain table
  gain <- matrix(-Inf, nrow = n, ncol = 2 * (n - 2))

  # Set gains of first triangle
  gain[remaining, 1] <- rowSums(cormat[remaining, triangles[1, ], drop = FALSE])

  # Set gains of second triangle
  gain[remaining, 2] <- rowSums(cormat[remaining, triangles[2, ], drop = FALSE])

  # Set gains of third triangle
  gain[remaining, 3] <- rowSums(cormat[remaining, triangles[3, ], drop = FALSE])

  # Set gains of fourth triangle
  gain[remaining, 4] <- rowSums(cormat[remaining, triangles[4, ], drop = FALSE])

  # Set number of triangles
  n_triangles <- 4

  # Check for remaining nodes
  if(n > 4){

    # Loop over remaining nodes
    for(next_node in 5:n){

      # Check for last node
      if(length(remaining) == 1){

        # Set chosen node
        chosen_node <- remaining

        # Set chosen column
        chosen_column <- 1

        # Get best triangle
        best_triangle <- which.max(gain[remaining, ])

      }else{

        # Get best gain of each triangle
        column_best_gain <- apply(gain[remaining, , drop = FALSE], 2, max)

        # Get best node of each triangle
        column_best_row <- apply(gain[remaining, , drop = FALSE], 2, which.max)

        # Get best triangle
        best_triangle <- which.max(column_best_gain)

        # Get chosen column
        chosen_column <- column_best_row[best_triangle]

        # Get chosen node
        chosen_node <- remaining[chosen_column]

      }

      # Remove chosen node from remaining nodes
      remaining <- remaining[-chosen_column]

      # Add chosen node to inserted nodes
      inserted[next_node] <- chosen_node

      # Loop over nodes of triangle
      for(k in seq_along(triangles[best_triangle, ])){

        # Get row of edge
        edge_row <- 6 + (3 * (next_node - 5) + k)

        # Get neighbor
        neighbor <- triangles[best_triangle, k]

        # Check for direction
        if(!depend || cormat[chosen_node, neighbor] > cormat[neighbor, chosen_node]){

          # Add edge from chosen node
          edges[edge_row, ] <- c(chosen_node, neighbor, 1)

        }else{

          # Add edge to chosen node
          edges[edge_row, ] <- c(neighbor, chosen_node, 1)

        }

      }

      # Add first new triangle
      triangles[n_triangles + 1, ] <- c(triangles[best_triangle, c(1, 3)], chosen_node)

      # Add second new triangle
      triangles[n_triangles + 2, ] <- c(triangles[best_triangle, c(2, 3)], chosen_node)

      # Replace chosen triangle
      triangles[best_triangle, ] <- c(triangles[best_triangle, c(1, 2)], chosen_node)

      # Remove chosen node from gain table
      gain[chosen_node, ] <- 0

      # Update gains of replaced triangle
      gain[remaining, best_triangle] <- rowSums(cormat[remaining, triangles[best_triangle, ], drop = FALSE])

      # Update gains of first new triangle
      gain[remaining, n_triangles + 1] <- rowSums(cormat[remaining, triangles[n_triangles + 1, ], drop = FALSE])

      # Update gains of second new triangle
      gain[remaining, n_triangles + 2] <- rowSums(cormat[remaining, triangles[n_triangles + 2, ], drop = FALSE])

      # Update number of triangles
      n_triangles <- n_triangles + 2

    }

  }

  # Initialize network
  adjacency <- matrix(0, nrow = n, ncol = n)

  # Add edges
  adjacency[cbind(edges[, 1], edges[, 2])] <- 1

  # Add reverse edges
  adjacency[cbind(edges[, 2], edges[, 1])] <- 1

  # Set diagonal
  diag(adjacency) <- 1

  # Restore signed values
  adjacency[adjacency == 1] <- signed_matrix[adjacency == 1]

  # Set column names
  colnames(adjacency) <- colnames(data)

  # Set row names
  row.names(adjacency) <- colnames(data)

  # Return network
  return(adjacency)

}

#' @noRd
# Errors ----
# Updated 13.10.2026
tmfg_errors <- function(data, depend)
{

  # 'data' errors
  data <- adjacency_matrix_error(data, "tmfg")

  # Check for missing or infinite values
  if(!all(is.finite(data))){

    # Send error
    stop(
      "In `tmfg()`: the similarity matrix has missing or infinite values (e.g., from a response everyone, or no one, gave).\n",
      "Remove those responses first (see `finalize()`).",
      call. = FALSE
    )

  }

  # Check for fewer than four nodes
  if(ncol(data) < 4){

    # Send error
    stop(
      paste0(
        "In `tmfg()`: input `", deparse(substitute(data)), "` has ", ncol(data), " nodes.\n",
        "TMFG requires at least 4 nodes to form its initial tetrahedron."
      ),
      call. = FALSE
    )

  }

  # 'depend' errors
  length_error(depend, 1, "tmfg")
  typeof_error(depend, "logical", "tmfg")

  # Check for asymmetric network
  if(!depend && !isSymmetric(unname(data))){

    # Send error
    stop(
      "In `tmfg()`: the similarity matrix is not symmetric. Only `depend = TRUE` takes an asymmetric (dependency) matrix.",
      call. = FALSE
    )

  }

  # Return 'data' (in case of data frame)
  return(data)

}
