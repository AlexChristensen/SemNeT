#' @title Triangulated Maximally Filtered Graph
#'
#' @description Applies the Triangulated Maximally Filtered Graph (TMFG)
#' filtering method (\strong{please see and cite Massara et al., 2016}). The
#' TMFG method uses a structural constraint that limits the number of
#' zero-order correlations included in the network (3\emph{n} - 6, where
#' \emph{n} is the number of variables). Construction begins by forming a
#' tetrahedron from the four nodes with the largest sum of correlations, then
#' iteratively adds each remaining node to the connected set of three nodes
#' (a triangle) it correlates with most strongly, until every node is included.
#'
#' @param data Matrix or data frame.
#' A square association matrix (e.g. a correlation or similarity matrix)
#'
#' @param depend Boolean.
#' Is the network a dependency (directed) network?
#' Defaults to \code{FALSE}. Set to \code{TRUE} to generate a TMFG-filtered
#' dependency network, where the direction of each edge follows whichever of
#' the two asymmetric values is larger
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
#' @importFrom utils combn
#'
#' @export
# Triangulated Maximally Filtered Graph
# Updated 06.09.2026
tmfg <- function(data, depend = FALSE)
{

  # Validate and coerce input before any computation begins
  data <- tmfg_errors(data, depend)

  # Number of nodes
  n <- ncol(data)

  # Signed values are kept for the final output; the filtering itself works
  # on absolute strength (a strong negative correlation is still a strong edge)
  signed_matrix <- data
  cormat <- abs(data)

  # TMFG is only well-defined below the literature's recommended minimum, but
  # still runs correctly -- warn instead of the original's unconditional
  # `print()`, which never actually surfaced through R's condition system
  if(n < 9){
    warning("`tmfg()`: fewer than 9 nodes is smaller than recommended for stable TMFG estimation.", call. = FALSE)
  }

  # Find the 4 nodes with the largest strength (row sum of above-average
  # connections) -- these seed the initial tetrahedron
  average_connection <- mean(cormat)
  strength <- rowSums(cormat * (cormat > average_connection))
  inserted <- order(strength, decreasing = TRUE)[1:4]

  # Every node not yet in the network
  remaining <- setdiff(1:n, inserted)

  # The 4 faces of the initial tetrahedron, as triangles of already-inserted nodes
  triangles <- matrix(nrow = (2 * n) - 4, ncol = 3)
  triangles[1, ] <- inserted[1:3]
  triangles[2, ] <- inserted[2:4]
  triangles[3, ] <- inserted[c(1, 2, 4)]
  triangles[4, ] <- inserted[c(1, 3, 4)]

  # Sparse edge list: 3n - 6 edges total, 6 of which are the tetrahedron's
  edges <- matrix(nrow = (3 * n) - 6, ncol = 3)

  # The 6 tetrahedron edges, one per pair of the first 4 inserted nodes --
  # replaces 6 duplicated if/else blocks with one loop over the pairs
  initial_pairs <- combn(4, 2)

  for(pair in seq_len(ncol(initial_pairs))){

    from <- inserted[initial_pairs[1, pair]]
    to <- inserted[initial_pairs[2, pair]]

    # Undirected: always (from, to). Directed: whichever asymmetric value is larger
    if(!depend || cormat[from, to] > cormat[to, from]){
      edges[pair, ] <- c(from, to, 1)
    }else{
      edges[pair, ] <- c(to, from, 1)
    }

  }

  # Gain table: for each not-yet-inserted node, its summed connection strength
  # to each candidate triangle -- `-Inf` marks "not a currently open triangle"
  gain <- matrix(-Inf, nrow = n, ncol = 2 * (n - 2))
  gain[remaining, 1] <- rowSums(cormat[remaining, triangles[1, ], drop = FALSE])
  gain[remaining, 2] <- rowSums(cormat[remaining, triangles[2, ], drop = FALSE])
  gain[remaining, 3] <- rowSums(cormat[remaining, triangles[3, ], drop = FALSE])
  gain[remaining, 4] <- rowSums(cormat[remaining, triangles[4, ], drop = FALSE])

  # Number of open triangles so far
  n_triangles <- 4

  # Greedily insert every remaining node into its best-fitting open triangle.
  # This step is inherently sequential (each insertion changes which
  # triangles are open for the next one), so it cannot be vectorized away --
  # only 4 nodes are already placed, so this only runs when there's at least
  # one more node to add
  if(n > 4){

    for(next_node in 5:n){

      if(length(remaining) == 1){

        # Only one candidate node left: it goes wherever its gain is highest
        chosen_node <- remaining
        chosen_column <- 1
        best_triangle <- which.max(gain[remaining, ])

      }else{

        # Best candidate node (and its gain) for every open triangle column,
        # vectorized via `apply()` in place of the original's per-column loop
        column_best_gain <- apply(gain[remaining, , drop = FALSE], 2, max)
        column_best_row <- apply(gain[remaining, , drop = FALSE], 2, which.max)

        best_triangle <- which.max(column_best_gain)
        chosen_column <- column_best_row[best_triangle]
        chosen_node <- remaining[chosen_column]

      }

      # Move the chosen node from "remaining" to "inserted"
      remaining <- remaining[-chosen_column]
      inserted[next_node] <- chosen_node

      # Connect the chosen node to all 3 nodes of its chosen triangle
      for(k in seq_along(triangles[best_triangle, ])){

        edge_row <- 6 + (3 * (next_node - 5) + k)
        neighbor <- triangles[best_triangle, k]

        if(!depend || cormat[chosen_node, neighbor] > cormat[neighbor, chosen_node]){
          edges[edge_row, ] <- c(chosen_node, neighbor, 1)
        }else{
          edges[edge_row, ] <- c(neighbor, chosen_node, 1)
        }

      }

      # The one triangle used is split into 2 new triangles with the new node;
      # a third "triangle" (the original, now interior) is retired
      triangles[n_triangles + 1, ] <- c(triangles[best_triangle, c(1, 3)], chosen_node)
      triangles[n_triangles + 2, ] <- c(triangles[best_triangle, c(2, 3)], chosen_node)
      triangles[best_triangle, ] <- c(triangles[best_triangle, c(1, 2)], chosen_node)

      # The inserted node is no longer a valid candidate for any triangle
      gain[chosen_node, ] <- 0

      # Refresh the gain table only for the 3 triangles that changed
      gain[remaining, best_triangle] <- rowSums(cormat[remaining, triangles[best_triangle, ], drop = FALSE])
      gain[remaining, n_triangles + 1] <- rowSums(cormat[remaining, triangles[n_triangles + 1, ], drop = FALSE])
      gain[remaining, n_triangles + 2] <- rowSums(cormat[remaining, triangles[n_triangles + 2, ], drop = FALSE])

      n_triangles <- n_triangles + 2

    }

  }

  # Sparse-to-dense conversion, vectorized via matrix indexing in place of
  # the original's row-by-row for-loop. Every edge is filled in both
  # directions regardless of 'depend' -- a dependency network's *topology*
  # is still undirected (both cells become nonzero), only the *edge weight*
  # copied back in below differs by direction, since the signed source
  # matrix need not itself be symmetric
  adjacency <- matrix(0, nrow = n, ncol = n)
  adjacency[cbind(edges[, 1], edges[, 2])] <- 1
  adjacency[cbind(edges[, 2], edges[, 1])] <- 1
  diag(adjacency) <- 1

  # Restore the original signed association values on every retained edge,
  # vectorized via logical indexing in place of the original's double for-loop
  adjacency[adjacency == 1] <- signed_matrix[adjacency == 1]

  colnames(adjacency) <- colnames(data)
  row.names(adjacency) <- colnames(data)

  return(adjacency)

}

#' @noRd
# Argument errors for `tmfg()`
# Updated 06.09.2026
tmfg_errors <- function(data, depend)
{

  # 'data' must be a square, numeric matrix (or coercible data.frame)
  data <- adjacency_matrix_error(data, "tmfg")

  # TMFG's initial tetrahedron requires at least 4 nodes to exist at all --
  # below that, `order(strength, decreasing = TRUE)[1:4]` would silently
  # introduce `NA` indices and corrupt every step downstream
  if(ncol(data) < 4){
    stop(
      paste0(
        "In `tmfg()`: input `", deparse(substitute(data)), "` has ", ncol(data), " nodes.\n",
        "TMFG requires at least 4 nodes to form its initial tetrahedron."
      ),
      call. = FALSE
    )
  }

  # 'depend' must be a single logical value
  object_error(depend, "vector", "tmfg")
  typeof_error(depend, "logical", "tmfg")
  length_error(depend, 1, "tmfg")

  return(data)

}
