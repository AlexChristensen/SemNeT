#' @title Pathfinder Network
#'
#' @description Estimates a semantic network from verbal fluency data by
#' applying the MST-Pathfinder Network algorithm (Quirin et al., 2008; see also
#' Schvaneveldt, 1990) to the response-order distance of Paulsen et al. (1996)
#'
#' @param data Matrix or data frame.
#' A preprocessed verbal fluency matrix where rows are participants and
#' columns are the order of their verbal fluency responses. Content may be
#' the raw word responses or an ordered numeric response matrix (see
#' \code{responses2binary()}). A strictly binary (0/1) matrix is not
#' accepted because it has discarded the response order the distance needs
#'
#' @details The distance between two responses \eqn{i} and \eqn{j} follows
#' Paulsen et al. (1996):
#' \deqn{D_{ij} = \frac{N}{T_{ij}^2} \sum_{k=1}^{T_{ij}} \frac{d_{ijk}}{n_k}}
#' where \eqn{d_{ijk}} is the number of positions between the two responses in
#' participant \eqn{k}'s list, \eqn{n_k} is that participant's number of
#' responses (the largest position in their row), \eqn{T_{ij}} is the number of participants who gave both
#' responses, and \eqn{N} is the number of participants. Responses that are
#' close together in many participants' lists, relative to how many responses
#' they gave, are close; a pair no participant gave together has an infinite
#' distance and can only be linked through other responses. The positions are
#' those of \code{\link{responses2binary}}'s \code{$order} (a response a
#' participant repeats is positioned at its last occurrence), and responses
#' nobody gave are not nodes.
#'
#' The network keeps a link between two responses only if no indirect path
#' between them has every one of its links shorter than the direct distance
#' (Pathfinder parameters \eqn{r = \infty} and \eqn{q = n - 1}), computed with
#' the MST-Pathfinder algorithm: the union of all minimum spanning trees of the
#' distance matrix. Pairs at exactly the same distance are all kept when none
#' is bridged by shorter links. When some responses are never given together
#' with the others the network is not connected (responses with no finite
#' distance to any other are isolated nodes).
#'
#' @return An adjacency matrix. The distances (\eqn{D_{ij}}) of the retained
#' links are attached as the \code{"weighted"} attribute (\code{0} where there
#' is no link)
#'
#' @examples
#' # Obtain data
#' data <- open.clean[1:100, ]
#'
#' # Estimate network
#' pf_net <- pf(data)
#'
#' @references
#' Paulsen, J. S., Romero, R., Chan, A., Davis, A. V., Heaton, R. K., & Jeste, D. V. (1996).
#' Impairment of the semantic network in schizophrenia.
#' \emph{Psychiatry Research}, \emph{63}, 109-121.
#'
#' Quirin, A., Cordon, O., Guerrero-Bote, V. P., Vargas-Quesada, B., & Moya-Aneon, F. (2008)
#' A quick MST-based algorithm to obtain Pathfinder networks (Inf, n-1).
#' \emph{Journal of the American Society for Information Science and Technology}, \emph{59}, 1912-1924.
#'
#' Schvaneveldt, R. W. (1990).
#' \emph{Pathfinder associative networks: Studies in knowledge organization}.
#' Norwood, NJ: Ablex Publishing.
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Pathfinder network ----
# Updated 13.10.2026
pf <- function(data)
{

  # Argument errors
  data <- pf_errors(data)

  # Check for ordered responses
  if(is.numeric(data)){

    # Ensure matrix
    ordered <- as.matrix(data)

    # Set missing responses to not given
    ordered[is.na(ordered)] <- 0

  }else{

    # Get response order
    ordered <- responses2binary(data)$order

  }

  # Keep responses given by someone
  ordered <- ordered[, colSums(ordered > 0) > 0, drop = FALSE]

  # Get vocabulary
  vocabulary <- colnames(ordered)

  # Check for no names
  if(is.null(vocabulary)){

    # Set generic names
    vocabulary <- paste0("V", seq_len(ncol(ordered)))

  }

  # Get distances
  D <- paulsen_distance(ordered)

  # Get pathfinder links
  links <- pathfinder_links(D)

  # Set binary network
  binary_network <- links

  # Set row names
  row.names(binary_network) <- vocabulary

  # Set column names
  colnames(binary_network) <- vocabulary

  # Keep distances of links
  weighted_network <- ifelse(links == 1, D, 0)

  # Set row names
  row.names(weighted_network) <- vocabulary

  # Set column names
  colnames(weighted_network) <- vocabulary

  # Attach weighted network
  attr(binary_network, "weighted") <- weighted_network

  # Return network
  return(binary_network)

}

#' @noRd
# Errors ----
# Updated 13.10.2026
pf_errors <- function(data)
{

  # 'data' errors
  data <- ordered_response_matrix_error(data, "pf")

  # Return 'data' (in case of data frame)
  return(data)

}

#' @noRd
# Paulsen distance (Paulsen et al., 1996) ----
# Updated 13.10.2026
paulsen_distance <- function(ordered)
{

  # Get number of words
  n_words <- ncol(ordered)

  # Initialize scaled distances
  scaled_distance <- matrix(0, nrow = n_words, ncol = n_words)

  # Initialize co-occurrence counts
  both_given <- matrix(0, nrow = n_words, ncol = n_words)

  # Loop over participants
  for(participant in seq_len(nrow(ordered))){

    # Get row of participant
    row <- ordered[participant, ]

    # Get responses given
    given <- which(row > 0)

    # Check for fewer than two responses
    if(length(given) < 2){

      # Skip participant
      next

    }

    # Get positions of responses
    positions <- row[given]

    # Get number of responses
    n_responses <- max(positions)

    # Get distances between positions
    gaps <- abs(outer(positions, positions, "-"))

    # Add scaled distances
    scaled_distance[given, given] <- scaled_distance[given, given] + gaps / n_responses

    # Add co-occurrences
    both_given[given, given] <- both_given[given, given] + 1

  }

  # Compute distances
  D <- nrow(ordered) / both_given^2 * scaled_distance

  # Set pairs never given together to infinite
  D[both_given == 0] <- Inf

  # Set self-distances to zero
  diag(D) <- 0

  # Return distances
  return(D)

}

#' @noRd
# Pathfinder links (Quirin et al., 2008) ----
# Updated 13.10.2026
pathfinder_links <- function(D)
{

  # Get number of nodes
  n_nodes <- ncol(D)

  # Initialize links
  links <- matrix(0, nrow = n_nodes, ncol = n_nodes)

  # Identify lower triangle
  lower <- lower.tri(D)

  # Get edge list
  edges <- cbind(row(D)[lower], col(D)[lower], D[lower])

  # Keep finite distances
  edges <- edges[is.finite(edges[, 3]), , drop = FALSE]

  # Sort edges by distance
  edges <- edges[order(edges[, 3], edges[, 2], edges[, 1]), , drop = FALSE]

  # Set cluster of each node
  cluster <- seq_len(n_nodes)

  # Set number of clusters
  n_clusters <- n_nodes

  # Start at cheapest edge
  position <- 1

  # Get number of edges
  n_edges <- nrow(edges)

  # Loop until edges or clusters run out
  while(position <= n_edges && n_clusters > 1){

    # Get end of tier of tied distances
    tier_end <- findInterval(edges[position, 3] + 1e-12 * max(1, abs(edges[position, 3])), edges[, 3])

    # Get tier
    tier <- edges[position:tier_end, , drop = FALSE]

    # Move to next tier
    position <- tier_end + 1

    # Keep edges between different clusters
    tier <- tier[cluster[tier[, 1]] != cluster[tier[, 2]], , drop = FALSE]

    # Loop over edges in tier
    for(edge in seq_len(nrow(tier))){

      # Get first node
      from_node <- tier[edge, 1]

      # Get second node
      to_node <- tier[edge, 2]

      # Add link
      links[from_node, to_node] <- 1

      # Add reverse link
      links[to_node, from_node] <- 1

      # Check for different clusters
      if(cluster[from_node] != cluster[to_node]){

        # Merge clusters
        cluster[cluster == cluster[from_node]] <- cluster[to_node]

        # Update number of clusters
        n_clusters <- n_clusters - 1

      }

    }

  }

  # Return links
  return(links)

}
