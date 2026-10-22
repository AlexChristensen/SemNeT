#' @title Pathfinder Network
#'
#' @description Estimates a semantic network from verbal fluency data by
#' applying the MST-Pathfinder Network algorithm (Quirin et al., 2008; see also
#' Schvaneveldt, 1990) to a distance between responses
#'
#' @param data Matrix or data frame.
#' A preprocessed verbal fluency matrix where rows are participants. Content
#' may be the raw word responses (columns are the order of the responses), an
#' ordered numeric response matrix (see \code{responses2binary()}), or, for
#' every \code{method} except \code{"paulsen"}, a binary (0/1) response matrix.
#' \code{"paulsen"} needs the response order, which a binary matrix has discarded
#'
#' @param method Character (length = 1).
#' Distance between responses that the Pathfinder algorithm prunes.
#' Defaults to \code{"chebyshev"}.
#' Available options:
#'
#' \itemize{
#'
#' \item \code{"chebyshev"} --- Chebyshev (maximum-coordinate) distance between
#' the responses' co-occurrence profiles (the default and the distance of
#' SemNeT versions before 2.1.0)
#'
#' \item \code{"euclidean"} --- Euclidean distance between the responses'
#' co-occurrence profiles
#'
#' \item \code{"manhattan"} --- Manhattan (city-block) distance between the
#' responses' co-occurrence profiles
#'
#' \item \code{"cosine"} --- Cosine distance (\code{1 - cosine similarity})
#' between the responses' co-occurrence profiles
#'
#' \item \code{"hamming"} --- Hamming distance (the number of other responses
#' whose co-occurrence counts differ) between the responses' co-occurrence
#' profiles
#'
#' \item \code{"jaccard"} --- Jaccard distance (\code{1 - Jaccard index}) of the
#' participants who gave each response
#'
#' \item \code{"paulsen"} --- Response-order distance of Paulsen et al. (1996)
#'
#' }
#'
#' @param q Numeric (length = 1).
#' Maximum number of links in the indirect paths that can remove a link (the
#' Pathfinder \eqn{q} parameter). A whole number of at least \code{2}.
#' Defaults to \code{NULL}, which uses \eqn{n - 1} (\eqn{n} is the number of
#' responses given by anyone; a path cannot be longer, so larger values, including
#' \code{Inf}, are treated as \eqn{n - 1}). Smaller values keep more links:
#' \code{q = 2} only checks paths through one other response
#'
#' @param r Numeric (length = 1).
#' Minkowski metric used to combine the distances along a path into the path's
#' length (the Pathfinder \eqn{r} parameter): the length of a path is
#' \eqn{(\sum_k d_k^r)^{1/r}}, where \eqn{d_k} are its link distances, and
#' \code{r = Inf} is the largest link distance. A link is removed when some
#' path of up to \code{q} links is shorter than it. A number of at least
#' \code{1}. Defaults to \code{Inf}. Larger values keep fewer links.
#' Schvaneveldt (1990) recommends \code{Inf} unless the distances are on a ratio
#' scale (distances are only ordinal when just their order is meaningful), and
#' \code{Inf} with \eqn{q = n - 1} is the setting the fast MST-Pathfinder
#' algorithm computes; any other setting is computed with the general
#' Pathfinder definition, which is slower for many responses.
#'
#' Suggested \code{r} for each \code{method} (these are guidance from the
#' properties of the distances, not established values; \code{Inf} is the
#' default and the conventional choice for all of them):
#'
#' \itemize{
#'
#' \item \code{"chebyshev"} --- \code{Inf}. A true metric, so \code{r = 1} keeps
#' every link; a finite \code{r} of 2 or more is defensible only if differences
#' in co-occurrence counts are treated as ratio-scaled
#'
#' \item \code{"euclidean"} --- \code{Inf}. \code{r = 2} is the finite value
#' that matches the metric (a path is measured as the distance is); \code{r = 1}
#' keeps every link
#'
#' \item \code{"manhattan"} --- \code{Inf}. \code{r = 1} (distances add along a
#' path, as the city-block distance adds differences) keeps every link, so use
#' \code{r = 2} or more if a finite value is wanted
#'
#' \item \code{"hamming"} --- \code{Inf}. Counts of differing co-occurrences are
#' a true metric: \code{r = 1} keeps every link, \code{r = 2} or more can prune
#'
#' \item \code{"cosine"} --- \code{Inf}. \code{1 - cosine} does not satisfy the
#' triangle inequality, so finite \code{r}, including \code{1}, can prune links,
#' but the result has no simple interpretation
#'
#' \item \code{"jaccard"} --- \code{Inf}. A true metric bounded by 1:
#' \code{r = 1} keeps every link between responses that are ever given together
#'
#' \item \code{"paulsen"} --- \code{Inf}. The distance is not guaranteed to
#' satisfy the triangle inequality, so finite \code{r}, including \code{1}, can
#' prune links; no finite value has been established for it
#'
#' }
#'
#' @details \strong{The distance is a modeling choice.} The MST-Pathfinder
#' algorithm (Quirin et al., 2008) is fully specified once a distance is given,
#' but it does not say which distance to use, and no distance is established as
#' the correct one for semantic networks. Different distances can give
#' noticeably different networks from the same data, so the choice should be
#' justified by the research question, should be reported, and, when
#' conclusions depend on it, should be checked against another distance.
#'
#' The \emph{co-occurrence profile} of a response is its row of the
#' co-occurrence matrix (the number of participants who gave both it and each
#' other response, with the diagonal set to zero). \code{"chebyshev"},
#' \code{"euclidean"}, \code{"manhattan"}, \code{"cosine"}, and
#' \code{"hamming"} (called "dominance", "Euclidean", "city block", "cosine",
#' and "Hamming" in the Pathfinder software of Schvaneveldt, pfnets.net) compare two
#' responses by how similar those profiles are, so two responses are close when
#' they co-occur with the same other responses (not necessarily with each
#' other). These distances use only whether a response was given, not where.
#' Responses given by a participant who gave nothing else have an empty profile
#' and, for \code{"cosine"}, an undefined distance, which is treated as
#' infinite (see below).
#'
#' \code{"jaccard"} compares the sets of participants directly: the distance is
#' \code{1 - a / (a + b + c)}, where \code{a} is the number of participants who
#' gave both responses and \code{b} and \code{c} are the numbers who gave only
#' one. It also ignores order.
#'
#' \code{"paulsen"} follows Paulsen et al. (1996):
#' \deqn{D_{ij} = \frac{N}{T_{ij}^2} \sum_{k=1}^{T_{ij}} \frac{d_{ijk}}{n_k}}
#' where \eqn{d_{ijk}} is the number of positions between the two responses in
#' participant \eqn{k}'s list, \eqn{n_k} is that participant's number of
#' responses (the largest position in their row), \eqn{T_{ij}} is the number of participants who gave both
#' responses, and \eqn{N} is the number of participants. Responses that are
#' close together in many participants' lists, relative to how many responses
#' they gave, are close. The positions are
#' those of \code{\link{responses2binary}}'s \code{$order} (a response a
#' participant repeats is positioned at its last occurrence).
#'
#' A pair that no participant gave together has an infinite distance for
#' \code{"paulsen"} and \code{"jaccard"} (and an undefined cosine distance is
#' set to infinite), so it can only be linked through other responses.
#' Responses nobody gave are not nodes.
#'
#' The network keeps a link between two responses only if no indirect path of
#' up to \code{q} links between them is shorter than the direct distance, where
#' path lengths are combined with the Minkowski metric \code{r} (a
#' PFnet(\eqn{r}, \eqn{q}); Schvaneveldt, 1990). With the defaults
#' (\eqn{r = \infty} and \eqn{q = n - 1}) this is computed with the
#' MST-Pathfinder algorithm (Quirin et al., 2008): the union of all minimum
#' spanning trees of the distance matrix. Pairs at exactly the same distance
#' are all kept when none is bridged by shorter links, and identical responses
#' (distance zero) are linked. When some responses have no finite distance to the others the
#' network is not connected (responses with no finite distance to any other are
#' isolated nodes).
#'
#' @return An adjacency matrix. The distances (\eqn{D_{ij}}) of the retained
#' links are attached as the \code{"weighted"} attribute (\code{0} where there
#' is no link, so a link at a distance of exactly zero also shows \code{0})
#'
#' @examples
#' # Obtain data
#' data <- open.clean[1:100, ]
#'
#' # Estimate network (default distance: Chebyshev)
#' pf_net <- pf(data)
#'
#' # Estimate network with the response-order distance of Paulsen et al.
#' pf_paulsen <- pf(data, method = "paulsen")
#'
#' # Estimate a denser network (paths of up to 2 links, Euclidean path lengths)
#' pf_dense <- pf(data, method = "euclidean", q = 2, r = 2)
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
# Updated 22.10.2026
pf <- function(
    data,
    method = c("chebyshev", "euclidean", "manhattan", "cosine", "hamming", "jaccard", "paulsen"),
    q = NULL, r = Inf
)
{

  # Argument errors
  validated <- pf_errors(data, method, q, r)

  # Get validated data
  data <- validated$data

  # Get validated method
  method <- validated$method

  # Get validated parameters
  q <- validated$q
  r <- validated$r

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
  D <- switch(
    method,
    paulsen = paulsen_distance(ordered),
    cooccurrence_distance(ordered > 0, method)
  )

  # Set largest number of links in a path (no path is longer than n - 1 links)
  q <- min(if(is.null(q)) Inf else q, ncol(D) - 1)

  # Get pathfinder links
  links <- pathfinder_links(D, q, r)

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
# Updated 22.10.2026
pf_errors <- function(data, method, q, r)
{

  # 'method' errors
  length_error(method, c(1, 7), "pf")
  typeof_error(method, "character", "pf")

  # Get method
  method <- match.arg(
    method, c("chebyshev", "euclidean", "manhattan", "cosine", "hamming", "jaccard", "paulsen")
  )

  # Check for object
  object_error(data, c("matrix", "data.frame"), "pf")

  # Check for binary response matrix
  binary <- is.numeric(as.matrix(data)) && all(as.matrix(data) %in% c(0, 1, NA))

  # Check for method that does not need response order
  if(method != "paulsen" && binary){

    # 'data' errors (binary responses are allowed)
    data <- response_matrix_error(data, "pf")

  }else{

    # 'data' errors (response order is needed)
    data <- ordered_response_matrix_error(data, "pf")

  }

  # 'q' errors
  if(!is.null(q)){

    length_error(q, 1, "pf")
    typeof_error(q, "numeric", "pf")

    # Check for whole number of at least two
    if(is.na(q) || q < 2 || (is.finite(q) && q != round(q))){

      # Send error
      stop(
        paste0(
          "In `pf()`: input `q` is ", q, ".\n",
          "Expected: a whole number of at least 2 (or `NULL` or `Inf` for n - 1)."
        ),
        call. = FALSE
      )

    }

  }

  # 'r' errors
  length_error(r, 1, "pf")
  typeof_error(r, "numeric", "pf")

  # Check for value below one
  if(is.na(r) || r < 1){

    # Send error
    stop(
      paste0(
        "In `pf()`: input `r` is ", r, ".\n",
        "Expected: a number of at least 1 (`Inf` for the maximum)."
      ),
      call. = FALSE
    )

  }

  # Return validated arguments
  return(list(data = data, method = method, q = q, r = r))

}

#' @noRd
# Co-occurrence-based distance ----
# Updated 22.10.2026
cooccurrence_distance <- function(given, method)
{

  # Ensure numeric presence matrix
  given <- 1 * given

  # Count participants who gave both responses
  both <- crossprod(given)

  # Check for Jaccard distance
  if(method == "jaccard"){

    # Get number of participants who gave each response
    totals <- diag(both)

    # Get number of participants who gave either response
    either <- outer(totals, totals, "+") - both

    # Compute distances
    D <- 1 - both / either

    # Set pairs never given together to infinite
    D[both == 0] <- Inf

  }else{

    # Remove self co-occurrences
    diag(both) <- 0

    # Compute distances between co-occurrence profiles
    D <- switch(
      method,
      chebyshev = as.matrix(dist(both, method = "maximum")),
      euclidean = as.matrix(dist(both, method = "euclidean")),
      manhattan = as.matrix(dist(both, method = "manhattan")),
      cosine = pmax(1 - cosine(both), 0),
      hamming = apply(both, 1, function(profile) rowSums(sweep(both, 2, profile, "!=")))
    )

    # Set undefined distances to infinite
    D[is.na(D)] <- Inf

  }

  # Set self-distances to zero
  diag(D) <- 0

  # Remove names
  dimnames(D) <- NULL

  # Return distances
  return(D)

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
# Pathfinder links ----
# Updated 22.10.2026
pathfinder_links <- function(D, q = Inf, r = Inf)
{

  # Check for the setting the MST-Pathfinder algorithm computes (r = Inf and q = n - 1)
  if(is.infinite(r) && q >= ncol(D) - 1){

    # Return MST-Pathfinder links
    return(mst_pathfinder_links(D))

  }

  # Return links of general Pathfinder network
  return(general_pathfinder_links(D, q, r))

}

#' @noRd
# General Pathfinder links (Schvaneveldt, 1990) ----
# Updated 22.10.2026
general_pathfinder_links <- function(D, q, r)
{

  # Get number of nodes
  n_nodes <- ncol(D)

  # Check for maximum path weight
  maximum <- is.infinite(r)

  # Set weights
  weight <- D

  # Check for Minkowski path weight
  if(!maximum){

    # Get largest finite weight
    largest <- max(weight[is.finite(weight)], 0)

    # Scale weights (path weights scale with them) to avoid overflow
    if(largest > 0){

      # Scale by largest weight
      weight <- weight / largest

    }

    # Raise weights to r so that weights of a path add
    weight <- weight^r

  }

  # Set self-weights to zero (a path of fewer links is also a path of q links)
  diag(weight) <- 0

  # Combine paths: weight of best path from i to j through any node
  combine <- function(first, second)
  {

    # Initialize best weights
    best <- rep(Inf, n_nodes^2)

    # Loop over intermediate nodes
    for(node in seq_len(n_nodes)){

      # Get weights of paths through node
      through <- if(maximum){

        pmax(first[, node], rep(second[node, ], each = n_nodes))

      }else{

        first[, node] + rep(second[node, ], each = n_nodes)

      }

      # Keep best weights
      best <- pmin(best, through)

    }

    # Return best weights
    return(matrix(best, nrow = n_nodes, ncol = n_nodes))

  }

  # Get best weights of paths of up to q links (by repeated squaring)
  best <- NULL
  base <- weight
  remaining <- q

  # Loop until all links are used
  while(remaining > 0){

    # Check for link count to include
    if(remaining %% 2 == 1){

      # Add paths
      best <- if(is.null(best)) base else combine(best, base)

    }

    # Halve remaining links
    remaining <- remaining %/% 2

    # Double paths
    if(remaining > 0){

      base <- combine(base, base)

    }

  }

  # Check for no links to combine (single node)
  if(is.null(best)){

    # Use direct weights
    best <- weight

  }

  # Keep finite links that no path of up to q links beats
  links <- is.finite(weight) & weight <= best + 1e-12 * pmax(1, abs(weight))

  # Convert to numeric matrix
  links <- 1 * links

  # Remove self-links
  diag(links) <- 0

  # Return links
  return(links)

}

#' @noRd
# MST-Pathfinder links (Quirin et al., 2008) ----
# Updated 13.10.2026
mst_pathfinder_links <- function(D)
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
