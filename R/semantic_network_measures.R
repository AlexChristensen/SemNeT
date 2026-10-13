#' @title Semantic Network Measures
#'
#' @description Computes the average shortest path length (\code{\link{aspl}}),
#' clustering coefficient (\code{\link{cc}}), and modularity (\code{\link{q}})
#' of a network
#'
#' @param A Matrix or data frame.
#' An adjacency matrix of a network
#'
#' @param measures Character.
#' Which global network measures to compute: any of \code{"aspl"},
#' \code{"cc"}, and \code{"q"}. Defaults to all three
#'
#' @param weighted Boolean.
#' Should weighted measures be computed?
#' Defaults to \code{FALSE}. Set to \code{TRUE} for weighted measures
#'
#' @param seed Numeric.
#' Passed on to \code{\link{q}} for reproducible community detection.
#' Defaults to \code{NULL} (not reproducible; \code{0} is random too) -- see
#' \code{\link{q}}'s Details
#'
#' @param na.rm Boolean.
#' Passed on to \code{\link{cc}}: should nodes with fewer than two neighbors
#' (whose clustering coefficient is undefined) be left out of the average
#' (\code{TRUE}) instead of counted as 0 (\code{FALSE})?
#' Only affects \code{"cc"}.
#' Defaults to \code{FALSE}
#'
#' @param signed Boolean.
#' Passed on to \code{\link{q}}: should modularity be computed on the signed
#' network (Gomez et al., 2009), discounting negative edges?
#' Only affects \code{"q"}, and only matters for a network with negative
#' weights, so it needs \code{weighted = TRUE} (an unweighted network is
#' binarized first, which removes the sign).
#' Defaults to \code{FALSE}
#'
#' @param type Character.
#' Passed on to \code{\link{aspl}}: \code{"arithmetic"} (the mean of each
#' node's mean distance to the nodes it can reach; unreachable pairs are left
#' out) or \code{"harmonic"} (the inverse of the mean inverse distance between
#' all pairs, which counts unreachable pairs). Only affects \code{"aspl"}.
#' Defaults to \code{"arithmetic"}
#'
#' @return Returns a named numeric vector with one value per requested measure
#'
#' @examples
#' # Simulate a toy binarized dataset
#' one <- matrix(sample(0:1, 100, replace = TRUE), nrow = 10, ncol = 10)
#'
#' # Compute a similarity matrix
#' cos <- similarity(one, method = "cosine")
#'
#' # Compute a network
#' net <- suppressWarnings(tmfg(cos))
#'
#' # Compute global network measures
#' global_measures <- semantic_network_measures(net)
#'
#' @references
#' Gomez, S., Jensen, P., & Arenas, A. (2009).
#' Analysis of community structure in networks of correlated data.
#' \emph{Physical Review E}, \emph{80}, 016114.
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Semantic network measures ----
# Updated 13.10.2026
semantic_network_measures <- function(
  A, measures = c("aspl", "cc", "q"), weighted = FALSE, seed = NULL, na.rm = FALSE, signed = FALSE,
  type = c("arithmetic", "harmonic")
)
{

  # Argument errors
  A <- semantic_network_measures_errors(A, measures, weighted, seed, na.rm, signed, type)

  # Set type
  type <- match.arg(type)

  # Check for signed without weights
  if(signed && !weighted && "q" %in% measures){

    # Send warning
    warning(
      "In `semantic_network_measures()`: 'signed' has no effect when `weighted = FALSE` ",
      "(the network is binarized first). Use `weighted = TRUE` for signed modularity.",
      call. = FALSE
    )

  }

  # Check for unweighted
  if(!weighted){

    # Binarize network
    A <- binarize(A)

  }

  # Set up measure functions
  available_measures <- list(
    aspl = function() as.numeric(aspl(A, weighted = weighted, type = type)),
    cc   = function() as.numeric(cc(A, weighted = weighted, na.rm = na.rm)),
    q    = function() as.numeric(q(A, seed = seed, signed = signed))
  )

  # Compute requested measures
  result <- vapply(measures, function(measure) available_measures[[measure]](), numeric(1))

  # Return result
  return(result)

}

#' @noRd
# Errors ----
# Updated 13.10.2026
semantic_network_measures_errors <- function(A, measures, weighted, seed, na.rm, signed, type)
{

  # 'A' errors
  A <- adjacency_matrix_error(A, "semantic_network_measures")

  # 'measures' errors
  length_error(measures, c(1, 2, 3), "semantic_network_measures")
  typeof_error(measures, "character", "semantic_network_measures")

  # Check for unknown measures
  if(!all(measures %in% c("aspl", "cc", "q"))){

    # Send error
    stop(
      "In `semantic_network_measures()`: 'measures' must only contain 'aspl', 'cc', and/or 'q'.",
      call. = FALSE
    )

  }

  # 'weighted' errors
  length_error(weighted, 1, "semantic_network_measures")
  typeof_error(weighted, "logical", "semantic_network_measures")

  # 'seed' errors
  if(!is.null(seed)){
    length_error(seed, 1, "semantic_network_measures")
    typeof_error(seed, "numeric", "semantic_network_measures")
    range_error(seed, c(0, Inf), "semantic_network_measures")
  }

  # 'na.rm' errors
  length_error(na.rm, 1, "semantic_network_measures")
  typeof_error(na.rm, "logical", "semantic_network_measures")

  # 'signed' errors
  length_error(signed, 1, "semantic_network_measures")
  typeof_error(signed, "logical", "semantic_network_measures")

  # 'type' errors
  length_error(type, c(1, 2), "semantic_network_measures")
  typeof_error(type, "character", "semantic_network_measures")

  # Return 'A' (in case of data frame)
  return(A)

}
