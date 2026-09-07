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
#' Defaults to \code{NULL} (not reproducible) -- see \code{\link{q}}'s Details
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
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Semantic network measures
# Updated 06.09.2026
semantic_network_measures <- function(A, measures = c("aspl", "cc", "q"), weighted = FALSE, seed = NULL)
{

  # Validate and coerce input before any computation begins
  A <- semantic_network_measures_errors(A, measures, weighted, seed)

  # Unweighted measures need a strictly binary network
  if(!weighted){
    A <- binarize(A)
  }

  # Each requested measure computed via its own dedicated function and
  # collected in a plain named list -- replaces the original's
  # `get()`-by-name/`environment()` metaprogramming, which silently
  # depended on this function's own local variable names matching the
  # requested measure names exactly
  available_measures <- list(
    aspl = function() as.numeric(aspl(A, weighted = weighted)),
    cc   = function() as.numeric(cc(A, weighted = weighted)),
    q    = function() as.numeric(q(A, seed = seed))
  )

  result <- vapply(measures, function(measure) available_measures[[measure]](), numeric(1))

  return(result)

}

#' @noRd
# Argument errors for `semantic_network_measures()`
# Updated 06.09.2026
semantic_network_measures_errors <- function(A, measures, weighted, seed)
{

  # 'A' must be a square, numeric matrix (or coercible data.frame)
  A <- adjacency_matrix_error(A, "semantic_network_measures")

  # 'measures' must be one or more of the documented options
  length_error(measures, c(1, 2, 3), "semantic_network_measures")
  object_error(measures, "vector", "semantic_network_measures")
  typeof_error(measures, "character", "semantic_network_measures")

  if(!all(measures %in% c("aspl", "cc", "q"))){
    stop(
      paste0(
        "In `semantic_network_measures()`: 'measures' must only contain 'aspl', 'cc', and/or 'q'."
      ),
      call. = FALSE
    )
  }

  # 'weighted' must be a single logical value
  object_error(weighted, "vector", "semantic_network_measures")
  typeof_error(weighted, "logical", "semantic_network_measures")
  length_error(weighted, 1, "semantic_network_measures")

  # 'seed' must be `NULL` or a single number -- `q()` validates it again,
  # but checking it here too avoids doing all of the above work first
  if(!is.null(seed)){
    length_error(seed, 1, "semantic_network_measures")
    typeof_error(seed, "numeric", "semantic_network_measures")
  }

  return(A)

}
