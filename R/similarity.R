#' @title Measures of Similarity
#'
#' @description Computes a similarity matrix between the columns of a
#' binarized verbal fluency or linguistic dataset (see Choi, Cha, & Tappert,
#' 2010 for additional measures)
#'
#' @param data Matrix or data frame.
#' A binarized dataset of verbal fluency or linguistic data
#'
#' @param method Character.
#' Type of similarity measure to compute. One of \code{"cosine"} (default),
#' \code{"angular"}, \code{"cor"}, \code{"euclid"}, \code{"faith"},
#' \code{"jaccard"}, \code{"phi"}, or \code{"rr"}. Defaults to \code{"cosine"}.
#' \code{"euclid"} is the Euclidean \emph{distance} between columns converted
#' to a similarity, \eqn{1 / (1 + d)}, so that (like every other option) larger
#' values mean more similar and the matrix can be used to estimate a network
#' (e.g., \code{\link{tmfg}})
#'
#' @return A symmetric similarity matrix. The diagonal is set to 1
#'
#' @details \code{"angular"}, \code{"faith"}, \code{"jaccard"} and \code{"rr"}
#' are computed from the 2 x 2 table of two columns' agreements and
#' disagreements across participants, which needs 0/1 data (other values are an
#' error); missing or infinite values are an error for every measure. A response nobody gave (an all-zero column) makes
#' \code{"cosine"}, \code{"angular"} and \code{"jaccard"} undefined (\code{NaN}),
#' and a response everybody gave (or nobody) makes \code{"cor"}/\code{"phi"}
#' undefined; remove such responses first (see \code{\link{finalize}}). A
#' participant who gave no response still counts as a case in which every
#' response is absent, which matters for the measures that use joint absences
#' (\code{"cor"}, \code{"phi"}, \code{"faith"}, \code{"rr"}). \code{"euclid"}
#' mostly reflects how often each response was given: two rare responses that
#' never co-occur look similar.
#'
#' @examples
#' # Simulate a toy binarized dataset
#' one <- matrix(sample(0:1, 100, replace = TRUE), nrow = 10, ncol = 10)
#'
#' # Compute a cosine similarity matrix
#' cos <- similarity(one, method = "cosine")
#'
#' @references
#' Choi, S. S., Cha, S. H., & Tappert, C. C. (2010).
#' A survey of binary similarity and distance measures.
#' \emph{Journal of Systemics, Cybernetics and Informatics}, \emph{8}, 43-48.
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Similarity measures ----
# Updated 06.09.2026
similarity <- function(data, method = c("cosine", "angular", "cor", "euclid", "faith", "jaccard", "phi", "rr"))
{

  # Argument errors
  data <- similarity_errors(data, method)

  # Set method
  method <- match.arg(method)

  # Check for cosine
  if(method == "cosine"){

    # Return cosine similarity
    return(cosine(as.matrix(data)))

  }

  # Check for correlation
  if(method %in% c("cor", "phi")){

    # Return correlation (phi for binary data)
    return(cor(data))

  }

  # Check for Euclidean
  if(method == "euclid"){

    # Return Euclidean distance as similarity
    return(1 / (1 + as.matrix(dist(t(data), diag = TRUE, upper = TRUE))))

  }

  # Ensure matrix
  binary_matrix <- as.matrix(data)

  # Get number of cases
  n_cases <- nrow(binary_matrix)

  # Get number of nodes
  n_nodes <- ncol(binary_matrix)

  # Get responses given by both (a)
  a <- crossprod(binary_matrix)

  # Get times each response was given
  ones <- colSums(binary_matrix)

  # Get times each response was given by row
  ones_by_row <- matrix(ones, nrow = n_nodes, ncol = n_nodes)

  # Get times each response was given by column
  ones_by_col <- t(ones_by_row)

  # Get responses given by row only (b)
  b <- ones_by_row - a

  # Get responses given by column only (c)
  c <- ones_by_col - a

  # Get responses given by neither (d)
  d <- n_cases - a - b - c

  # Compute similarity
  mat <- switch(
    method,
    angular = 1 - (2 * acos(a / sqrt(ones_by_row * ones_by_col)) / pi),
    faith   = (a + 0.5 * d) / (a + b + c + d),
    jaccard = a / (a + b + c),
    rr      = a / (a + b + c + d)
  )

  # Set self-similarity to one
  diag(mat) <- 1

  # Set column names
  colnames(mat) <- colnames(data)

  # Set row names
  row.names(mat) <- colnames(data)

  # Return similarity
  return(mat)

}

#' @noRd
# Errors ----
# Updated 13.10.2026
similarity_errors <- function(data, method)
{

  # 'data' errors
  object_error(data, c("matrix", "data.frame"), "similarity")

  # Check for data frame
  if(is.data.frame(data)){

    # Convert to matrix
    data <- as.matrix(data)

  }

  # Check for numeric
  typeof_error(data, "numeric", "similarity")

  # Check for missing or infinite values
  if(!all(is.finite(data))){

    # Send error
    stop(
      "In `similarity()`: 'data' has missing or infinite values.\n",
      "Every response of every participant must be 0 or 1 (not given or given).",
      call. = FALSE
    )

  }

  # 'method' errors
  length_error(method, c(1, 8), "similarity")
  typeof_error(method, "character", "similarity")

  # Get chosen method
  chosen <- if(length(method) == 8) method[1] else method

  # Check for binary methods without binary data
  if(chosen %in% c("angular", "faith", "jaccard", "rr") && !all(data %in% c(0, 1))){

    # Send error
    stop(
      "In `similarity()`: method '", chosen, "' needs binary (0/1) data, but 'data' has other values.\n",
      "Convert the responses first (see `responses2binary()`).",
      call. = FALSE
    )

  }

  # Return 'data' (in case of data frame)
  return(data)

}
