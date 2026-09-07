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
#' \code{"jaccard"}, \code{"phi"}, or \code{"rr"}. Defaults to \code{"cosine"}
#'
#' @return A symmetric similarity matrix
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
#' @importFrom stats cor dist
#'
#' @export
# Similarity measures
# Updated 06.09.2026
similarity <- function(data, method = c("cosine", "angular", "cor", "euclid", "faith", "jaccard", "phi", "rr"))
{

  # Validate and coerce input before any computation begins
  data <- similarity_errors(data, method)

  # `match.arg()` picks the *first* listed choice when 'method' is missing,
  # which is why "cosine" is listed first above -- it's the documented default
  method <- match.arg(method)

  # Cosine is already a single vectorized matrix computation
  if(method == "cosine"){
    return(cosine(as.matrix(data)))
  }

  # `cor`/`phi` are mathematically identical for binary data -- the Pearson
  # correlation of two 0/1 columns *is* the phi coefficient. The original
  # package tried to compute a separate "phi" formula but placed it in
  # unreachable code, so both names have always resolved to `cor()` in
  # practice; this makes that the documented behavior instead of dead code
  if(method %in% c("cor", "phi")){
    return(cor(data))
  }

  # Euclidean distance between columns, always returned as a plain matrix
  # (the original returned a `dist` object here, but a matrix everywhere
  # else -- an inconsistent return type depending on 'method')
  if(method == "euclid"){
    return(as.matrix(dist(t(data), diag = TRUE, upper = TRUE)))
  }

  # Everything else ("angular", "faith", "jaccard", "rr") is a function of
  # the four cells of every pairwise 2x2 contingency table:
  #   a = both columns 1, b = row-column 1 only, c = column-column 1 only, d = both 0
  # All four quantities are obtainable from one matrix product plus the
  # column sums, in place of the original's per-pair for-loop over `data`
  binary_matrix <- as.matrix(data)
  n_cases <- nrow(binary_matrix)
  n_nodes <- ncol(binary_matrix)

  # a[i, j] = number of participants giving both response i and response j
  a <- crossprod(binary_matrix)

  # Total times each response was given -- recycled down columns (every
  # column of this matrix equals 'ones', so entry [i, j] is ones[i])
  ones <- colSums(binary_matrix)
  ones_by_row <- matrix(ones, nrow = n_nodes, ncol = n_nodes)
  ones_by_col <- t(ones_by_row)

  # b[i, j] = response i given, j not; c[i, j] = response j given, i not
  b <- ones_by_row - a
  c <- ones_by_col - a
  d <- n_cases - a - b - c

  # Compute the requested measure across the whole matrix at once -- each
  # formula below is symmetric in (i, j), so no separate lower-triangle
  # fill-and-mirror step is needed the way the original required
  mat <- switch(
    method,
    angular = 1 - (2 * acos(a / sqrt(ones_by_row * ones_by_col)) / pi),
    faith   = (a + 0.5 * d) / (a + b + c + d),
    jaccard = a / (a + b + c),
    rr      = a / (a + b + c + d)
  )

  # Self-similarity is always 1, regardless of formula edge cases at a==ones
  diag(mat) <- 1

  colnames(mat) <- colnames(data)
  row.names(mat) <- colnames(data)

  return(mat)

}

#' @noRd
# Argument errors for `similarity()`
# Updated 06.09.2026
similarity_errors <- function(data, method)
{

  # 'data' must be a matrix or a data.frame (coercible to a matrix)
  object_error(data, c("matrix", "data.frame"), "similarity")

  # Coerce a data.frame input into a plain matrix
  if(is.data.frame(data)){
    data <- as.matrix(data)
  }

  # 'data' must hold numeric values
  typeof_error(data, "numeric", "similarity")

  # 'method' must be one (and only one) of the documented options
  length_error(method, c(1, 8), "similarity")

  return(data)

}
