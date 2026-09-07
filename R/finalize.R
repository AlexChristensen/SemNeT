#' @title Finalize a Response Matrix
#'
#' @description Finalizes a binary response matrix by keeping only responses
#' given by at least a minimum number of participants
#'
#' @param data Matrix or data frame.
#' A binary response matrix (participants x responses)
#'
#' @param minimum_cases Numeric.
#' Minimum number of participants required to have given a response for that
#' response to be kept.
#' Defaults to \code{2}
#'
#' @return A binary response matrix containing only responses given by at
#' least \code{minimum_cases} participants
#'
#' @examples
#' # Obtain binary data
#' bin <- open.binary
#'
#' # Finalize
#' final <- finalize(bin)
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Finalize a response matrix
# Updated 06.09.2026
finalize <- function(data, minimum_cases = 2)
{

  # Validate and coerce input before any computation begins
  data <- finalize_errors(data, minimum_cases)

  # Keep only responses given by at least 'minimum_cases' participants
  kept_responses <- which(colSums(data) >= minimum_cases)

  # No response met the threshold -- the original silently returned a
  # useless 0-column matrix here, which would then fail confusingly in
  # whatever downstream function received it (e.g. `equate()`, a network
  # estimator). A clear error at the source is easier to act on
  if(length(kept_responses) == 0){
    stop(
      paste0(
        "In `finalize()`: no response was given by at least 'minimum_cases' (",
        minimum_cases, ") participants.\n",
        "Lower 'minimum_cases' or check that `data` is a valid response matrix."
      ),
      call. = FALSE
    )
  }

  return(data[, kept_responses, drop = FALSE])

}

#' @noRd
# Argument errors for `finalize()`
# Updated 06.09.2026
finalize_errors <- function(data, minimum_cases)
{

  # 'data' must be a matrix or a data.frame, already binary (or coercible).
  # `response_matrix_error()` also accepts raw (not-yet-binarized) word
  # responses -- appropriate for `pf()`/`tmfg()`, which convert that
  # themselves, but `finalize()`'s own body (`colSums()`) assumes numeric
  # 0/1 content and would otherwise fail with a confusing base-R error
  # ("'x' must be numeric or complex") instead of a clear one
  data <- response_matrix_error(data, "finalize")

  if(is.character(data) || is.factor(data)){
    stop(
      "In `finalize()`: 'data' contains raw (not-yet-binarized) word responses.\n",
      "'data' must already be a binary (0/1) response matrix.",
      call. = FALSE
    )
  }

  # 'minimum_cases' must be a single non-negative whole number
  length_error(minimum_cases, 1, "finalize")
  typeof_error(minimum_cases, "numeric", "finalize")
  range_error(minimum_cases, c(0, Inf), "finalize")

  return(data)

}
