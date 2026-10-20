#' @title Finalize a Response Matrix
#'
#' @description Finalizes a binary response matrix by keeping only responses
#' given by at least a minimum number of participants
#'
#' @param data Matrix or data frame.
#' A binary response matrix (participants x responses)
#'
#' @param minimum_cases Numeric.
#' Minimum number of participants (at least \code{1}) required to have given
#' a response for that response to be kept. Only rare responses are removed:
#' a response that \emph{every} participant gave is kept (it makes
#' \code{"cor"} and \code{"phi"} similarities undefined; drop such columns
#' yourself)
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
# Finalize response matrix ----
# Updated 06.09.2026
finalize <- function(data, minimum_cases = 2)
{

  # Argument errors
  data <- finalize_errors(data, minimum_cases)

  # Identify responses given by enough participants
  kept_responses <- which(colSums(data) >= minimum_cases)

  # Check for no remaining responses
  if(length(kept_responses) == 0){

    # Send error
    stop(
      paste0(
        "In `finalize()`: no response was given by at least 'minimum_cases' (",
        minimum_cases, ") participants.\n",
        "Lower 'minimum_cases' or check that `data` is a valid response matrix."
      ),
      call. = FALSE
    )

  }

  # Return kept responses
  return(data[, kept_responses, drop = FALSE])

}

#' @noRd
# Errors ----
# Updated 15.10.2026
finalize_errors <- function(data, minimum_cases)
{

  # 'data' errors
  data <- response_matrix_error(data, "finalize")

  # Check for raw responses
  if(is.character(data) || is.factor(data)){

    # Send error
    stop(
      "In `finalize()`: 'data' contains raw (not-yet-binarized) word responses.\n",
      "'data' must already be a binary (0/1) response matrix.",
      call. = FALSE
    )

  }

  # Check for missing values
  if(anyNA(data)){

    # Send error
    stop(
      "In `finalize()`: 'data' has missing values.\n",
      "Every response of every participant must be 0 or 1 (not given or given).",
      call. = FALSE
    )

  }

  # 'minimum_cases' errors
  length_error(minimum_cases, 1, "finalize")
  typeof_error(minimum_cases, "numeric", "finalize")
  range_error(minimum_cases, c(1, Inf), "finalize")

  # Check for whole number
  if(minimum_cases != round(minimum_cases)){

    # Send error
    stop("In `finalize()`: 'minimum_cases' must be a whole number.", call. = FALSE)

  }

  # Return 'data' (in case of data frame)
  return(data)

}
