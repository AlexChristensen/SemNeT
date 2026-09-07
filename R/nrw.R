#' @title Naive Random Walk Network Estimation
#'
#' @description Estimates a semantic network using the Naive Random Walk
#' method described in Lerner, Ogrocki, and Thomas (2009)
#'
#' @param data Matrix or data frame.
#' A preprocessed verbal fluency matrix where rows are participants and
#' columns are the order of their verbal fluency responses. Content may be
#' the raw word responses or an ordered numeric response matrix (see
#' \code{responses_to_binary()})
#'
#' @param type Character.
#' Type of \code{threshold} to apply. One of:
#' \itemize{
#' \item \code{"num"} --- Minimum number of co-occurrences
#' \item \code{"prop"} --- Minimum proportion of co-occurrences
#' }
#' Defaults to \code{"num"}
#'
#' @param threshold Numeric.
#' Value of the minimum number or proportion of co-occurrences.
#' Defaults to \code{0} for both \code{"num"} and \code{"prop"}
#'
#' @return Returns an undirected semantic network
#'
#' @examples
#' # Get data
#' data <- open.clean
#'
#' # Organize group data
#' group <- open.group
#' low <- data[which(group == "Low"), ]
#' high <- data[which(group == "High"), ]
#'
#' # Compute networks
#' low_net <- nrw(low)
#' high_net <- nrw(high)
#'
#' @references
#' Lerner, A. J., Ogrocki, P. K., & Thomas, P. J. (2009).
#' Network graph analysis of category fluency testing.
#' \emph{Cognitive and Behavioral Neurology}, \emph{22}, 45-52.
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Naive random walk network estimation
# Updated 06.09.2026
nrw <- function(data, type = c("num", "prop"), threshold = 0)
{

  # Validate and coerce input before any computation begins
  data <- nrw_errors(data, type, threshold)

  # `match.arg()` also supplies the "num" default when 'type' is missing
  type <- match.arg(type)

  # An ordered numeric response matrix must first become word responses
  if(is.numeric(data)){
    data <- binary_to_responses(data)
  }

  mat <- as.matrix(data)
  mat <- bad_response(mat)

  vocabulary <- sort(na.omit(unique(as.vector(mat))))

  co_occurrence <- matrix(0, nrow = length(vocabulary), ncol = length(vocabulary))
  colnames(co_occurrence) <- vocabulary
  row.names(co_occurrence) <- vocabulary

  # Pad each participant's response row with NA at both ends, so a response
  # in the first or last position still has a defined (empty) neighbor
  padded_responses <- cbind(rep(NA, nrow(mat)), mat, rep(NA, nrow(mat)))

  # Each participant's response order is ragged (some gave more responses
  # than others), so counting adjacent-response pairs stays a loop
  for(participant in seq_len(nrow(padded_responses))){

    responses_given <- padded_responses[participant, !is.na(padded_responses[participant, ])]
    full_row <- padded_responses[participant, ]

    for(response in seq_along(responses_given)){

      position <- which(full_row == responses_given[response])
      neighbors <- na.omit(full_row[c(position - 1, position + 1)])

      if(length(neighbors) != 0){
        co_occurrence[responses_given[response], neighbors] <- co_occurrence[responses_given[response], neighbors] + 1
        co_occurrence[neighbors, responses_given[response]] <- co_occurrence[neighbors, responses_given[response]] + 1
      }

    }

  }

  # Convert counts to a proportion of participants when requested
  if(type == "prop"){
    co_occurrence <- co_occurrence / nrow(data)
  }

  # Threshold the (count or proportion) co-occurrence matrix down to a binary network
  co_occurrence[co_occurrence < threshold] <- 0

  if(threshold != 0){

    co_occurrence[co_occurrence >= threshold] <- 1

    # Nodes that lost every connection at this threshold are dropped entirely
    disconnected <- which(colSums(co_occurrence) == 0)
    if(length(disconnected) > 0){
      co_occurrence <- co_occurrence[-disconnected, -disconnected]
    }

  }else{
    co_occurrence[co_occurrence != 0] <- 1
  }

  return(co_occurrence)

}

#' @noRd
# Argument errors for `nrw()`
# Updated 06.09.2026
nrw_errors <- function(data, type, threshold)
{

  # 'data' must be a raw (not-yet-binarized) response matrix -- naive random
  # walk needs response order, which a plain 0/1 binary matrix has already
  # discarded. Fixes the original's `max() >= 1` bug (see
  # `ordered_response_matrix_error()` in helpers.R), which rejected
  # virtually every valid ordered numeric response matrix
  data <- ordered_response_matrix_error(data, "nrw")

  # 'type' must be one (and only one) of the documented options
  length_error(type, c(1, 2), "nrw")

  # 'threshold' must be a single non-negative number, and additionally
  # below 1 when 'type' is "prop" (a proportion can't exceed 1)
  length_error(threshold, 1, "nrw")
  typeof_error(threshold, "numeric", "nrw")

  # `nrw()` always forwards 'type' here explicitly (even when its own caller
  # left it at the default), so `missing(type)` would always read `FALSE` --
  # `length(type) == 1` is the actual signal that a single choice was made
  if(length(type) == 1 && type == "prop"){
    range_error(threshold, c(0, 1), "nrw")
  }else{
    range_error(threshold, c(0, Inf), "nrw")
  }

  return(data)

}
