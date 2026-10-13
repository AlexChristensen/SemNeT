#' @title Naive Random Walk Network Estimation
#'
#' @description Estimates a semantic network using the Naive Random Walk
#' method described in Lerner, Ogrocki, and Thomas (2009)
#'
#' @param data Matrix or data frame.
#' A preprocessed verbal fluency matrix where rows are participants and
#' columns are the order of their verbal fluency responses. Content may be
#' the raw word responses or an ordered numeric response matrix (see
#' \code{responses2binary()})
#'
#' @param type Character.
#' Type of \code{threshold} to apply. One of:
#' \itemize{
#' \item \code{"num"} --- Minimum number of participants in whose responses
#' the two responses were adjacent
#' \item \code{"prop"} --- Minimum proportion of participants in whose
#' responses the two responses were adjacent
#' }
#' Defaults to \code{"num"}
#'
#' @param threshold Numeric.
#' Value of the minimum number or proportion of co-occurrences.
#' Defaults to \code{0} for both \code{"num"} and \code{"prop"}.
#' With a \code{threshold} above 0, responses left with no link are dropped
#' from the network; at \code{0} every response is kept (including one that
#' was never adjacent to a different response)
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
# Naive random walk network ----
# Updated 06.09.2026
nrw <- function(data, type = c("num", "prop"), threshold = 0)
{

  # Argument errors
  data <- nrw_errors(data, type, threshold)

  # Set type
  type <- match.arg(type)

  # Check for ordered responses
  if(is.numeric(data)){

    # Convert to word responses
    data <- binary2responses(data)

  }

  # Ensure matrix
  mat <- as.matrix(data)

  # Replace bad responses with missing
  mat <- bad_response(mat)

  # Get vocabulary
  vocabulary <- sort(na.omit(unique(as.vector(mat))))

  # Initialize co-occurrence matrix
  co_occurrence <- matrix(0, nrow = length(vocabulary), ncol = length(vocabulary))

  # Set column names
  colnames(co_occurrence) <- vocabulary

  # Set row names
  row.names(co_occurrence) <- vocabulary

  # Loop over participants
  for(participant in seq_len(nrow(mat))){

    # Get responses of participant
    responses_given <- mat[participant, ]

    # Get first response of each pair
    first <- match(responses_given[-length(responses_given)], vocabulary)

    # Get second response of each pair
    second <- match(responses_given[-1], vocabulary)

    # Identify adjacent distinct responses
    adjacent <- !is.na(first) & !is.na(second) & first != second

    # Check for adjacent responses
    if(any(adjacent)){

      # Get unique pairs
      pairs <- unique(cbind(pmin(first[adjacent], second[adjacent]), pmax(first[adjacent], second[adjacent])))

      # Count pairs
      co_occurrence[pairs] <- co_occurrence[pairs] + 1

      # Count reversed pairs
      co_occurrence[pairs[, 2:1, drop = FALSE]] <- co_occurrence[pairs[, 2:1, drop = FALSE]] + 1

    }

  }

  # Check for proportion
  if(type == "prop"){

    # Convert counts to proportions
    co_occurrence <- co_occurrence / nrow(data)

  }

  # Remove co-occurrences below threshold
  co_occurrence[co_occurrence < threshold] <- 0

  # Check for threshold
  if(threshold != 0){

    # Binarize co-occurrences at threshold
    co_occurrence[co_occurrence >= threshold] <- 1

    # Identify nodes without edges
    disconnected <- which(colSums(co_occurrence) == 0)

    # Check for disconnected nodes
    if(length(disconnected) > 0){

      # Remove disconnected nodes
      co_occurrence <- co_occurrence[-disconnected, -disconnected, drop = FALSE]

    }

  }else{

    # Binarize co-occurrences
    co_occurrence[co_occurrence != 0] <- 1

  }

  # Return network
  return(co_occurrence)

}

#' @noRd
# Errors ----
# Updated 13.10.2026
nrw_errors <- function(data, type, threshold)
{

  # 'data' errors
  data <- ordered_response_matrix_error(data, "nrw")

  # 'type' errors
  length_error(type, c(1, 2), "nrw")
  typeof_error(type, "character", "nrw")

  # 'threshold' errors
  length_error(threshold, 1, "nrw")
  typeof_error(threshold, "numeric", "nrw")

  # Check for proportion
  if(length(type) == 1 && type == "prop"){

    # Proportions range from 0 to 1
    range_error(threshold, c(0, 1), "nrw")

  }else{

    # Counts range from 0 upward
    range_error(threshold, c(0, Inf), "nrw")

  }

  # Return 'data' (in case of data frame)
  return(data)

}
