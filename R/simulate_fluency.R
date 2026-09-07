#' @title Simulate a Verbal Fluency Binary Response Matrix
#'
#' @description Simulates verbal fluency data based on the number of nodes
#' desired in the resulting network. Each response's total endorsement count
#' is simulated from a Poisson distribution (see \code{\link{rpois}}), using
#' frequencies from the \code{\link{animals.freq}} data; participants'
#' individual responses are then simulated with a probability of endorsing
#' each response equal to that response's simulated total over the number
#' of participants
#'
#' @param nodes Numeric.
#' Number of nodes (unique responses) to simulate.
#' Defaults to \code{100}
#'
#' @param cases Numeric.
#' Number of participants to simulate.
#' Defaults to \code{500}
#'
#' @param random Boolean.
#' Should response frequencies be randomly sampled (with replacement) from
#' \code{\link{animals.freq}}, rather than taken in their existing order?
#' Defaults to \code{FALSE}
#'
#' @return A binary matrix with \code{cases} rows (participants) and
#' \code{nodes} columns (responses)
#'
#' @examples
#' # Simulate data for 50 nodes and 200 participants
#' simulate_fluency(nodes = 50, cases = 200)
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @importFrom stats rpois
#'
#' @export
# Simulate verbal fluency data
# Updated 06.09.2026
simulate_fluency <- function(nodes = 100, cases = 500, random = FALSE)
{

  # Validate and coerce input before any computation begins
  simulate_fluency_errors(nodes, cases, random)

  # Response frequencies to simulate each node's popularity from -- sampled
  # (with replacement) when 'random' is requested, otherwise taken as-is in
  # `animals.freq`'s existing order (only meaningful when 'nodes' doesn't
  # exceed how many frequencies exist to take, checked in the errors above)
  if(random){
    sampled_frequencies <- sample(SemNeT::animals.freq, size = nodes, replace = TRUE)
  }else{
    sampled_frequencies <- SemNeT::animals.freq[seq_len(nodes)]
  }

  # Each response's simulated total endorsement count across all participants
  response_totals <- rpois(nodes, sampled_frequencies) + 1

  # Convert totals to endorsement probabilities, scaled so even the most
  # popular response has some chance of *not* being endorsed by a participant
  endorsement_probabilities <- response_totals / (max(response_totals) * 1.05)

  # Initialize the response matrix
  response_matrix <- matrix(0, nrow = cases, ncol = nodes)

  # Each node's responses are simulated independently, so this stays a loop
  # rather than a single vectorized draw
  for(node in seq_len(nodes)){

    response_matrix[, node] <- sample(
      c(0, 1), cases, replace = TRUE,
      prob = c(1 - endorsement_probabilities[node], endorsement_probabilities[node])
    )

  }

  # Guarantee every node has at least one response -- otherwise a node could
  # simulate to a fully unused (all-zero) column, which is meaningless as a
  # "response" and would break downstream functions that assume every column
  # was actually given by someone
  unused_nodes <- which(colSums(response_matrix) == 0)

  if(length(unused_nodes) > 0){
    for(node in unused_nodes){
      response_matrix[sample(cases, 1), node] <- 1
    }
  }

  return(response_matrix)

}

#' @noRd
# Argument errors for `simulate_fluency()`
# Updated 06.09.2026
simulate_fluency_errors <- function(nodes, cases, random)
{

  # 'nodes' must be a single positive whole number
  length_error(nodes, 1, "simulate_fluency")
  typeof_error(nodes, "numeric", "simulate_fluency")
  range_error(nodes, c(1, Inf), "simulate_fluency")

  # 'cases' must be a single positive whole number
  length_error(cases, 1, "simulate_fluency")
  typeof_error(cases, "numeric", "simulate_fluency")
  range_error(cases, c(1, Inf), "simulate_fluency")

  # 'random' must be a single logical value
  object_error(random, "vector", "simulate_fluency")
  typeof_error(random, "logical", "simulate_fluency")
  length_error(random, 1, "simulate_fluency")

  # Without 'random', frequencies are taken in existing order from a
  # fixed-length reference -- requesting more nodes than that has no
  # well-defined behavior (the original recycled `animals.freq` instead,
  # which risks the `rpois()`-internal "longer object length is not a
  # multiple of shorter object length" warning rather than a clear error)
  if(!random && nodes > length(SemNeT::animals.freq)){
    stop(
      paste0(
        "In `simulate_fluency()`: 'nodes' (", nodes, ") exceeds the number of available ",
        "reference frequencies (", length(SemNeT::animals.freq), ").\n",
        "Set `random = TRUE` to sample frequencies with replacement instead."
      ),
      call. = FALSE
    )
  }

}
