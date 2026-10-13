#' @title Simulate a Verbal Fluency Binary Response Matrix
#'
#' @description Simulates verbal fluency data based on the number of nodes
#' desired in the resulting network. Each response's total endorsement count
#' is simulated from a Poisson distribution (see \code{\link{rpois}}), using
#' frequencies from the \code{\link{animals.freq}} data; participants'
#' individual responses are then simulated with a probability of endorsing
#' each response equal to that response's simulated total (plus 1) over 1.05
#' times the largest such total. The probability therefore does not depend on
#' the number of participants, and every response is simulated independently of
#' the others: the data contain no association between responses, so a network
#' estimated from them (which still has its full complement of edges, e.g.
#' \code{\link{tmfg}}) describes noise. Use them to try out functions, not as a
#' model of semantic structure
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
#' @export
# Simulate verbal fluency data ----
# Updated 06.09.2026
simulate_fluency <- function(nodes = 100, cases = 500, random = FALSE)
{

  # Argument errors
  simulate_fluency_errors(nodes, cases, random)

  # Check for random frequencies
  if(random){

    # Sample frequencies with replacement
    sampled_frequencies <- sample(SemNeT::animals.freq, size = nodes, replace = TRUE)

  }else{

    # Take frequencies in order
    sampled_frequencies <- SemNeT::animals.freq[seq_len(nodes)]

  }

  # Simulate total endorsements of each node
  response_totals <- rpois(nodes, sampled_frequencies) + 1

  # Convert totals to endorsement probabilities
  endorsement_probabilities <- response_totals / (max(response_totals) * 1.05)

  # Initialize response matrix
  response_matrix <- matrix(0, nrow = cases, ncol = nodes)

  # Loop over nodes
  for(node in seq_len(nodes)){

    # Simulate responses of node
    response_matrix[, node] <- sample(
      c(0, 1), cases, replace = TRUE,
      prob = c(1 - endorsement_probabilities[node], endorsement_probabilities[node])
    )

  }

  # Identify nodes without responses
  unused_nodes <- which(colSums(response_matrix) == 0)

  # Check for unused nodes
  if(length(unused_nodes) > 0){

    # Loop over unused nodes
    for(node in unused_nodes){

      # Give node one response
      response_matrix[sample(cases, 1), node] <- 1

    }

  }

  # Return response matrix
  return(response_matrix)

}

#' @noRd
# Errors ----
# Updated 13.10.2026
simulate_fluency_errors <- function(nodes, cases, random)
{

  # 'nodes' errors
  length_error(nodes, 1, "simulate_fluency")
  typeof_error(nodes, "numeric", "simulate_fluency")
  range_error(nodes, c(1, Inf), "simulate_fluency")

  # 'cases' errors
  length_error(cases, 1, "simulate_fluency")
  typeof_error(cases, "numeric", "simulate_fluency")
  range_error(cases, c(1, Inf), "simulate_fluency")

  # 'random' errors
  length_error(random, 1, "simulate_fluency")
  typeof_error(random, "logical", "simulate_fluency")

  # Check for more nodes than reference frequencies
  if(!random && nodes > length(SemNeT::animals.freq)){

    # Send error
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
