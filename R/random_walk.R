#' @title Random Walk Simulation
#'
#' @description Simulates random walks over two networks to examine the
#' characteristics of spontaneous spreading activation (see Kenett & Austerweil, 2016)
#'
#' @param A Matrix or data frame.
#' Adjacency matrix of a semantic network
#'
#' @param B Matrix or data frame.
#' A comparison adjacency matrix of a semantic network
#'
#' @param name_a Character.
#' Label for network \code{A} in the output.
#' Defaults to \code{"A"}
#'
#' @param name_b Character.
#' Label for network \code{B} in the output.
#' Defaults to \code{"B"}
#'
#' @param reps Numeric.
#' Sets the largest number of steps tested, \code{reps * 10}: walks are run for
#' \code{steps}, \code{steps + 10}, \code{steps + 20}, ... up to
#' \code{reps * 10} steps (so \code{steps} cannot exceed \code{reps * 10}).
#' Defaults to \code{20} (up to 200 steps)
#'
#' @param steps Numeric.
#' Number of random steps to begin with.
#' Defaults to \code{10}
#'
#' @param iter Numeric.
#' Number of iterations for each random walk.
#' Defaults to \code{10000}
#'
#' @param cores Numeric.
#' Number of computer processing cores to use.
#' Defaults to \code{1}
#'
#' @details The comparison is descriptive. Each network's walks are
#' \code{iter} simulated draws, and \code{iter} is chosen by the user, so the
#' \emph{p}-value of a test on those draws would only reflect how many walks
#' were simulated: any difference, however small, becomes \dQuote{significant}
#' with enough of them. No \emph{p}-values are reported. Each statistic
#' (number of unique nodes visited, similarity between the start and the last
#' newly visited node of the walk, and the same at the 5th unique node) is
#' instead summarized by
#' \itemize{
#'
#' \item the difference in means (\code{A} minus \code{B}) with a Monte Carlo
#' 95\% interval, which describes the error from simulating a finite number
#' of walks (and shrinks as \code{iter} grows), not uncertainty about the
#' networks
#'
#' \item Cohen's \emph{d}, which does not depend on \code{iter}
#'
#' }
#' Neither accounts for the uncertainty in the estimated networks; use
#' \code{\link{bootstrap_SemNeT}} for that. The number of unique nodes visited
#' depends mechanically on a network's size and density, so networks of
#' different sizes (e.g., unequated networks) differ on it partly for that
#' reason alone.
#'
#' Every walk starts at a node drawn at random from the nodes with at least one
#' connection in each network (an isolated node cannot be walked from), and the
#' two networks' walks share each step's random draw. When the pooled standard
#' deviation of a statistic is 0, \eqn{d} is \code{0} for no difference in
#' means and \code{Inf} (signed) for a difference, since any difference is then
#' infinitely large in standard deviation units. The 5-unique-node statistic is
#' measured at each network's own 5th distinct node (or its last, if the walk
#' visited fewer).
#'
#' @return A list with \code{long} (every simulated summary statistic, all
#' numeric: each network's mean and standard deviation, and the difference,
#' its Monte Carlo interval limits, and Cohen's \emph{d} for each statistic)
#' and \code{short} (a compact, correctly-typed summary with the difference,
#' Cohen's \emph{d}, and a direction that is the network with the larger mean
#' when \eqn{|d| \ge .2} (Cohen's small effect) and \code{"negligible"}
#' otherwise)
#'
#' @examples
#' # Simulate toy binarized datasets
#' one <- matrix(sample(0:1, 100, replace = TRUE), nrow = 10, ncol = 10)
#' two <- matrix(sample(0:1, 100, replace = TRUE), nrow = 10, ncol = 10)
#'
#' # Compute similarity matrices
#' cos1 <- similarity(one, method = "cosine")
#' cos2 <- similarity(two, method = "cosine")
#'
#' # Compute networks
#' net1 <- suppressWarnings(tmfg(cos1))
#' net2 <- suppressWarnings(tmfg(cos2))
#'
#' \dontshow{rw_results <- random_walk(net1, net2, iter = 10, cores = 1)}
#' \donttest{
#' # Run random walk analysis
#' rw_results <- random_walk(net1, net2, iter = 100, cores = 2)
#' }
#'
#' @references
#' Kenett, Y. N., & Austerweil, J. L. (2016).
#' Examining search processes in low and high creative individuals with random walks.
#' In \emph{Paper presented at the proceedings of the 38th annual meeting of the cognitive science society}. Austin, TX.
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com> and Yoed Kenett <yoedkenett@gmail.com>
#'
#' @export
# Random walk simulation ----
# Updated 15.10.2026
random_walk <- function(A, B, name_a = "A", name_b = "B", reps = 20, steps = 10, iter = 10000, cores = 1)
{

  # Argument errors
  validated <- random_walk_errors(A, B, name_a, name_b, reps, steps, iter, cores)

  # Get first network
  A <- validated$A

  # Get second network
  B <- validated$B

  # Get number of nodes of first network
  n_a <- ncol(A)

  # Get number of nodes of second network
  n_b <- ncol(B)

  # Binarize first network
  A <- binarize(A)

  # Binarize second network
  B <- binarize(B)

  # Remove self-loops of first network
  diag(A) <- 0

  # Remove self-loops of second network
  diag(B) <- 0

  # Get degree of first network
  degree_a <- colSums(A)

  # Get degree of second network
  degree_b <- colSums(B)

  # Get starting nodes of first network
  start_a <- which(degree_a > 0)

  # Get starting nodes of second network
  start_b <- which(degree_b > 0)

  # Check for no edges
  if(length(start_a) == 0 || length(start_b) == 0){

    # Send error
    stop("In `random_walk()`: both networks must have at least one edge to walk on.", call. = FALSE)

  }

  # Get transition probabilities of first network
  transition_a <- A / degree_a

  # Get transition probabilities of second network
  transition_b <- B / degree_b

  # Get distances of first network
  distance_a <- distance(A)

  # Get distances of second network
  distance_b <- distance(B)

  # Get cumulative transitions of first network
  cumulative_transition_a <- cumulative_transitions(transition_a, A)

  # Get cumulative transitions of second network
  cumulative_transition_b <- cumulative_transitions(transition_b, B)

  # Simulate one walk on both networks
  simulate_walk <- function(n_steps, n_a, n_b, cumulative_transition_a, cumulative_transition_b, distance_a, distance_b)
  {

    # Initialize visited nodes of first network
    visited_a <- numeric(n_steps)

    # Initialize visited nodes of second network
    visited_b <- numeric(n_steps)

    # Sample start of first network
    current_a <- start_a[sample.int(length(start_a), 1)]

    # Sample start of second network
    current_b <- start_b[sample.int(length(start_b), 1)]

    # Record start of first network
    visited_a[1] <- current_a

    # Record start of second network
    visited_b[1] <- current_b

    # Check for more than one step
    if(n_steps > 1){

      # Loop over steps
      for(step in 2:n_steps){

        # Draw random number
        draw <- runif(1)

        # Get next node of first network
        next_a <- which(cumulative_transition_a[current_a, ] > draw)[1]

        # Get next node of second network
        next_b <- which(cumulative_transition_b[current_b, ] > draw)[1]

        # Record next node of first network
        visited_a[step] <- next_a

        # Record next node of second network
        visited_b[step] <- next_b

        # Move first network
        current_a <- next_a

        # Move second network
        current_b <- next_b

      }

    }

    # Get unique nodes of first network
    unique_a <- unique(visited_a)

    # Get unique nodes of second network
    unique_b <- unique(visited_b)

    # Get similarity of first and last node of first network
    similarity_a <- exp(-distance_a[visited_a[1], unique_a[length(unique_a)]])

    # Get similarity of first and last node of second network
    similarity_b <- exp(-distance_b[visited_b[1], unique_b[length(unique_b)]])

    # Get similarity at five unique nodes of first network
    similarity_a_5 <- exp(-distance_a[visited_a[1], unique_a[min(5, length(unique_a))]])

    # Get similarity at five unique nodes of second network
    similarity_b_5 <- exp(-distance_b[visited_b[1], unique_b[min(5, length(unique_b))]])

    # Return statistics of walk
    return(list(
      unique = c(length(unique_a), length(unique_b)),
      similarity = c(similarity_a, similarity_b),
      similarity_5 = c(similarity_a_5, similarity_b_5)
    ))

  }

  # Get step levels
  step_levels <- seq(steps, reps * 10, 10)

  # Send message
  message("Computing random walks...")

  # Loop over step levels
  simulations_by_level <- lapply(seq_along(step_levels), function(level){

    # Send progress message
    message(paste0("Repetition ", level, " of ", length(step_levels), " (", step_levels[level], " steps)"))

    # Simulate walks of level
    parallel_process(
      iter,
      function(i) simulate_walk(step_levels[level], n_a, n_b, cumulative_transition_a, cumulative_transition_b, distance_a, distance_b),
      cores = cores,
      progress = FALSE
    )

  })

  # Format long results
  long_results <- format_random_walk_long(simulations_by_level, step_levels, name_a, name_b)

  # Format short results
  short_results <- format_random_walk_short(long_results, name_a, name_b)

  # Return results
  return(list(long = long_results, short = short_results))

}

#' @noRd
# Cumulative transition probabilities ----
# Updated 15.10.2026
cumulative_transitions <- function(transition, A)
{

  # Get cumulative sums of each node's transition probabilities
  cumulative <- t(apply(transition, 1, cumsum))

  # Loop over nodes with neighbors
  for(node in which(rowSums(A) > 0)){

    # Get last neighbor
    last_neighbor <- max(which(A[node, ] > 0))

    # Set the cumulative probability to exactly one from the last neighbor on
    # (rounding can leave it just below one, which would let a draw fall past it)
    cumulative[node, last_neighbor:ncol(cumulative)] <- 1

  }

  # Return cumulative transitions
  return(cumulative)

}

#' @noRd
# Format long random walk results ----
# Updated 13.10.2026
format_random_walk_long <- function(simulations_by_level, step_levels, name_a, name_b)
{

  # Set statistics
  statistics <- c("uniq", "sim", "sim5")

  # Initialize results
  results <- matrix(NA, nrow = length(step_levels), ncol = 25)

  # Set column names
  colnames(results) <- c(
    "Steps",
    paste("M.uniq", name_a, sep = "."), paste("SD.uniq", name_a, sep = "."),
    paste("M.sim", name_a, sep = "."), paste("SD.sim", name_a, sep = "."),
    paste("M.sim5", name_a, sep = "."), paste("SD.sim5", name_a, sep = "."),
    paste("M.uniq", name_b, sep = "."), paste("SD.uniq", name_b, sep = "."),
    paste("M.sim", name_b, sep = "."), paste("SD.sim", name_b, sep = "."),
    paste("M.sim5", name_b, sep = "."), paste("SD.sim5", name_b, sep = "."),
    paste0(rep(c("diff", "lower", "upper", "d"), times = 3), ".", rep(statistics, each = 4))
  )

  # Summarize difference of a statistic
  summarize_statistic <- function(values){

    # Get paired differences
    differences <- values[, 1] - values[, 2]

    # Get Monte Carlo error
    monte_carlo_error <- 1.96 * sd(differences) / sqrt(length(differences))

    # Get pooled standard deviation
    pooled_sd <- sqrt((var(values[, 1]) + var(values[, 2])) / 2)

    # Compute effect size
    effect <- if(isTRUE(pooled_sd > 0)){

      # Standardize difference
      mean(differences) / pooled_sd

    }else if(isTRUE(pooled_sd == 0)){

      # Set zero or infinite effect
      sign(mean(differences)) * ifelse(mean(differences) == 0, 0, Inf)

    }else{

      # Set undefined effect
      NA_real_

    }

    # Return difference, interval, and effect
    c(mean(differences), mean(differences) - monte_carlo_error, mean(differences) + monte_carlo_error, effect)

  }

  # Loop over step levels
  for(level in seq_along(step_levels)){

    # Get simulations of level
    simulations <- simulations_by_level[[level]]

    # Get unique node counts
    unique_counts <- t(sapply(simulations, function(x) x$unique))

    # Get similarities
    similarities <- t(sapply(simulations, function(x) x$similarity))

    # Get five-unique-node similarities
    similarities_5 <- t(sapply(simulations, function(x) x$similarity_5))

    # Set results of level
    results[level, ] <- c(
      step_levels[level],
      mean(unique_counts[, 1]), sd(unique_counts[, 1]),
      mean(similarities[, 1]), sd(similarities[, 1]),
      mean(similarities_5[, 1]), sd(similarities_5[, 1]),
      mean(unique_counts[, 2]), sd(unique_counts[, 2]),
      mean(similarities[, 2]), sd(similarities[, 2]),
      mean(similarities_5[, 2]), sd(similarities_5[, 2]),
      summarize_statistic(unique_counts), summarize_statistic(similarities), summarize_statistic(similarities_5)
    )

  }

  # Return results
  return(results)

}

# Format short random walk results ----
# Updated 13.10.2026
format_random_walk_short <- function(long_results, name_a, name_b)
{

  # Label direction of effect
  direction <- function(d){
    swiftelse(
      is.na(d), "undefined",
      swiftelse(
        abs(d) < .2, "negligible",
        swiftelse(d > 0, paste(name_a, ">", name_b), paste(name_b, ">", name_a))
      )
    )
  }

  # Return summary
  data.frame(
    Steps = unname(long_results[, "Steps"]),
    `Unique Nodes (difference)` = round(long_results[, "diff.uniq"], 3),
    `Unique Nodes (d)` = round(long_results[, "d.uniq"], 3),
    `Direction (Unique Nodes)` = direction(long_results[, "d.uniq"]),
    `Similarity (difference)` = round(long_results[, "diff.sim"], 3),
    `Similarity (d)` = round(long_results[, "d.sim"], 3),
    `Direction (Similarity)` = direction(long_results[, "d.sim"]),
    `5-step Similarity (difference)` = round(long_results[, "diff.sim5"], 3),
    `5-step Similarity (d)` = round(long_results[, "d.sim5"], 3),
    `Direction (5-step Similarity)` = direction(long_results[, "d.sim5"]),
    check.names = FALSE,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

}

#' @noRd
# Errors ----
# Updated 13.10.2026
random_walk_errors <- function(A, B, name_a, name_b, reps, steps, iter, cores)
{

  # 'A' errors
  A <- adjacency_matrix_error(A, "random_walk", require_symmetric = TRUE)

  # 'B' errors
  B <- adjacency_matrix_error(B, "random_walk", require_symmetric = TRUE)

  # 'name_a' errors
  length_error(name_a, 1, "random_walk")
  typeof_error(name_a, "character", "random_walk")

  # 'name_b' errors
  length_error(name_b, 1, "random_walk")
  typeof_error(name_b, "character", "random_walk")

  # 'reps' errors
  length_error(reps, 1, "random_walk")
  typeof_error(reps, "numeric", "random_walk")
  range_error(reps, c(1, Inf), "random_walk")

  # 'steps' errors
  length_error(steps, 1, "random_walk")
  typeof_error(steps, "numeric", "random_walk")
  range_error(steps, c(1, Inf), "random_walk")

  # Check for steps beyond largest level
  if(steps > reps * 10){

    # Send error
    stop(
      "In `random_walk()`: 'steps' (", steps, ") cannot exceed 'reps * 10' (", reps * 10, "), the largest number of steps tested.",
      call. = FALSE
    )

  }

  # 'iter' errors
  length_error(iter, 1, "random_walk")
  typeof_error(iter, "numeric", "random_walk")
  range_error(iter, c(1, Inf), "random_walk")

  # 'cores' errors
  length_error(cores, 1, "random_walk")
  typeof_error(cores, "numeric", "random_walk")
  range_error(cores, c(1, Inf), "random_walk")

  # Return 'A' and 'B' (in case of data frames)
  return(list(A = A, B = B))

}
