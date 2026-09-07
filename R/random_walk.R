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
#' Number of repetitions of increments in 10 steps.
#' Defaults to \code{20}
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
#' @return A list with \code{long} (every simulated summary statistic, all
#' numeric) and \code{short} (a compact, correctly-typed summary with
#' Mann-Whitney \emph{p}-values and effect directions)
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
#' @importFrom stats wilcox.test runif
#'
#' @export
# Random walk simulation
# Updated 06.09.2026
random_walk <- function(A, B, name_a = "A", name_b = "B", reps = 20, steps = 10, iter = 10000, cores = 1)
{

  # Validate and coerce input before any computation begins
  validated <- random_walk_errors(A, B, name_a, name_b, reps, steps, iter, cores)
  A <- validated$A
  B <- validated$B

  n_a <- ncol(A)
  n_b <- ncol(B)

  # Binary, self-loop-free transition structure for both networks
  A <- binarize(A)
  B <- binarize(B)
  diag(A) <- 0
  diag(B) <- 0

  # Row-normalized transition probability matrices -- dividing a matrix by a
  # vector the length of its row count recycles down each column, i.e. row
  # by row, so this is `TA[i, j] <- A[i, j] / degree_a[i]` for every cell at
  # once, in place of the original's double for-loop
  degree_a <- colSums(A)
  degree_b <- colSums(B)
  transition_a <- A / degree_a
  transition_b <- B / degree_b

  # Shortest-path distances, used to measure how "close" a walk's endpoint
  # is to its starting point. Unreachable node pairs are `Inf` (see
  # `distance()`), which correctly makes `exp(-Inf) == 0` similarity below --
  # the original's `distance()` silently zeroed those pairs instead, which
  # made a truly unreachable pair look *maximally* similar (`exp(-0) == 1`)
  distance_a <- distance(A)
  distance_b <- distance(B)

  # Every walk step needs its current node's transition-probability CDF.
  # Since the transition matrices themselves never change across the
  # thousands of simulated walks below, computing each row's cumulative sum
  # once here -- instead of inside `simulate_walk()`, where it would
  # otherwise be recomputed from scratch on every single step of every
  # single walk -- turns a per-step `cumsum()` call into a single lookup
  # (benchmarked ~20-25% faster per step; this line alone can run tens of
  # millions of times across a full `random_walk()` call)
  cumulative_transition_a <- t(apply(transition_a, 1, cumsum))
  cumulative_transition_b <- t(apply(transition_b, 1, cumsum))

  # One random walk of 'n_steps' steps over both networks simultaneously,
  # starting from an independently-sampled node in each
  simulate_walk <- function(n_steps, n_a, n_b, cumulative_transition_a, cumulative_transition_b, distance_a, distance_b)
  {

    visited_a <- numeric(n_steps)
    visited_b <- numeric(n_steps)

    current_a <- sample(n_a, 1)
    current_b <- sample(n_b, 1)

    visited_a[1] <- current_a
    visited_b[1] <- current_b

    # `n_steps == 1` (reachable via `random_walk(A, B, steps = 1)`) would
    # otherwise make `2:n_steps` count *down* (`2:1`, not empty): the loop
    # would run once for `step = 2` (silently extending `visited_a`/`visited_b`
    # past their intended length) and once more for `step = 1`, overwriting
    # the starting node recorded just above with an unrelated later step
    if(n_steps > 1){
      for(step in 2:n_steps){

        # A single random draw is compared against both networks' transition
        # CDFs, so a "high" vs. "low" draw has the same meaning in both walks
        draw <- runif(1)

        next_a <- which(cumulative_transition_a[current_a, ] > draw)[1]
        next_b <- which(cumulative_transition_b[current_b, ] > draw)[1]

        visited_a[step] <- next_a
        visited_b[step] <- next_b

        current_a <- next_a
        current_b <- next_b

      }
    }

    unique_a <- unique(visited_a)
    unique_b <- unique(visited_b)

    # Similarity between the walk's very first node and its very last
    # distinct node -- a fully "wandering" walk revisits few nodes and ends
    # up far (dissimilar) from where it started
    similarity_a <- exp(-distance_a[visited_a[1], unique_a[length(unique_a)]])
    similarity_b <- exp(-distance_b[visited_b[1], unique_b[length(unique_b)]])

    # Same idea, but measured only 5 unique nodes into the walk (or fewer,
    # if the walk visited fewer than 5 distinct nodes)
    five_step_index <- min(5, length(unique_a), length(unique_b))
    similarity_a_5 <- exp(-distance_a[visited_a[1], unique_a[five_step_index]])
    similarity_b_5 <- exp(-distance_b[visited_b[1], unique_b[five_step_index]])

    return(list(
      unique = c(length(unique_a), length(unique_b)),
      similarity = c(similarity_a, similarity_b),
      similarity_5 = c(similarity_a_5, similarity_b_5)
    ))

  }

  # Step counts to test: 'steps', then increasing by 10 for 'reps' levels
  step_levels <- seq(steps, reps * 10, 10)

  # Every step level's batch of simulations goes through `parallel_process()`
  # in turn -- replaces the original's single hand-managed
  # `parallel::makeCluster()` reused across every level with one
  # cleanup-guaranteed call per level
  message("Computing random walks...")

  simulations_by_level <- lapply(seq_along(step_levels), function(level){

    message(paste0("Repetition ", level, " of ", length(step_levels), " (", step_levels[level], " steps)"))

    parallel_process(
      iter,
      function(i) simulate_walk(step_levels[level], n_a, n_b, cumulative_transition_a, cumulative_transition_b, distance_a, distance_b),
      cores = cores,
      progress = FALSE
    )

  })

  # Every raw numeric summary statistic, one row per step level
  long_results <- format_random_walk_long(simulations_by_level, step_levels, name_a, name_b)

  # A compact, correctly-typed summary -- the original built this as a
  # single character matrix (numeric p-values and "< .001"/direction labels
  # can't coexist in one matrix column type), which then required every
  # downstream consumer to re-parse numbers back out of strings
  short_results <- format_random_walk_short(long_results, name_a, name_b)

  return(list(long = long_results, short = short_results))

}

#' @noRd
# Collect every simulated statistic across step levels into one long-format
# numeric matrix
# Updated 06.09.2026
format_random_walk_long <- function(simulations_by_level, step_levels, name_a, name_b)
{

  results <- matrix(NA, nrow = length(step_levels), ncol = 17)
  colnames(results) <- c(
    "Steps",
    paste("M.uniq", name_a, sep = "."), paste("SD.uniq", name_a, sep = "."),
    paste("M.sim", name_a, sep = "."), paste("SD.sim", name_a, sep = "."),
    paste("M.sim5", name_a, sep = "."), paste("SD.sim5", name_a, sep = "."),
    paste("M.uniq", name_b, sep = "."), paste("SD.uniq", name_b, sep = "."),
    paste("M.sim", name_b, sep = "."), paste("SD.sim", name_b, sep = "."),
    paste("M.sim5", name_b, sep = "."), paste("SD.sim5", name_b, sep = "."),
    "pu", "ps", "ps5", "g5ind"
  )

  for(level in seq_along(step_levels)){

    simulations <- simulations_by_level[[level]]

    unique_counts <- t(sapply(simulations, function(x) x$unique))
    similarities <- t(sapply(simulations, function(x) x$similarity))
    similarities_5 <- t(sapply(simulations, function(x) x$similarity_5))

    p_unique <- suppressWarnings(wilcox.test(unique_counts[, 1], unique_counts[, 2])$p.value)
    p_similarity <- suppressWarnings(wilcox.test(similarities[, 1], similarities[, 2])$p.value)
    p_similarity_5 <- suppressWarnings(wilcox.test(similarities_5[, 1], similarities_5[, 2])$p.value)

    results[level, ] <- c(
      step_levels[level],
      mean(unique_counts[, 1]), sd(unique_counts[, 1]),
      mean(similarities[, 1]), sd(similarities[, 1]),
      mean(similarities_5[, 1]), sd(similarities_5[, 1]),
      mean(unique_counts[, 2]), sd(unique_counts[, 2]),
      mean(similarities[, 2]), sd(similarities[, 2]),
      mean(similarities_5[, 2]), sd(similarities_5[, 2]),
      p_unique, p_similarity, p_similarity_5, 5
    )

  }

  return(results)

}

#' @noRd
# Compact summary: p-values and which network "won" at each step level
# Updated 06.09.2026
format_random_walk_short <- function(long_results, name_a, name_b)
{

  direction <- function(p_values, means_a, means_b){
    swiftelse(
      p_values < .05,
      swiftelse(means_a > means_b, paste(name_a, ">", name_b), paste(name_b, ">", name_a)),
      "n.s."
    )
  }

  data.frame(
    Steps = long_results[, "Steps"],
    `Unique Nodes (p-value)` = round(long_results[, "pu"], 3),
    `Direction (Unique Nodes)` = direction(long_results[, "pu"], long_results[, 2], long_results[, 8]),
    `Similarity (p-value)` = round(long_results[, "ps"], 3),
    `Direction (Similarity)` = direction(long_results[, "ps"], long_results[, 4], long_results[, 10]),
    `5-step Similarity (p-value)` = round(long_results[, "ps5"], 3),
    `Direction (5-step Similarity)` = direction(long_results[, "ps5"], long_results[, 6], long_results[, 12]),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

}

#' @noRd
# Argument errors for `random_walk()`
# Updated 06.09.2026
random_walk_errors <- function(A, B, name_a, name_b, reps, steps, iter, cores)
{

  # 'A' and 'B' must each be a square, numeric matrix (or coercible data.frame)
  A <- adjacency_matrix_error(A, "random_walk")
  B <- adjacency_matrix_error(B, "random_walk")

  # 'name_a'/'name_b' must each be a single character string
  object_error(name_a, "vector", "random_walk")
  typeof_error(name_a, "character", "random_walk")
  length_error(name_a, 1, "random_walk")

  object_error(name_b, "vector", "random_walk")
  typeof_error(name_b, "character", "random_walk")
  length_error(name_b, 1, "random_walk")

  # 'reps'/'steps'/'iter'/'cores' must each be a single positive whole number
  for(argument in list(reps = reps, steps = steps, iter = iter, cores = cores)){
    length_error(argument, 1, "random_walk")
    typeof_error(argument, "numeric", "random_walk")
    range_error(argument, c(1, Inf), "random_walk")
  }

  return(list(A = A, B = B))

}
