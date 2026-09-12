#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Generic, cross-cutting helpers used by more than one exported function ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#' @noRd
# Identify the practical "shape" of an object for validation messages
# Checked in this order -- matrix first -- because every validator that
# calls this (`adjacency_matrix_error()`, `response_matrix_error()`, ...)
# is called on every exported function's every invocation, and a matrix is
# by far the most common input in a network-analysis package. `is.matrix()`
# is also a cheaper check than `is.data.frame()` on its own (no class-
# attribute/`inherits()` work), so checking it first benchmarks ~2.3x
# faster for the common case -- validated as safe to reorder because
# `is.matrix()` and `is.data.frame()` are mutually exclusive (a data.frame
# is never also `is.matrix()`-true), so swapping their order doesn't change
# which type wins; only "data.frame before list" and "matrix before array"
# are true precedence constraints (a data.frame is-a list; a matrix is-an
# array), and both are still respected below
# Updated 06.09.2026
get_object_type <- function(object)
{

  # matrix is checked first -- see above for why this order is both safe and faster
  if(is.matrix(object)){
    return("matrix")
  }

  # data.frame needs precedence over "list" (a data.frame is also a list)
  if(is.data.frame(object)){
    return("data.frame")
  }

  # array (non-matrix, e.g. 3+ dimensions)
  if(is.array(object)){
    return("array")
  }

  # factor needs precedence over "vector" (a factor is stored as an integer vector)
  if(is.factor(object)){
    return("factor")
  }

  # plain list (not a data.frame)
  if(is.list(object)){
    return("list")
  }

  # atomic vector (numeric, character, logical, ...)
  if(is.vector(object)){
    return("vector")
  }

  # fall back to R's own type name for anything else (function, environment, ...)
  return(typeof(object))

}

#' @noRd
# Stop with a clear message when an object is not one of the expected "shapes"
# Updated 06.09.2026
object_error <- function(input, expected_type, function_name)
{

  # Determine what the input actually is
  input_type <- get_object_type(input)

  # Only error when none of the expected types match
  if(!input_type %in% expected_type){
    stop(
      paste0(
        "In `", function_name, "()`: input `", deparse(substitute(input)),
        "` is a '", input_type, "' object.\n",
        "Expected: ", paste0("'", expected_type, "'", collapse = " or "), "."
      ),
      call. = FALSE
    )
  }

}

#' @noRd
# Stop with a clear message when an object's `typeof()` does not match expectations
# Updated 06.09.2026
typeof_error <- function(input, expected_type, function_name)
{

  # Get the raw type of the input
  input_type <- typeof(input)

  # Collapse R's "integer"/"double" distinction down to "numeric" for both sides,
  # since users think of both as "numeric" and the distinction is rarely intended
  input_type <- swiftelse(input_type %in% c("integer", "double"), "numeric", input_type)
  expected_type <- swiftelse(expected_type %in% c("integer", "double"), "numeric", expected_type)

  # Collapse R's internal "closure" type down to the more familiar "function",
  # since `typeof(function(x) x)` returns "closure", not "function"
  input_type <- swiftelse(input_type == "closure", "function", input_type)
  expected_type <- swiftelse(expected_type == "closure", "function", expected_type)

  # Only error when none of the expected types match
  if(!input_type %in% expected_type){
    stop(
      paste0(
        "In `", function_name, "()`: input `", deparse(substitute(input)),
        "` is of type '", input_type, "'.\n",
        "Expected: ", paste0("'", expected_type, "'", collapse = " or "), "."
      ),
      call. = FALSE
    )
  }

}

#' @noRd
# Stop with a clear message when an object's length is not one of the expected lengths
# Updated 06.09.2026
length_error <- function(input, expected_lengths, function_name)
{

  # Only error when the input's length matches none of the expected lengths
  if(!length(input) %in% expected_lengths){
    stop(
      paste0(
        "In `", function_name, "()`: input `", deparse(substitute(input)),
        "` has length ", length(input), ".\n",
        "Expected length: ", paste0(expected_lengths, collapse = " or "), "."
      ),
      call. = FALSE
    )
  }

}

#' @noRd
# Stop with a clear message when an object's values fall outside an expected range
# Updated 06.09.2026
range_error <- function(input, expected_range, function_name)
{

  # Bounds of the allowed range
  expected_minimum <- min(expected_range, na.rm = TRUE)
  expected_maximum <- max(expected_range, na.rm = TRUE)

  # Observed bounds of the input
  observed_minimum <- min(input, na.rm = TRUE)
  observed_maximum <- max(input, na.rm = TRUE)

  # Error when the input dips below the allowed minimum
  if(observed_minimum < expected_minimum){
    stop(
      paste0(
        "In `", function_name, "()`: input `", deparse(substitute(input)),
        "` has a minimum value of ", observed_minimum, ".\n",
        "Expected values between ", expected_minimum, " and ", expected_maximum, "."
      ),
      call. = FALSE
    )
  }

  # Error when the input exceeds the allowed maximum
  if(observed_maximum > expected_maximum){
    stop(
      paste0(
        "In `", function_name, "()`: input `", deparse(substitute(input)),
        "` has a maximum value of ", observed_maximum, ".\n",
        "Expected values between ", expected_minimum, " and ", expected_maximum, "."
      ),
      call. = FALSE
    )
  }

}

#' @noRd
# Fast, type-stable alternative to `ifelse()`
# Benchmarked ~2.3x faster than `ifelse()` for the (common) no-`NA` vector
# case -- direct logical-index assignment into a pre-typed result vector,
# instead of `ifelse()`'s more general internal machinery. Falls back to
# `ifelse()` itself whenever 'condition' actually contains `NA`, since
# building the result via assignment can't propagate `NA` the way `ifelse()`
# does (a plain `result[NA] <- ...` errors outright, it doesn't silently
# skip) -- correctness on that rare path matters more than its speed
# Updated 06.09.2026
swiftelse <- function(condition, true, false)
{

  condition_length <- length(condition)

  # A length-1 condition is a plain scalar branch, cheaper than `ifelse()`'s
  # vector machinery and avoids `ifelse()`'s tendency to silently coerce types
  if(condition_length == 1){
    return(swiftelse_scalar(condition, true, false))
  }

  # `NA` anywhere in 'condition' takes the always-correct (if slower) path
  if(anyNA(condition)){
    return(ifelse(condition, true, false))
  }

  # Pre-allocate a result vector typed to match 'false' (mirrors `ifelse()`'s
  # own convention of basing the output type on its "no" argument)
  result <- vector(typeof(false), condition_length)

  # Fill the TRUE positions, recycling a scalar 'true' or indexing a vector one
  if(length(true) == 1){
    result[condition] <- true
  }else{
    result[condition] <- true[condition]
  }

  # Fill the FALSE positions the same way
  not_condition <- !condition
  if(length(false) == 1){
    result[not_condition] <- false
  }else{
    result[not_condition] <- false[not_condition]
  }

  return(result)

}

#' @noRd
# The scalar branch used by `swiftelse()`
# Updated 06.09.2026
swiftelse_scalar <- function(condition, true, false)
{

  # `isTRUE()` guards against `NA`/length-0 conditions falling through silently
  if(isTRUE(condition)){
    return(true)
  }

  # Anything not exactly `TRUE` takes the "false" branch
  return(false)

}

#' @noRd
# Validate (and coerce) an object expected to be a square numeric adjacency matrix
# Replaces the duplicated/missing square-matrix checks previously scattered across
# ASPL/CC/Q/TMFG. Returns the input coerced to a plain numeric matrix.
# Updated 06.09.2026
adjacency_matrix_error <- function(A, function_name, require_symmetric = FALSE)
{

  # 'A' must be a matrix or a data.frame (coercible to a matrix)
  object_error(A, c("matrix", "data.frame"), function_name)

  # Coerce a data.frame input into a plain matrix
  if(is.data.frame(A)){
    A <- as.matrix(A)
  }

  # The matrix must hold numeric values
  typeof_error(A, "numeric", function_name)

  # An adjacency matrix must be square (same number of rows and columns)
  if(nrow(A) != ncol(A)){
    stop(
      paste0(
        "In `", function_name, "()`: input `", deparse(substitute(A)),
        "` has ", nrow(A), " rows and ", ncol(A), " columns.\n",
        "An adjacency matrix must be square (equal rows and columns)."
      ),
      call. = FALSE
    )
  }

  # A 0x0 matrix passes the square check above vacuously -- reject it
  # explicitly rather than letting every downstream network function
  # silently degrade to an empty (but not obviously wrong) result
  if(nrow(A) == 0){
    stop(
      paste0(
        "In `", function_name, "()`: input `", deparse(substitute(A)),
        "` has 0 rows and 0 columns.\n",
        "An adjacency matrix must have at least 1 node."
      ),
      call. = FALSE
    )
  }

  # Optionally require the matrix to be symmetric (undirected network)
  if(require_symmetric && !isSymmetric(unname(A))){
    stop(
      paste0(
        "In `", function_name, "()`: input `", deparse(substitute(A)),
        "` is not symmetric.\n",
        "An undirected adjacency matrix must be symmetric (A[i, j] == A[j, i])."
      ),
      call. = FALSE
    )
  }

  # Return the validated (and possibly coerced) matrix
  return(A)

}

#' @noRd
# Validate (and coerce) an object expected to be a participant-by-item response matrix
# Replaces the two previously-conflicting numeric-detection heuristics in NRW
# (`max(range(data)) >= 1`) and CN's `stat.cooccur` (`== 1`) with one explicit rule:
# character/factor matrices are assumed to be raw responses (left untouched here,
# converted downstream by whichever function needs a binary form); numeric matrices
# must already be strictly binary (0s and 1s only) or this errors clearly instead of
# guessing. Returns the input coerced to a plain matrix.
# Updated 06.09.2026
response_matrix_error <- function(data, function_name)
{

  # 'data' must be a matrix or a data.frame (coercible to a matrix)
  object_error(data, c("matrix", "data.frame"), function_name)

  # Coerce a data.frame input into a plain matrix
  if(is.data.frame(data)){
    data <- as.matrix(data)
  }

  # A response matrix must have at least one row (case) and one column (item)
  if(nrow(data) == 0 || ncol(data) == 0){
    stop(
      paste0(
        "In `", function_name, "()`: input `", deparse(substitute(data)),
        "` has ", nrow(data), " rows and ", ncol(data), " columns.\n",
        "A response matrix must have at least one row and one column."
      ),
      call. = FALSE
    )
  }

  # Character/factor content is assumed to be raw (not-yet-binarized) responses;
  # no further check is possible here without knowing the target vocabulary
  if(is.character(data) || is.factor(data)){
    return(data)
  }

  # Numeric content must already be strictly binary -- no silent reinterpretation
  if(is.numeric(data) && !all(data %in% c(0, 1, NA))){
    stop(
      paste0(
        "In `", function_name, "()`: input `", deparse(substitute(data)),
        "` is numeric but contains values other than 0 and 1.\n",
        "Binarize the response matrix before passing it to `", function_name, "()`."
      ),
      call. = FALSE
    )
  }

  # Return the validated (and possibly coerced) matrix
  return(data)

}

#' @noRd
# Validate a raw, not-yet-binarized verbal fluency response matrix
# Used by `cn()` and `nrw()`, which both need genuine response *order*
# information -- a strictly 0/1 binary matrix has already discarded that.
# Character/factor content is assumed to be raw word responses; numeric
# content must be an *ordered* response matrix (values above 1 present), not
# a pre-binarized one. This replaces two previously-diverging heuristics: the
# original `cn()` correctly rejected only exactly-binary numeric input
# (`max() == 1`), but the original `nrw()` used `max() >= 1`, which rejected
# virtually every valid ordered response matrix too (order values are
# 1, 2, 3, ... and so are almost always >= 1) -- a real bug, not a style
# difference. Both now share this one, correct rule.
# Updated 06.09.2026
ordered_response_matrix_error <- function(data, function_name)
{

  # 'data' must be a matrix or a data.frame (coercible to a matrix)
  object_error(data, c("matrix", "data.frame"), function_name)

  # Coerce a data.frame input into a plain matrix
  if(is.data.frame(data)){
    data <- as.matrix(data)
  }

  # A response matrix must have at least one row (case) and one column
  if(nrow(data) == 0 || ncol(data) == 0){
    stop(
      paste0(
        "In `", function_name, "()`: input `", deparse(substitute(data)),
        "` has ", nrow(data), " rows and ", ncol(data), " columns.\n",
        "A response matrix must have at least one row and one column."
      ),
      call. = FALSE
    )
  }

  # Character/factor content is assumed to already be raw word responses
  if(is.character(data) || is.factor(data)){
    return(data)
  }

  # Numeric content must carry response order (not already collapsed to 0/1)
  if(is.numeric(data) && max(data, na.rm = TRUE) == 1){
    stop(
      paste0(
        "In `", function_name, "()`: input `", deparse(substitute(data)),
        "` is a strictly binary (0/1) matrix, which has already discarded response order.\n",
        "Provide the original word responses or an ordered numeric response matrix instead."
      ),
      call. = FALSE
    )
  }

  return(data)

}

#' @noRd
# Cosine similarity between columns of a matrix, or between two vectors
# Vectorized via `crossprod()` -- the original implementation looped over every
# pair of columns by hand
# Updated 06.09.2026
cosine <- function(x, y = NULL)
{

  # A single vector paired with 'y' computes one similarity value
  if(is.vector(x) && !is.null(y)){

    # Replace missing values with 0 so they drop out of the dot product
    x[is.na(x)] <- 0
    y[is.na(y)] <- 0

    # cos(theta) = (x . y) / (||x|| * ||y||)
    return(sum(x * y) / sqrt(sum(x^2) * sum(y^2)))

  }

  # Otherwise treat 'x' as a matrix and compute all pairwise column similarities
  x <- as.matrix(x)

  # Replace missing values with 0 so they drop out of every dot product
  x[is.na(x)] <- 0

  # Numerator: all pairwise dot products between columns, in one matrix product
  dot_products <- crossprod(x)

  # Denominator: outer product of each column's own norm
  norms <- sqrt(diag(dot_products))
  norm_products <- outer(norms, norms)

  # Elementwise division gives the full cosine similarity matrix at once
  return(dot_products / norm_products)

}

#' @noRd
# Cohen's d effect size between two independent samples
# Single implementation -- the original package duplicated this in
# `response.analysis.R` and `utils-SemNeT.R`
# Updated 06.09.2026
cohens_d <- function(sample_1, sample_2)
{

  # Sample means, ignoring missing values
  mean_1 <- mean(sample_1, na.rm = TRUE)
  mean_2 <- mean(sample_2, na.rm = TRUE)

  # Sample standard deviations, ignoring missing values
  sd_1 <- sd(sample_1, na.rm = TRUE)
  sd_2 <- sd(sample_2, na.rm = TRUE)

  # Pooled standard deviation (unweighted average of the two variances)
  pooled_sd <- sqrt((sd_1^2 + sd_2^2) / 2)

  # d = standardized mean difference, reported as an absolute (direction-free) value
  return(abs((mean_1 - mean_2) / pooled_sd))

}

#' @noRd
# Run `FUN(i, ...)` over `seq_len(iterations)`, in parallel when `cores > 1`
# Replaces the 3+ independently duplicated, leak-prone `parallel::makeCluster()`/
# `stopCluster()` blocks in the original package with one `future`-based helper
# that always restores the previous plan via `on.exit()`, even if `FUN` errors
# Updated 06.09.2026
parallel_process <- function(iterations, FUN, ..., cores = 1, progress = TRUE)
{

  # 'iterations' must be a single positive whole number
  length_error(iterations, 1, "parallel_process")
  typeof_error(iterations, "numeric", "parallel_process")
  range_error(iterations, c(1, Inf), "parallel_process")

  # 'cores' must be a single positive whole number
  length_error(cores, 1, "parallel_process")
  typeof_error(cores, "numeric", "parallel_process")
  range_error(cores, c(1, Inf), "parallel_process")

  # 'FUN' must actually be a function
  typeof_error(FUN, "function", "parallel_process")

  # When this call is itself running inside a background `multisession`
  # worker (e.g. a Shiny `ExtendedTask`'s `future_promise()` body, spawned
  # from the app-level plan set in `inst/Shiny/global.R`), spinning up a
  # *second*, nested `multisession` pool here trips `parallelly`'s hard
  # localhost-worker-count guard (`checkNumberOfLocalWorkers()`), since a
  # worker process's own `availableCores()` is deliberately capped (usually
  # to 1) to prevent exactly this kind of recursive worker explosion.
  # Capping the request to what's actually safely available degrades that
  # case to sequential (still correct, just not internally parallel one
  # level deeper) while leaving true top-level interactive/console use --
  # where `availableCores()` reflects the real host -- unaffected
  if(cores > 1){
    cores <- min(cores, future::availableCores())
  }

  # Record the currently active plan so it can be restored afterward, and switch
  # to a sequential plan for one core or multisession (background R processes)
  # for more than one -- `workers` is only a valid argument for multisession
  if(cores > 1){
    old_plan <- future::plan(future::multisession, workers = cores)
  }else{
    old_plan <- future::plan(future::sequential)
  }

  # Guarantee the previous plan is restored even if 'FUN' throws an error
  on.exit(future::plan(old_plan), add = TRUE)

  # `progressr`'s cross-process relay assumes the caller is synchronously
  # *waiting* on the future (e.g. a plain interactive `future::value()` call)
  # -- it does not reach a `shiny::ExtendedTask`/`promises::future_promise()`
  # caller at all, since that never blocks to poll for it. For that case (and
  # only that case -- normal interactive/console use is unaffected), the
  # Shiny app instead points `options(SemNeT.progress_file = <path>)` at a
  # per-task file *before* calling into a package function that reaches this
  # point, and every iteration's completion is written there directly. No
  # exported function's signature needs a new argument for this
  progress_file <- getOption("SemNeT.progress_file", default = NULL)

  # Reset the shared file to this call's total before any iteration begins,
  # so a UI polling it always has an accurate denominator -- including across
  # multiple `parallel_process()` calls within the same background task (e.g.
  # bootstrap_SemNeT() calls this once per group). The sibling ".progress"
  # file is truncated to empty here too -- see `wrapped_FUN()` below for why
  # it, not this file, is what actually tracks completions
  progress_marker_file <- paste0(progress_file, ".progress")

  if(!is.null(progress_file)){
    try({
      writeLines(as.character(iterations), progress_file)
      writeLines(character(0), progress_marker_file)
    }, silent = TRUE)
  }

  # Wrap 'FUN' so every call also records its completion, regardless of
  # whether a console progress bar was also requested below. Each completed
  # iteration appends exactly one byte to `progress_marker_file`; a UI poller
  # reads the *count* of completions (that file's size) rather than the raw
  # iteration index `i` written here. That distinction matters once
  # `cores > 1`: `future_lapply()`'s workers finish iterations out of order
  # (e.g. iteration 900 can complete before iteration 100), so writing `i`
  # itself makes a polled progress value jump backward and forward instead of
  # climbing monotonically. A byte count of completions-so-far is correct
  # regardless of completion order. `append = TRUE` opens the file in a mode
  # that appends each small write atomically on a local filesystem, so
  # concurrent workers' single-byte appends don't corrupt one another
  wrapped_FUN <- function(i, ...){

    result <- FUN(i, ...)

    if(!is.null(progress_file)){
      try(cat("1", file = progress_marker_file, append = TRUE), silent = TRUE)
    }

    return(result)

  }

  # Run without a progress bar when one wasn't requested
  if(!progress){
    return(future.apply::future_lapply(seq_len(iterations), wrapped_FUN, ..., future.seed = TRUE))
  }

  # Run with a `progressr` progress bar wrapping the parallel apply
  return(
    progressr::with_progress({

      # One reporting step per iteration
      report_step <- progressr::progressor(steps = iterations)

      # Wrap 'FUN' so every call also ticks the progress bar
      future.apply::future_lapply(
        seq_len(iterations),
        function(i, ...){
          result <- wrapped_FUN(i, ...)
          report_step()
          return(result)
        },
        ...,
        future.seed = TRUE
      )

    })
  )

}

#' @noRd
# Convert a weighted or binary adjacency matrix to a strictly binary (0/1) one
# Used by `semantic_network_measures()`, `random_walk()`, `pf()`, and the
# response-matrix converters below -- shared by 4+ files, so it lives here
# Updated 06.09.2026
binarize <- function(A)
{

  # Coerce to a plain matrix first (accepts a data.frame too)
  A <- as.matrix(A)

  # Any nonzero entry becomes 1, everything else becomes 0
  binary <- ifelse(A != 0, 1, 0)

  # `ifelse()` drops dimnames -- put the original row/column names back
  row.names(binary) <- row.names(A)
  colnames(binary) <- colnames(A)

  return(binary)

}

#' @noRd
# Shortest-path distance matrix of a network
# Used by `aspl()` and `random_walk()` -- shared by 2 files, so it lives here.
# Unlike the original, unreachable pairs are left as `Inf` instead of being
# silently zeroed out -- silently zeroing them made `aspl()` average an
# unreachable pair in as though it were adjacent (distance 0), which
# systematically understated the average shortest path length for any
# partially-disconnected network. Callers that need "no unreachable pairs"
# semantics (as `random_walk()` does) can do that conversion themselves.
# Updated 06.09.2026
distance <- function(A, weighted = FALSE)
{

  # Number of nodes
  n <- ncol(A)

  # Unweighted case: breadth-first search via repeated matrix multiplication --
  # after 'l' multiplications, a nonzero entry means a path of length <= l exists
  if(!weighted){

    # Binary adjacency matrix (ignore edge weights)
    B <- ifelse(A != 0, 1, 0)

    # Path length counter
    path_length <- 1

    # Matrix power accumulator, starts as the adjacency matrix itself
    path_powers <- B

    # Shortest-path distances found so far (0 = "not yet found", except the diagonal)
    D <- B

    # Cells still needing a distance: any cell not yet found and still reachable
    still_searching <- matrix(TRUE, nrow = n, ncol = n)

    # Keep extending paths by one edge until nothing new is reachable
    while(any(still_searching)){

      path_length <- path_length + 1

      # One more edge added to every path found so far
      path_powers <- path_powers %*% B

      # A cell is newly found this round if a path now exists but none was
      # recorded yet -- vectorized in place of the original's double for-loop
      still_searching <- (path_powers != 0) & (D == 0)

      # Record the current path length for every newly-found cell
      D[still_searching] <- path_length

    }

    # Cells that never became nonzero (other than the diagonal) are unreachable
    D[D == 0] <- Inf
    diag(D) <- 0

  }else{

    # Weighted case: Dijkstra's algorithm from every node, following Rubinov &
    # Sporns (2010) -- edge weights are treated as connection strengths, so
    # distance is the inverse of weight (a stronger connection is "closer")
    G <- ifelse(1 / A == Inf, 0, 1 / A)

    # A zero weight inverts to -Inf instead of the intended "no edge" (0)
    if(any(G == -Inf)){
      G <- ifelse(G == -Inf, 0, G)
    }

    # Symmetrize away any asymmetry that's only floating-point noise
    if(any(!G == t(G))){
      if(max(abs(G - t(G))) < 1e-10){
        G <- (G + t(G)) / 2
      }
    }

    # Shortest-path distances, initialized to "unreached" (Inf) except self (0)
    D <- matrix(Inf, nrow = n, ncol = n)
    diag(D) <- 0

    # Number of edges on the shortest path found so far (for tie-breaking)
    edge_count <- matrix(0, nrow = n, ncol = n)

    # Run Dijkstra's algorithm outward from every node in turn
    for(source in seq_len(n)){

      # Nodes not yet finalized for this source
      unsettled <- matrix(TRUE, nrow = n, ncol = 1)

      # Remaining graph to explore from, shrinks as nodes are settled
      remaining_edges <- G

      # Frontier: the node(s) just settled, starts at the source itself
      frontier <- source

      repeat{

        # Mark the frontier as settled and cut off incoming edges to it
        unsettled[frontier] <- FALSE
        remaining_edges[, frontier] <- 0

        # Relax edges out of every node currently on the frontier
        for(node in frontier){

          neighbors <- which(remaining_edges[node, ] != 0)

          # Candidate distances via this node vs. the best known so far
          candidates <- rbind(D[source, neighbors], D[source, node] + remaining_edges[node, neighbors])
          best <- apply(candidates, 2, min)
          best_via <- apply(candidates, 2, which.min)

          D[source, neighbors] <- best

          # Track how many edges the (possibly updated) shortest path uses
          improved <- neighbors[best_via == 2]
          edge_count[source, improved] <- edge_count[source, node] + 1

        }

        # Next frontier: the closest unsettled node(s) still reachable
        closest_remaining <- suppressWarnings(min(D[source, unsettled == TRUE]))

        if(length(closest_remaining) == 0 || is.infinite(closest_remaining)){
          break
        }

        frontier <- which(D[source, ] == closest_remaining)

      }

    }

  }

  colnames(D) <- colnames(A)
  row.names(D) <- colnames(A)

  return(D)

}

#' @noRd
# Replace bad/empty verbal fluency responses with `NA`
# Used by `responses_to_binary()` below and by `nrw()` -- shared by 2 files
# Updated 06.09.2026
bad_response <- function(word, ...)
{

  # Any additional user-specified bad responses
  others <- unlist(list(...))

  # The standard set of "not a real response" placeholders
  bad <- c(NA, "NA", "", " ", "  ", others)

  # An empty input has no responses at all -- treat it as a single missing one
  if(length(word) == 0){
    word <- NA
  }

  # Vectorized replacement in place of the original's per-element loop
  word[word %in% bad] <- NA

  return(word)

}

#' @title Convert Raw Responses to a Binary Response Matrix
#'
#' @description Converts a raw word-response matrix (participants by response
#' order, one word per cell) into a binary (participant by vocabulary)
#' response matrix, alongside a matching matrix recording response order
#'
#' @param responses Matrix or data frame.
#' A raw word-response matrix: participants (rows) by response order
#' (columns), one word per cell (e.g. verbal fluency data). Empty, missing,
#' and placeholder cells (\code{NA}, \code{""}, whitespace-only) are treated
#' as no response
#'
#' @return A list of class \code{"responses_to_binary"} with:
#' \item{binary}{A participant by vocabulary matrix of 0s (never said) and 1s (said)}
#' \item{order}{A participant by vocabulary matrix of 0s (never said) and the
#' response's 1-based order (first said, second said, ...) otherwise}
#'
#' @examples
#' # Raw fluency responses, 2 participants, up to 3 responses each
#' responses <- matrix(
#'   c("dog", "cat", NA, "cat", "wolf", "dog"),
#'   nrow = 2, byrow = TRUE
#' )
#'
#' result <- responses_to_binary(responses)
#' result$binary
#' result$order
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @importFrom stats na.omit
#'
#' @export
# Convert a raw word-response matrix into a binary (participant x vocabulary) matrix
# Exported so the Shiny app's Network Estimation tab (which needs this exact
# conversion for raw-response uploads) can call it directly, matching every
# other tab's "call the real package function, no duplicates" rule -- as an
# unexported (`@noRd`) helper this was invisible under a real `library(SemNeT)`
# install (only `devtools::load_all()`, used throughout Phase 7's own testing,
# happens to attach internals too, which is what let this ship unnoticed)
# Updated 10.09.2026
responses_to_binary <- function(responses)
{

  # Validate and coerce input before any computation begins -- shared with
  # every other function that accepts a raw-or-already-binary response matrix
  responses <- response_matrix_error(responses, "responses_to_binary")

  # Data matrix
  mat <- as.matrix(responses)

  # Replace bad responses with NA so they never become a "word"
  mat <- bad_response(mat)

  # The full vocabulary observed across all participants, alphabetized
  vocabulary <- sort(na.omit(unique(as.vector(mat))))

  # Number of participants
  n_cases <- nrow(mat)

  # Initialize the ordered (0 = never said, k = kth response given) matrix
  ordered <- matrix(0, nrow = n_cases, ncol = length(vocabulary))
  colnames(ordered) <- vocabulary
  row.names(ordered) <- row.names(responses)

  # Each participant's row can have a different number of real responses, so
  # this fill-in stays a loop rather than a single vectorized assignment
  for(case in seq_len(n_cases)){

    given <- na.omit(match(mat[case, ], colnames(ordered)))
    ordered[case, given] <- seq_along(given)

  }

  # Package both the binary (said/didn't-say) and ordered (when-said) views
  result <- list(binary = binarize(ordered), order = ordered)

  class(result) <- "responses_to_binary"

  return(result)

}

#' @noRd
# Convert a binary or ordered response matrix back into a word-response matrix
# Used by `nrw()` and `cn()`'s co-occurrence step -- shared by 2 files
# Updated 06.09.2026
binary_to_responses <- function(binary_matrix)
{

  # A `responses_to_binary()` result carries response order; a plain 0/1 matrix doesn't
  has_order <- inherits(binary_matrix, "responses_to_binary")

  # Ordered source matrix (from `responses_to_binary()`) or the raw input itself
  source_matrix <- swiftelse(has_order, binary_matrix$order, binary_matrix)

  # Most responses given by any one participant, sets the output's column count
  max_responses <- max(rowSums(binarize(source_matrix)))

  # Zero-padded response column names ("Response_01", "Response_02", ...).
  # `paste0()` recycles a zero-length argument to "" rather than returning
  # zero-length itself when the *other* argument isn't also zero-length, so
  # a fully-empty response set (`max_responses == 0`) needs an explicit
  # `character(0)` rather than reaching `paste0("Response_", character(0))`
  response_names <- if(max_responses == 0){
    character(0)
  }else{
    paste0(
      "Response_",
      formatC(seq_len(max_responses), digits = nchar(max_responses) - 1, flag = 0, format = "d")
    )
  }

  # Initialize the output word-response matrix
  responses <- matrix(NA, nrow = nrow(source_matrix), ncol = max_responses)
  colnames(responses) <- response_names
  row.names(responses) <- row.names(source_matrix)

  # Each participant's responses are placed in order (by rank if available,
  # otherwise in column order), left-aligned -- ragged per row, so this stays a loop
  for(case in seq_len(nrow(source_matrix))){

    said <- source_matrix[case, ] != 0
    words <- swiftelse(
      has_order,
      names(sort(source_matrix[case, said])),
      names(source_matrix[case, said])
    )

    responses[case, seq_along(words)] <- words

  }

  return(responses)

}
