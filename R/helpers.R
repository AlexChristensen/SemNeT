#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Generic, cross-cutting helpers used by more than one exported function ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#' @noRd
# Get object type ----
# Updated 06.09.2026
get_object_type <- function(object)
{

  # Check for matrix
  if(is.matrix(object)){

    # Return matrix
    return("matrix")

  }

  # Check for data frame
  if(is.data.frame(object)){

    # Return data frame
    return("data.frame")

  }

  # Check for array
  if(is.array(object)){

    # Return array
    return("array")

  }

  # Check for factor
  if(is.factor(object)){

    # Return factor
    return("factor")

  }

  # Check for list
  if(is.list(object)){

    # Return list
    return("list")

  }

  # Check for vector
  if(is.vector(object)){

    # Return vector
    return("vector")

  }

  # Return type of anything else
  return(typeof(object))

}

#' @noRd
# Error for object type ----
# Updated 06.09.2026
object_error <- function(input, expected_type, function_name)
{

  # Get type of input
  input_type <- get_object_type(input)

  # Check for expected type
  if(!input_type %in% expected_type){

    # Send error
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
# Error for `typeof` ----
# Updated 06.09.2026
typeof_error <- function(input, expected_type, function_name)
{

  # Get type of input
  input_type <- typeof(input)

  # Convert integer and double to numeric in input
  input_type <- swiftelse(input_type %in% c("integer", "double"), "numeric", input_type)

  # Convert integer and double to numeric in expectation
  expected_type <- swiftelse(expected_type %in% c("integer", "double"), "numeric", expected_type)

  # Convert closure to function in input
  input_type <- swiftelse(input_type == "closure", "function", input_type)

  # Convert closure to function in expectation
  expected_type <- swiftelse(expected_type == "closure", "function", expected_type)

  # Check for expected type
  if(!input_type %in% expected_type){

    # Send error
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
# Error for `length` ----
# Updated 06.09.2026
length_error <- function(input, expected_lengths, function_name)
{

  # Check for expected length
  if(!length(input) %in% expected_lengths){

    # Send error
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
# Error for `range` ----
# Updated 06.09.2026
range_error <- function(input, expected_range, function_name)
{

  # Get expected minimum
  expected_minimum <- min(expected_range, na.rm = TRUE)

  # Get expected maximum
  expected_maximum <- max(expected_range, na.rm = TRUE)

  # Get observed minimum
  observed_minimum <- min(input, na.rm = TRUE)

  # Get observed maximum
  observed_maximum <- max(input, na.rm = TRUE)

  # Check for minimum below range
  if(observed_minimum < expected_minimum){

    # Send error
    stop(
      paste0(
        "In `", function_name, "()`: input `", deparse(substitute(input)),
        "` has a minimum value of ", observed_minimum, ".\n",
        "Expected values between ", expected_minimum, " and ", expected_maximum, "."
      ),
      call. = FALSE
    )

  }

  # Check for maximum above range
  if(observed_maximum > expected_maximum){

    # Send error
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
# Swift `ifelse` ----
# Updated 06.09.2026
swiftelse <- function(condition, true, false)
{

  # Get length of condition
  condition_length <- length(condition)

  # Check for scalar condition
  if(condition_length == 1){

    # Return scalar result
    return(swiftelse_scalar(condition, true, false))

  }

  # Check for missing values
  if(anyNA(condition)){

    # Return `ifelse` result
    return(ifelse(condition, true, false))

  }

  # Initialize result
  result <- vector(typeof(false), condition_length)

  # Check for single value
  if(length(true) == 1){

    # Fill true positions with value
    result[condition] <- true

  }else{

    # Fill true positions with values
    result[condition] <- true[condition]

  }

  # Get false positions
  not_condition <- !condition

  # Check for single value
  if(length(false) == 1){

    # Fill false positions with value
    result[not_condition] <- false

  }else{

    # Fill false positions with values
    result[not_condition] <- false[not_condition]

  }

  # Return result
  return(result)

}

#' @noRd
# Swift scalar `ifelse` ----
# Updated 06.09.2026
swiftelse_scalar <- function(condition, true, false)
{

  # Check for true condition
  if(isTRUE(condition)){

    # Return true value
    return(true)

  }

  # Return false value
  return(false)

}

#' @noRd
# Error for adjacency matrix ----
# Updated 06.09.2026
adjacency_matrix_error <- function(A, function_name, require_symmetric = FALSE)
{

  # Check for matrix or data frame
  object_error(A, c("matrix", "data.frame"), function_name)

  # Check for data frame
  if(is.data.frame(A)){

    # Convert to matrix
    A <- as.matrix(A)

  }

  # Check for numeric
  typeof_error(A, "numeric", function_name)

  # Check for square matrix
  if(nrow(A) != ncol(A)){

    # Send error
    stop(
      paste0(
        "In `", function_name, "()`: input `", deparse(substitute(A)),
        "` has ", nrow(A), " rows and ", ncol(A), " columns.\n",
        "An adjacency matrix must be square (equal rows and columns)."
      ),
      call. = FALSE
    )

  }

  # Check for empty matrix
  if(nrow(A) == 0){

    # Send error
    stop(
      paste0(
        "In `", function_name, "()`: input `", deparse(substitute(A)),
        "` has 0 rows and 0 columns.\n",
        "An adjacency matrix must have at least 1 node."
      ),
      call. = FALSE
    )

  }

  # Check for symmetric matrix
  if(require_symmetric && !isSymmetric(unname(A))){

    # Send error
    stop(
      paste0(
        "In `", function_name, "()`: input `", deparse(substitute(A)),
        "` is not symmetric.\n",
        "An undirected adjacency matrix must be symmetric (A[i, j] == A[j, i])."
      ),
      call. = FALSE
    )

  }

  # Return matrix
  return(A)

}

#' @noRd
# Error for response matrix ----
# Updated 06.09.2026
response_matrix_error <- function(data, function_name)
{

  # Check for matrix or data frame
  object_error(data, c("matrix", "data.frame"), function_name)

  # Check for data frame
  if(is.data.frame(data)){

    # Convert to matrix
    data <- as.matrix(data)

  }

  # Check for empty matrix
  if(nrow(data) == 0 || ncol(data) == 0){

    # Send error
    stop(
      paste0(
        "In `", function_name, "()`: input `", deparse(substitute(data)),
        "` has ", nrow(data), " rows and ", ncol(data), " columns.\n",
        "A response matrix must have at least one row and one column."
      ),
      call. = FALSE
    )

  }

  # Check for raw responses
  if(is.character(data) || is.factor(data)){

    # Return raw responses
    return(data)

  }

  # Check for non-binary values
  if(is.numeric(data) && !all(data %in% c(0, 1, NA))){

    # Send error
    stop(
      paste0(
        "In `", function_name, "()`: input `", deparse(substitute(data)),
        "` is numeric but contains values other than 0 and 1.\n",
        "Binarize the response matrix before passing it to `", function_name, "()`."
      ),
      call. = FALSE
    )

  }

  # Return matrix
  return(data)

}

#' @noRd
# Error for ordered response matrix ----
# Updated 06.09.2026
ordered_response_matrix_error <- function(data, function_name)
{

  # Check for matrix or data frame
  object_error(data, c("matrix", "data.frame"), function_name)

  # Check for data frame
  if(is.data.frame(data)){

    # Convert to matrix
    data <- as.matrix(data)

  }

  # Check for empty matrix
  if(nrow(data) == 0 || ncol(data) == 0){

    # Send error
    stop(
      paste0(
        "In `", function_name, "()`: input `", deparse(substitute(data)),
        "` has ", nrow(data), " rows and ", ncol(data), " columns.\n",
        "A response matrix must have at least one row and one column."
      ),
      call. = FALSE
    )

  }

  # Check for raw responses
  if(is.character(data) || is.factor(data)){

    # Return raw responses
    return(data)

  }

  # Check for binary matrix
  if(is.numeric(data) && max(data, na.rm = TRUE) == 1){

    # Send error
    stop(
      paste0(
        "In `", function_name, "()`: input `", deparse(substitute(data)),
        "` is a strictly binary (0/1) matrix, which has already discarded response order.\n",
        "Provide the original word responses or an ordered numeric response matrix instead."
      ),
      call. = FALSE
    )

  }

  # Return matrix
  return(data)

}

#' @noRd
# Cosine similarity ----
# Updated 06.09.2026
cosine <- function(x, y = NULL)
{

  # Check for two vectors
  if(is.vector(x) && !is.null(y)){

    # Set missing values in first vector to zero
    x[is.na(x)] <- 0

    # Set missing values in second vector to zero
    y[is.na(y)] <- 0

    # Return cosine of vectors
    return(sum(x * y) / sqrt(sum(x^2) * sum(y^2)))

  }

  # Ensure matrix
  x <- as.matrix(x)

  # Set missing values to zero
  x[is.na(x)] <- 0

  # Get dot products of columns
  dot_products <- crossprod(x)

  # Get norms of columns
  norms <- sqrt(diag(dot_products))

  # Get products of norms
  norm_products <- outer(norms, norms)

  # Return cosine similarities
  return(dot_products / norm_products)

}

#' @noRd
# Cohen's d ----
# Updated 13.10.2026
cohens_d <- function(sample_1, sample_2)
{

  # Get mean of first sample
  mean_1 <- mean(sample_1, na.rm = TRUE)

  # Get mean of second sample
  mean_2 <- mean(sample_2, na.rm = TRUE)

  # Get standard deviation of first sample
  sd_1 <- sd(sample_1, na.rm = TRUE)

  # Get standard deviation of second sample
  sd_2 <- sd(sample_2, na.rm = TRUE)

  # Get size of first sample
  n_1 <- sum(!is.na(sample_1))

  # Get size of second sample
  n_2 <- sum(!is.na(sample_2))

  # Get degrees of freedom
  degrees_of_freedom <- n_1 + n_2 - 2

  # Check for too few observations
  if(n_1 < 1 || n_2 < 1 || degrees_of_freedom < 1){

    # Return undefined effect
    return(NA_real_)

  }

  # Get sum of squares
  sum_of_squares <- (if(n_1 > 1) (n_1 - 1) * sd_1^2 else 0) + (if(n_2 > 1) (n_2 - 1) * sd_2^2 else 0)

  # Get pooled standard deviation
  pooled_sd <- sqrt(sum_of_squares / degrees_of_freedom)

  # Check for no variation
  if(pooled_sd == 0){

    # Return zero or infinite effect
    return(if(mean_1 == mean_2) 0 else Inf)

  }

  # Return absolute effect
  return(abs((mean_1 - mean_2) / pooled_sd))

}

#' @noRd
# Network counts ----
# Updated 10.10.2026
network_counts <- function(network)
{

  # Return number of nodes and edges
  return(c(
    nodes = nrow(network),
    edges = (sum(network != 0) - sum(diag(network) != 0)) / 2
  ))

}

#' @noRd
# Legacy inference warning ----
# Updated 10.10.2026
legacy_inference_warning <- function(function_name, replacement)
{

  # Send warning
  warning(
    paste0(
      "`", function_name, "()`: parametric tests (t-test, ANOVA, ANCOVA) applied to bootstrap replicates are statistically invalid.\n",
      "They treat the `iter` bootstrap samples as independent observations (the degrees of freedom scale with `iter`) and ",
      "divide the bootstrap standard deviation -- which is already the standard error -- by sqrt(`iter`), so test statistics ",
      "are inflated by roughly sqrt(`iter`) and become 'significant' simply by increasing `iter`.\n",
      "Do not use these results for inference. ", replacement, "\n",
      "See Tu, Chodorow, Brooks, Ezrina, & Valian (under review), 'Inflated Statistics Lead to Incorrect Inference: ",
      "How (Not) to Make Cognitive Network Group Comparisons'."
    ),
    call. = FALSE
  )

}

#' @noRd
# Evaluate with local seed ----
# Updated 13.10.2026
with_local_seed <- function(seed, expr)
{

  # Check for no seed
  if(is.null(seed)){

    # Return expression
    return(expr)

  }

  # Check for existing seed
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)

  # Check for existing seed
  if(had_seed){

    # Store existing seed
    old_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)

  }

  # Restore seed on exit
  on.exit({

    # Check for existing seed
    if(had_seed){

      # Restore seed
      assign(".Random.seed", old_seed, envir = globalenv())

    }else if(exists(".Random.seed", envir = globalenv(), inherits = FALSE)){

      # Remove seed
      rm(".Random.seed", envir = globalenv())

    }

  }, add = TRUE)

  # Set seed
  set.seed(
    as.integer(floor(seed) %% .Machine$integer.max),
    kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection"
  )

  # Return expression
  return(expr)

}

#' @noRd
# Seed for compiled code ----
# Updated 13.10.2026
compiled_seed <- function(seed = NULL)
{

  # Check for no seed
  if(is.null(seed)){

    # Return clock seed
    return(0)

  }

  # Return seed
  return(as.double(seed))

}

#' @noRd
# Check for random seed ----
# Updated 13.10.2026
random_seed <- function(seed)
{

  # Return whether seed is none or zero
  return(is.null(seed) || isTRUE(seed == 0))

}

#' @noRd
# Comparison notice ----
# Updated 13.10.2026
comparison_notice <- function(self = c("bootstrap_test_SemNeT", "permutation_SemNeT"))
{

  # Set function
  self <- match.arg(self)

  # Get other function
  other <- if(self == "bootstrap_test_SemNeT") "permutation_SemNeT" else "bootstrap_test_SemNeT"

  # Return notice
  return(
    paste0(
      "`bootstrap_test_SemNeT()` and `permutation_SemNeT()` should always be run together. ",
      "Run `", other, "()` as well and report both."
    )
  )

}

#' @noRd
# Reproducible seeds ----
# Updated 11.10.2026
reproducible_seeds <- function(n, seed = NULL)
{

  # Return seeds from compiled code
  return(
    .Call(
      "r_xoshiro_seeds",
      as.integer(n), compiled_seed(seed),
      PACKAGE = "SemNeT"
    )
  )

}

#' @noRd
# Shuffle ----
# Updated 11.10.2026
shuffle <- function(x, size = length(x), seed = NULL)
{

  # Return shuffled elements from compiled code
  return(
    x[.Call(
      "r_xoshiro_shuffle",
      as.integer(seq_along(x)),
      compiled_seed(seed),
      PACKAGE = "SemNeT"
    )][seq_len(size)]
  )

}

#' @noRd
# Shuffle with replacement ----
# Updated 11.10.2026
shuffle_replace <- function(x, size = length(x), seed = NULL)
{

  # Return resampled elements from compiled code
  return(
    x[.Call(
      "r_xoshiro_shuffle_replace",
      as.integer(seq_along(x)), compiled_seed(seed),
      PACKAGE = "SemNeT"
    )][seq_len(size)]
  )

}

#' @noRd
# Parallel process ----
# Updated 06.09.2026
parallel_process <- function(iterations, FUN, ..., cores = 1, progress = TRUE)
{

  # 'iterations' errors
  length_error(iterations, 1, "parallel_process")
  typeof_error(iterations, "numeric", "parallel_process")
  range_error(iterations, c(1, Inf), "parallel_process")

  # 'cores' errors
  length_error(cores, 1, "parallel_process")
  typeof_error(cores, "numeric", "parallel_process")
  range_error(cores, c(1, Inf), "parallel_process")

  # 'FUN' errors
  typeof_error(FUN, "function", "parallel_process")

  # Check for multiple cores
  if(cores > 1){

    # Cap cores at those available
    cores <- min(cores, future::availableCores())

  }

  # Check for multiple cores
  if(cores > 1){

    # Set parallel plan and store old plan
    old_plan <- future::plan(future::multisession, workers = cores)

  }else{

    # Set sequential plan and store old plan
    old_plan <- future::plan(future::sequential)

  }

  # Restore old plan on exit
  on.exit(future::plan(old_plan), add = TRUE)

  # Get progress file of Shiny app
  progress_file <- getOption("SemNeT.progress_file", default = NULL)

  # Get completion file of Shiny app
  progress_marker_file <- paste0(progress_file, ".progress")

  # Check for progress file
  if(!is.null(progress_file)){

    # Reset progress files
    try({

      # Write total iterations
      writeLines(as.character(iterations), progress_file)

      # Empty completion file
      writeLines(character(0), progress_marker_file)

    }, silent = TRUE)

  }

  # Wrap function to record completion
  wrapped_FUN <- function(i, ...){

    # Run function
    result <- FUN(i, ...)

    # Check for progress file
    if(!is.null(progress_file)){

      # Record completion
      try(cat("1", file = progress_marker_file, append = TRUE), silent = TRUE)

    }

    # Return result
    return(result)

  }

  # Check for no progress bar
  if(!progress){

    # Return results without progress bar
    return(future.apply::future_lapply(seq_len(iterations), wrapped_FUN, ..., future.seed = TRUE))

  }

  # Return results with progress bar
  return(
    progressr::with_progress({

      # Set up progress bar
      report_step <- progressr::progressor(steps = iterations)

      # Run iterations
      future.apply::future_lapply(
        seq_len(iterations),
        function(i, ...){

          # Run wrapped function
          result <- wrapped_FUN(i, ...)

          # Advance progress bar
          report_step()

          # Return result
          return(result)

        },
        ...,
        future.seed = TRUE
      )

    })
  )

}

#' @noRd
# Binarize ----
# Updated 06.09.2026
binarize <- function(A)
{

  # Ensure matrix
  A <- as.matrix(A)

  # Set nonzero values to one
  binary <- ifelse(A != 0, 1, 0)

  # Restore row names
  row.names(binary) <- row.names(A)

  # Restore column names
  colnames(binary) <- colnames(A)

  # Return binary matrix
  return(binary)

}

#' @noRd
# Shortest path distances ----
# Updated 06.09.2026
distance <- function(A, weighted = FALSE)
{

  # Get number of nodes
  n <- ncol(A)

  # Check for unweighted
  if(!weighted){

    # Binarize network
    B <- ifelse(A != 0, 1, 0)

    # Start path length at one
    path_length <- 1

    # Start path powers at network
    path_powers <- B

    # Start distances at network
    D <- B

    # Initialize cells still searching
    still_searching <- matrix(TRUE, nrow = n, ncol = n)

    # Loop until no new paths
    while(any(still_searching)){

      # Increase path length
      path_length <- path_length + 1

      # Extend paths by one edge
      path_powers <- path_powers %*% B

      # Identify newly found paths
      still_searching <- (path_powers != 0) & (D == 0)

      # Record path length of new paths
      D[still_searching] <- path_length

    }

    # Set unreachable pairs to infinite
    D[D == 0] <- Inf

    # Set self-distances to zero
    diag(D) <- 0

  }else{

    # Convert weights to lengths
    G <- ifelse(1 / A == Inf, 0, 1 / A)

    # Check for negative infinite lengths
    if(any(G == -Inf)){

      # Set to zero
      G <- ifelse(G == -Inf, 0, G)

    }

    # Check for asymmetry
    if(any(!G == t(G))){

      # Check for asymmetry from rounding
      if(max(abs(G - t(G))) < 1e-10){

        # Symmetrize network
        G <- (G + t(G)) / 2

      }

    }

    # Initialize distances as unreached
    D <- matrix(Inf, nrow = n, ncol = n)

    # Set self-distances to zero
    diag(D) <- 0

    # Initialize edge counts
    edge_count <- matrix(0, nrow = n, ncol = n)

    # Loop over source nodes
    for(source in seq_len(n)){

      # Initialize unsettled nodes
      unsettled <- matrix(TRUE, nrow = n, ncol = 1)

      # Initialize remaining edges
      remaining_edges <- G

      # Start at source
      frontier <- source

      # Repeat until nodes are settled
      repeat{

        # Settle frontier
        unsettled[frontier] <- FALSE

        # Remove edges into frontier
        remaining_edges[, frontier] <- 0

        # Loop over frontier nodes
        for(node in frontier){

          # Get neighbors of node
          neighbors <- which(remaining_edges[node, ] != 0)

          # Get candidate distances
          candidates <- rbind(D[source, neighbors], D[source, node] + remaining_edges[node, neighbors])

          # Get best distances
          best <- apply(candidates, 2, min)

          # Get best paths
          best_via <- apply(candidates, 2, which.min)

          # Update distances
          D[source, neighbors] <- best

          # Get improved neighbors
          improved <- neighbors[best_via == 2]

          # Update edge counts
          edge_count[source, improved] <- edge_count[source, node] + 1

        }

        # Get closest unsettled distance
        closest_remaining <- suppressWarnings(min(D[source, unsettled == TRUE]))

        # Check for no reachable nodes
        if(length(closest_remaining) == 0 || is.infinite(closest_remaining)){

          # Stop repeating
          break

        }

        # Get next frontier
        frontier <- which(D[source, ] == closest_remaining)

      }

    }

  }

  # Set column names
  colnames(D) <- colnames(A)

  # Set row names
  row.names(D) <- colnames(A)

  # Return distances
  return(D)

}

#' @noRd
# Bad responses ----
# Updated 06.09.2026
bad_response <- function(word, ...)
{

  # Get additional bad responses
  others <- unlist(list(...))

  # Set bad responses
  bad <- c(NA, "NA", "", " ", "  ", others)

  # Check for no responses
  if(length(word) == 0){

    # Set missing response
    word <- NA

  }

  # Set bad responses to missing
  word[word %in% bad] <- NA

  # Return responses
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
#' @return A list of class \code{"responses2binary"} with:
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
#' result <- responses2binary(responses)
#' result$binary
#' result$order
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Responses to binary ----
# Updated 10.09.2026
responses2binary <- function(responses)
{

  # Argument errors
  responses <- responses2binary_errors(responses)

  # Ensure matrix
  mat <- as.matrix(responses)

  # Replace bad responses with missing
  mat <- bad_response(mat)

  # Get vocabulary
  vocabulary <- sort(na.omit(unique(as.vector(mat))))

  # Get number of participants
  n_cases <- nrow(mat)

  # Initialize ordered matrix
  ordered <- matrix(0, nrow = n_cases, ncol = length(vocabulary))

  # Set column names
  colnames(ordered) <- vocabulary

  # Set row names
  row.names(ordered) <- row.names(responses)

  # Loop over participants
  for(case in seq_len(n_cases)){

    # Get positions of responses in vocabulary
    given <- na.omit(match(mat[case, ], colnames(ordered)))

    # Record order of responses
    ordered[case, given] <- seq_along(given)

  }

  # Set up result
  result <- list(binary = binarize(ordered), order = ordered)

  # Set class
  class(result) <- "responses2binary"

  # Return result
  return(result)

}

#' @noRd
# Errors ----
# Updated 13.10.2026
responses2binary_errors <- function(responses)
{

  # 'responses' errors
  responses <- response_matrix_error(responses, "responses2binary")

  # Return 'responses' (in case of data frame)
  return(responses)

}

#' @title Convert a Binary Response Matrix to Raw Responses
#'
#' @description Converts a binary (participant by vocabulary) response matrix,
#' or the result of \code{\link[SemNeT]{responses2binary}}, back into a raw
#' word-response matrix (participants by response order, one word per cell)
#'
#' @param binary_matrix Matrix, data frame, or \code{"responses2binary"} object.
#' A participant by vocabulary matrix of 0s (never said) and 1s (said), with
#' words as column names. When a \code{\link[SemNeT]{responses2binary}} result
#' is provided, the stored response order is used to place each participant's
#' words in the order they were said. A numeric matrix with values above 1 is
#' read the same way (each value is the rank of the response, as in
#' \code{responses2binary(...)$order}); a strictly 0/1 matrix has no order, so
#' its words are placed in column order
#'
#' @return A matrix of words with participants as rows and response order as
#' columns (\code{Response_01}, \code{Response_02}, ...). Rows are left-aligned
#' and padded with \code{NA}
#'
#' @examples
#' # Raw fluency responses, 2 participants, up to 3 responses each
#' responses <- matrix(
#'   c("dog", "cat", NA, "cat", "wolf", "dog"),
#'   nrow = 2, byrow = TRUE
#' )
#'
#' # Round trip: raw -> binary -> raw
#' binary2responses(responses2binary(responses))
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Binary to responses ----
# Updated 13.10.2026
binary2responses <- function(binary_matrix)
{

  # Argument errors
  binary_matrix <- binary2responses_errors(binary_matrix)

  # Check for response order
  has_order <- inherits(binary_matrix, "responses2binary")

  # Get source matrix
  source_matrix <- swiftelse(has_order, binary_matrix$order, binary_matrix)

  # Check for ranks in matrix
  has_order <- has_order || any(source_matrix > 1, na.rm = TRUE)

  # Get largest number of responses
  max_responses <- max(rowSums(binarize(source_matrix)))

  # Set response names
  response_names <- if(max_responses == 0){

    # Set no names
    character(0)

  }else{

    # Set zero-padded names
    paste0(
      "Response_",
      formatC(seq_len(max_responses), digits = nchar(max_responses) - 1, flag = 0, format = "d")
    )

  }

  # Initialize responses
  responses <- matrix(NA, nrow = nrow(source_matrix), ncol = max_responses)

  # Set column names
  colnames(responses) <- response_names

  # Set row names
  row.names(responses) <- row.names(source_matrix)

  # Loop over participants
  for(case in seq_len(nrow(source_matrix))){

    # Identify responses given
    said <- source_matrix[case, ] != 0

    # Get words in order
    words <- swiftelse(
      has_order,
      names(sort(source_matrix[case, said])),
      names(source_matrix[case, said])
    )

    # Fill responses of participant
    responses[case, seq_along(words)] <- words

  }

  # Return responses
  return(responses)

}

#' @noRd
# Errors ----
# Updated 13.10.2026
binary2responses_errors <- function(binary_matrix)
{

  # 'binary_matrix' errors
  if(!inherits(binary_matrix, "responses2binary")){

    # Check for matrix or data frame
    object_error(binary_matrix, c("matrix", "data.frame"), "binary2responses")

    # Check for data frame
    if(is.data.frame(binary_matrix)){

      # Convert to matrix
      binary_matrix <- as.matrix(binary_matrix)

    }

    # Check for numeric
    typeof_error(binary_matrix, "numeric", "binary2responses")

    # Check for column names
    if(is.null(colnames(binary_matrix))){

      # Send error
      stop(
        "In `binary2responses()`: 'binary_matrix' has no column names.\n",
        "The words (responses) must be the columns' names.",
        call. = FALSE
      )

    }

  }

  # Return 'binary_matrix' (in case of data frame)
  return(binary_matrix)

}
