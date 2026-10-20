#' @title Community Network Estimation
#'
#' @description Estimates a semantic network using the Community Network
#' method described in Goni et al. (2011)
#'
#' @param data Matrix or data frame.
#' A preprocessed verbal fluency matrix where rows are participants and
#' columns are the order of their verbal fluency responses. Content may be
#' the raw word responses or an ordered numeric response matrix (see
#' \code{responses2binary()})
#'
#' @param window Numeric.
#' Size of window to look for co-occurrences in (a whole number; Goni et al.
#' (2011) require it to be smaller than the average number of responses per
#' participant).
#' Defaults to \code{2}
#'
#' @param alpha Numeric.
#' Significance level of the test of each pair of responses' co-occurrence
#' against chance, following Goni et al. (2011): the pair's co-occurrence rate
#' gets a two-sided \code{1 - alpha} Clopper-Pearson confidence interval, and an
#' edge is kept when the rate expected by chance falls below the interval's
#' lower bound (so each tail holds \code{alpha / 2}). Must be strictly between
#' \code{0} and \code{1}. The tests are \strong{not} adjusted for the number of
#' pairs tested.
#' Defaults to \code{.05}
#'
#' @param enrich Boolean.
#' Should the network be enriched by connecting all nodes within their
#' respective modules?
#' Defaults to \code{FALSE}
#'
#' @param cutoff Numeric.
#' Height at which the hierarchical clustering of the generalized topological
#' overlap dissimilarity is cut into modules when \code{enrich = TRUE}.
#' Goni et al. (2011) chose their cutoff (0.58 in their data) so that it
#' separated the blocks of the overlap matrix as well as possible, so there is
#' \strong{no established value}: it depends on the data and should be
#' inspected rather than assumed. A warning is given whenever
#' \code{enrich = TRUE}.
#' Defaults to \code{0.50}
#'
#' @return Returns an undirected semantic network. As in Goni et al. (2011), only
#' the largest connected component is kept: responses without a significant
#' co-occurrence with the others (typically many of the less frequent responses)
#' are dropped, so the network has fewer nodes than there are distinct
#' responses. A 0 x 0 network (with a warning) is returned when no pair of
#' responses co-occurs significantly
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
#' \dontrun{
#' # Compute networks
#' low_net <- cn(low)
#' high_net <- cn(high)
#' }
#'
#' @references
#' Goni, J., Arrondo, G., Sepulcre, J., Martincorena, I., de Mendizabal, N. V., Corominas-Murtra, B., ... & Villoslada, P. (2011).
#' The semantic organization of the animal category: Evidence from semantic verbal fluency and network theory.
#' \emph{Cognitive Processing}, \emph{12}, 183-196.
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Community network ----
# Updated 15.10.2026
cn <- function(data, window = 2, alpha = .05, enrich = FALSE, cutoff = 0.50)
{

  # Argument errors
  data <- cn_errors(data, window, alpha, enrich, cutoff)

  # Get significant co-occurrence network
  adjacency <- stat_cooccur(data, window = window, alpha = alpha)

  # Get connected components (of the full network, so memberships index its nodes)
  components <- igraph::components(
    igraph::graph_from_adjacency_matrix(
      adjacency != 0, mode = "undirected", diag = FALSE
    )
  )

  # Check for no significant co-occurrences
  if(!any(adjacency != 0)){

    # Send warning
    warning(
      "In `cn()`: no pair of responses co-occurred significantly more than expected by chance, so the network is empty. ",
      "Consider a larger `alpha` or a larger `window`.",
      call. = FALSE
    )

    # Return empty network
    return(adjacency[0, 0, drop = FALSE])

  }

  # Get nodes of largest component
  largest_component <- which(components$membership == which.max(components$csize))

  # Keep largest component
  adjacency <- adjacency[largest_component, largest_component, drop = FALSE]

  # Check for enrichment
  if(enrich){

    # Send warning
    warning(
      "In `cn()`: the module `cutoff` (", cutoff, ") used for `enrich = TRUE` has no established value. ",
      "Goni et al. (2011) chose theirs (0.58) to separate the blocks of their own overlap matrix, so it is data-dependent ",
      "and should be checked for your data.",
      call. = FALSE
    )

    # Connect nodes within modules
    adjacency <- enrich_network(adjacency, gtom(adjacency), cutoff = cutoff)

  }

  # Return network
  return(adjacency)

}

#' @noRd
# Errors ----
# Updated 13.10.2026
cn_errors <- function(data, window, alpha, enrich, cutoff)
{

  # 'data' errors
  data <- ordered_response_matrix_error(data, "cn")

  # 'window' errors
  length_error(window, 1, "cn")
  typeof_error(window, "numeric", "cn")
  range_error(window, c(1, Inf), "cn")

  # Check for whole number
  if(window != round(window)){

    # Send error
    stop("In `cn()`: 'window' must be a whole number.", call. = FALSE)

  }

  # 'alpha' errors
  length_error(alpha, 1, "cn")
  typeof_error(alpha, "numeric", "cn")
  range_error(alpha, c(0, 1), "cn")

  # Check for limits of alpha
  if(alpha <= 0 || alpha >= 1){

    # Send error
    stop("In `cn()`: 'alpha' must be strictly between 0 and 1.", call. = FALSE)

  }

  # 'enrich' errors
  length_error(enrich, 1, "cn")
  typeof_error(enrich, "logical", "cn")

  # 'cutoff' errors
  length_error(cutoff, 1, "cn")
  typeof_error(cutoff, "numeric", "cn")

  # Check for limits of cutoff
  if(is.na(cutoff) || cutoff <= 0 || cutoff >= 1){

    # Send error
    stop("In `cn()`: 'cutoff' must be strictly between 0 and 1.", call. = FALSE)

  }

  # Return 'data' (in case of data frame)
  return(data)

}

#' @noRd
# Co-occurrence counts ----
# Updated 06.09.2026
cooccur <- function(data, window = 2)
{

  # Ensure matrix
  mat <- as.matrix(data)

  # Set empty responses to missing
  mat <- ifelse(mat == "", NA, mat)

  # Get vocabulary
  vocabulary <- sort(na.omit(unique(as.vector(mat))))

  # Recode responses as vocabulary positions
  numeric_mat <- matrix(
    apply(mat, 2, function(column) match(column, vocabulary)),
    nrow = nrow(mat), ncol = ncol(mat)
  )

  # Pad ends with missing
  padded <- cbind(
    matrix(NA, nrow = nrow(numeric_mat), ncol = window),
    numeric_mat,
    matrix(NA, nrow = nrow(numeric_mat), ncol = window)
  )

  # Get number of positions
  n_positions <- ncol(padded)

  # Initialize co-occurrence matrix
  co_occurrence <- matrix(0, nrow = length(vocabulary), ncol = length(vocabulary))

  # Loop over participants
  for(participant in seq_len(nrow(data))){

    # Get row of participant
    row <- padded[participant, ]

    # Get positions with responses
    present_positions <- which(!is.na(row))

    # Check for no responses
    if(length(present_positions) == 0){

      # Skip participant
      next

    }

    # Get responses of participant
    words_here <- row[present_positions]

    # Loop over unique responses
    for(word in unique(words_here)){

      # Get positions of response
      positions_of_word <- present_positions[words_here == word]

      # Get positions within window
      neighbor_positions <- unlist(lapply(positions_of_word, function(position){
        c(position - window:1, position + 1:window)
      }))

      # Keep positions inside row
      neighbor_positions <- neighbor_positions[neighbor_positions >= 1 & neighbor_positions <= n_positions]

      # Get neighboring responses
      neighboring_words <- row[neighbor_positions]

      # Keep distinct neighbors
      neighboring_words <- setdiff(unique(neighboring_words[!is.na(neighboring_words)]), word)

      # Count co-occurrences
      co_occurrence[word, neighboring_words] <- co_occurrence[word, neighboring_words] + 1

    }

  }

  # Return co-occurrences
  return(co_occurrence)

}

#' @noRd
# Relative frequency ----
# Updated 06.09.2026
rel_freq <- function(data)
{

  # Ensure matrix
  mat <- as.matrix(data)

  # Set empty responses to missing
  mat <- ifelse(mat == "", NA, mat)

  # Get vocabulary
  vocabulary <- sort(na.omit(unique(as.vector(mat))))

  # Identify participants giving each response
  said_by <- matrix(
    sapply(vocabulary, function(word) rowSums(mat == word, na.rm = TRUE) > 0),
    nrow = nrow(mat), ncol = length(vocabulary)
  )

  # Return proportion of participants
  return(colMeans(said_by))

}

#' @noRd
# Statistical co-occurrence (Goni et al., 2011) ----
# Updated 06.09.2026
stat_cooccur <- function(data, window = 2, alpha = .05)
{

  # Check for ordered responses
  if(is.numeric(data)){

    # Convert to word responses
    data <- binary2responses(data)

  }

  # Ensure matrix
  mat <- as.matrix(data)

  # Replace bad responses with missing
  mat <- bad_response(mat)

  # Get number of participants
  n_participants <- nrow(mat)

  # Get mean number of responses
  mean_responses <- round(mean(rowSums(!is.na(mat))), 2)

  # Get vocabulary
  vocabulary <- sort(na.omit(unique(as.vector(mat))))

  # Get number of words
  n_words <- length(vocabulary)

  # Initialize network
  adjacency <- matrix(0, nrow = n_words, ncol = n_words)

  # Get relative frequencies
  relative_frequency <- rel_freq(mat)

  # Get co-occurrences
  co_occurrence <- cooccur(mat, window = window)

  # Compute chance probability of co-occurrence
  chance_co_occurrence <- (2 / (mean_responses * (mean_responses - 1))) *
    ((window * mean_responses) - (window * (window + 1) / 2))

  # Check for more than one word
  if(n_words > 1){

    # Check for one response on average
    if(mean_responses <= 1){

      # Send error
      stop(
        "In `cn()`: participants must give more than one response on average (mean = ", mean_responses, ").",
        call. = FALSE
      )

    }

    # Check for window of at least the list length (chance co-occurrence is undefined)
    if(window >= mean_responses){

      # Send error
      stop(
        "In `cn()`: 'window' (", window, ") must be smaller than the mean number of responses per participant (",
        mean_responses, ").",
        call. = FALSE
      )

    }

    # Loop over first words
    for(i in seq_len(n_words - 1)){

      # Loop over second words
      for(j in (i + 1):n_words){

        # Get observed co-occurrences
        observed <- co_occurrence[i, j]

        # Check for repeated co-occurrence
        if(observed > 1){

          # Compute expected co-occurrence by chance
          expected_by_chance <- relative_frequency[i] * relative_frequency[j] * chance_co_occurrence

          # Compute two-sided Clopper-Pearson interval
          confidence_interval <- binom.test(
            observed, n_participants, conf.level = 1 - alpha
          )$conf.int

          # Check for chance below lower bound
          if(expected_by_chance < confidence_interval[1]){

            # Add edge
            adjacency[i, j] <- 1

            # Add reverse edge
            adjacency[j, i] <- 1

          }

        }

      }

    }

  }

  # Set column names
  colnames(adjacency) <- vocabulary

  # Set row names
  row.names(adjacency) <- vocabulary

  # Return network
  return(adjacency)

}

#' @noRd
# Generalized topological overlap ----
# Updated 06.09.2026
gtom <- function(A, steps = 2)
{

  # Initialize reachable nodes
  reachable <- A

  # Initialize expanded network
  expanded <- reachable

  # Get number of nodes
  n_nodes <- ncol(A)

  # Loop over steps
  for(step in seq_len(steps)[-1]){

    # Loop over nodes
    for(node in seq_len(n_nodes)){

      # Get neighbors of node
      neighbors <- as.vector(unname(which(reachable[node, ] == 1)))

      # Check for one neighbor
      if(length(neighbors) == 1){

        # Keep row as matrix
        neighbor_rows <- t(as.matrix(reachable[neighbors, ]))

      }else{

        # Get rows of neighbors
        neighbor_rows <- reachable[neighbors, ]

      }

      # Get neighbors of neighbors
      neighbors_of_neighbors <- unname(unlist(apply(neighbor_rows, 1, function(row) which(row == 1))))

      # Get all reachable nodes
      all_reachable <- setdiff(unique(neighbors_of_neighbors), node)

      # Add reachable nodes
      expanded[node, all_reachable] <- 1

      # Add reverse reachable nodes
      expanded[all_reachable, node] <- 1

    }

    # Update reachable nodes
    reachable <- expanded

  }

  # Compute numerator
  numerator <- reachable %*% reachable + A + diag(1, ncol(A))

  # Get reachable totals
  reachable_totals <- colSums(reachable)

  # Get totals by row
  totals_by_row <- matrix(rep(reachable_totals, ncol(A)), nrow = ncol(A), byrow = TRUE)

  # Get totals by column
  totals_by_col <- t(totals_by_row)

  # Compute denominator
  denominator <- -A + pmin(totals_by_row, totals_by_col) + 1

  # Return overlap
  return(numerator / denominator)

}

#' @noRd
# Enrich network ----
# Updated 13.10.2026
enrich_network <- function(A, GTOM, cutoff = 0.50)
{

  # Compute dissimilarity
  dissimilarity <- as.dist(1 - GTOM, upper = TRUE)

  # Cluster nodes
  linkage <- hclust(d = dissimilarity, method = "average")

  # Cut dendrogram into modules
  modules <- cutree(linkage, h = cutoff)

  # Loop over modules
  for(module in seq_len(max(modules))){

    # Get nodes of module
    members <- which(modules == module)

    # Connect nodes of module
    A[members, members] <- 1

  }

  # Remove self-loops
  diag(A) <- 0

  # Return network
  return(A)

}
