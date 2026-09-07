#' @title Community Network Estimation
#'
#' @description Estimates a semantic network using the Community Network
#' method described in Goni et al. (2011)
#'
#' @param data Matrix or data frame.
#' A preprocessed verbal fluency matrix where rows are participants and
#' columns are the order of their verbal fluency responses. Content may be
#' the raw word responses or an ordered numeric response matrix (see
#' \code{responses_to_binary()})
#'
#' @param window Numeric.
#' Size of window to look for co-occurrences in.
#' Defaults to \code{2}
#'
#' @param alpha Numeric.
#' Significance value.
#' Defaults to \code{.05}
#'
#' @param enrich Boolean.
#' Should the network be enriched by connecting all nodes within their
#' respective modules?
#' Defaults to \code{FALSE}
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
# Community network estimation
# Updated 06.09.2026
cn <- function(data, window = 2, alpha = .05, enrich = FALSE)
{

  # Validate and coerce input before any computation begins
  data <- cn_errors(data, window, alpha, enrich)

  # Statistically significant co-occurrence network
  adjacency <- stat_cooccur(data, window = window, alpha = alpha)

  # Isolated responses (no significant co-occurrence with anything) are
  # dropped by retaining only the largest connected component
  components <- suppressWarnings(igraph::components(convert2igraph(adjacency)))
  largest_component <- which(components$membership == which.max(components$csize))
  adjacency <- adjacency[largest_component, largest_component]

  # Optionally connect every node within the same module to every other node
  if(enrich){
    adjacency <- enrich_network(adjacency, gtom(adjacency))
  }

  return(adjacency)

}

#' @noRd
# Argument errors for `cn()`
# Updated 06.09.2026
cn_errors <- function(data, window, alpha, enrich)
{

  # 'data' must be a raw (not-yet-binarized) response matrix -- co-occurrence
  # windowing needs response order, which a plain 0/1 binary matrix has
  # already discarded
  data <- ordered_response_matrix_error(data, "cn")

  # 'window' must be a single positive whole number
  length_error(window, 1, "cn")
  typeof_error(window, "numeric", "cn")
  range_error(window, c(1, Inf), "cn")

  # 'alpha' must be a single value between 0 and 1
  length_error(alpha, 1, "cn")
  typeof_error(alpha, "numeric", "cn")
  range_error(alpha, c(0, 1), "cn")

  # 'enrich' must be a single logical value
  object_error(enrich, "vector", "cn")
  typeof_error(enrich, "logical", "cn")
  length_error(enrich, 1, "cn")

  return(data)

}

#' @noRd
# Word co-occurrence counts within a sliding window of response order
# Used only by `stat_cooccur()` below
# Updated 06.09.2026
cooccur <- function(data, window = 2)
{

  mat <- as.matrix(data)
  mat <- ifelse(mat == "", NA, mat)

  # The full vocabulary observed across all participants
  vocabulary <- sort(na.omit(unique(as.vector(mat))))

  # Recode every response as its integer position in the vocabulary
  numeric_mat <- apply(mat, 2, function(column) match(column, vocabulary))

  # Pad both ends with NA so a window lookup never runs off the row
  padded <- cbind(
    matrix(NA, nrow = nrow(numeric_mat), ncol = window),
    numeric_mat,
    matrix(NA, nrow = nrow(numeric_mat), ncol = window)
  )

  n_positions <- ncol(padded)
  co_occurrence <- matrix(0, nrow = length(vocabulary), ncol = length(vocabulary))

  # For every participant, tally which words fell within 'window' positions
  # of each other in their own response order. Looping participant-outer and
  # only over the (small) set of words *that participant actually said* --
  # rather than word-outer over the *entire* vocabulary for every participant,
  # as a first version of this did -- computes the exact same commutative sum
  # (matrix-cell increments don't depend on visiting order) over far fewer
  # (participant, word) combinations: most vocabulary words were never said
  # by most participants, and the original spent a `which()` scan finding
  # that out empty-handed for every single one of them. Benchmarked ~2.5x
  # faster on the bundled `open.clean` fixture, verified identical output
  # (including when a participant repeats the same response, which changes
  # nothing here since the original already unioned that word's neighbors
  # across all of its positions before deduplicating)
  for(participant in seq_len(nrow(data))){

    row <- padded[participant, ]
    present_positions <- which(!is.na(row))

    if(length(present_positions) == 0){
      next
    }

    words_here <- row[present_positions]

    for(word in unique(words_here)){

      positions_of_word <- present_positions[words_here == word]

      neighbor_positions <- unlist(lapply(positions_of_word, function(position){
        c(position - window:1, position + 1:window)
      }))
      neighbor_positions <- neighbor_positions[neighbor_positions >= 1 & neighbor_positions <= n_positions]

      neighboring_words <- row[neighbor_positions]
      neighboring_words <- setdiff(unique(neighboring_words[!is.na(neighboring_words)]), word)

      co_occurrence[word, neighboring_words] <- co_occurrence[word, neighboring_words] + 1

    }

  }

  return(co_occurrence)

}

#' @noRd
# Relative frequency (proportion of participants) of each response
# Used only by `stat_cooccur()` below
# Updated 06.09.2026
rel_freq <- function(data)
{

  mat <- as.matrix(data)
  mat <- ifelse(mat == "", NA, mat)

  vocabulary <- sort(na.omit(unique(as.vector(mat))))

  # For every word in the vocabulary, whether each participant's row
  # contains it at least once -- vectorized via `sapply()`/`rowSums()` in
  # place of the original's nested per-word, per-participant for-loops.
  # `sapply()` only returns a proper (participants x vocabulary) matrix when
  # each per-word result has length > 1; with exactly 1 participant, every
  # per-word result collapses to a length-1 logical and `sapply()` silently
  # returns a plain vector instead, which `colMeans()` below would then
  # reject outright -- reshaping explicitly guards against that regardless
  # of how many participants there are
  said_by <- matrix(
    sapply(vocabulary, function(word) rowSums(mat == word, na.rm = TRUE) > 0),
    nrow = nrow(mat), ncol = length(vocabulary)
  )

  return(colMeans(said_by))

}

#' @importFrom stats binom.test
#' @noRd
# Statistical co-occurrence network estimation (Goni et al., 2011)
# Used only by `cn()` above
# Updated 06.09.2026
stat_cooccur <- function(data, window = 2, alpha = .05)
{

  # An ordered numeric response matrix must first become word responses
  if(is.numeric(data)){
    data <- binary_to_responses(data)
  }

  mat <- as.matrix(data)
  mat <- bad_response(mat)

  n_participants <- nrow(mat)

  # Average number of responses given per participant
  mean_responses <- round(mean(rowSums(!is.na(mat))), 2)

  vocabulary <- sort(na.omit(unique(as.vector(mat))))
  n_words <- length(vocabulary)

  adjacency <- matrix(0, nrow = n_words, ncol = n_words)

  relative_frequency <- rel_freq(data)
  co_occurrence <- cooccur(data, window = window)

  # Probability that two words would co-occur by chance alone
  chance_co_occurrence <- (2 / (mean_responses * (mean_responses - 1))) *
    ((window * mean_responses) - (window * (window + 1) / 2))

  # An edge is retained when its observed co-occurrence significantly
  # exceeds what chance alone (given each word's own frequency) would predict.
  # With fewer than 2 words in the vocabulary, no pair exists to test at all --
  # `1:(n_words - 1)` would otherwise silently count *down* (e.g. `1:0` is
  # `c(1, 0)`, not empty) and the inner range would then index outside the
  # 1x1 (or 0x0) 'adjacency' matrix, erroring instead of returning the
  # already-correct all-zero result computed above
  if(n_words > 1){
    for(i in seq_len(n_words - 1)){
      for(j in (i + 1):n_words){

        observed <- co_occurrence[i, j]

        if(observed > 1){

          expected_by_chance <- relative_frequency[i] * relative_frequency[j] * chance_co_occurrence
          confidence_interval <- binom.test(observed, n_participants, alpha)$conf.int

          if(expected_by_chance < confidence_interval[1]){
            adjacency[i, j] <- 1
            adjacency[j, i] <- 1
          }

        }

      }
    }
  }

  colnames(adjacency) <- vocabulary
  row.names(adjacency) <- vocabulary

  return(adjacency)

}

#' @noRd
# Generalized topological overlap: how many neighbors two nodes share,
# extended outward by 'steps' hops
# Used only by `cn()` above, via `enrich = TRUE`
# Updated 06.09.2026
gtom <- function(A, steps = 2)
{

  reachable <- A
  expanded <- reachable
  n_nodes <- ncol(A)

  for(step in seq_len(steps)[-1]){
    for(node in seq_len(n_nodes)){

      neighbors <- as.vector(unname(which(reachable[node, ] == 1)))

      # A single neighbor's row needs re-wrapping as a matrix, or `apply()`
      # below would iterate over its individual elements instead of the row
      if(length(neighbors) == 1){
        neighbor_rows <- t(as.matrix(reachable[neighbors, ]))
      }else{
        neighbor_rows <- reachable[neighbors, ]
      }

      neighbors_of_neighbors <- unname(unlist(apply(neighbor_rows, 1, function(row) which(row == 1))))
      all_reachable <- setdiff(unique(neighbors_of_neighbors), node)

      expanded[node, all_reachable] <- 1
      expanded[all_reachable, node] <- 1

    }

    reachable <- expanded

  }

  numerator <- reachable %*% reachable + A + diag(1, ncol(A))

  reachable_totals <- colSums(reachable)
  totals_by_row <- matrix(rep(reachable_totals, ncol(A)), nrow = ncol(A), byrow = TRUE)
  totals_by_col <- t(totals_by_row)

  denominator <- -A + pmin(totals_by_row, totals_by_col) + 1

  return(numerator / denominator)

}

#' @importFrom stats hclust cutree as.dist
#' @noRd
# Connect every node within the same module (via hierarchical clustering on
# generalized topological overlap) to every other node in that module
# Used only by `cn()` above, via `enrich = TRUE`
# Updated 06.09.2026
enrich_network <- function(A, GTOM)
{

  dissimilarity <- as.dist(1 - GTOM, upper = TRUE)
  linkage <- hclust(d = dissimilarity, method = "average")

  # Module cut-height follows Goni et al. (2011)
  modules <- cutree(linkage, h = 0.35)

  for(module in seq_len(max(modules))){
    members <- which(modules == module)
    A[members, members] <- 1
  }

  diag(A) <- 0

  return(A)

}
