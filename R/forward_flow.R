#' @title Forward Flow
#'
#' @description Computes the "forward flow" of a sequence of verbal fluency
#' or free association responses (Gray et al., 2019): for each response
#' after the first, the mean semantic distance (1 - cosine similarity)
#' between that response and every response that preceded it, using word
#' vectors from a user-supplied embedding matrix.
#'
#' Unlike the pre-2.0.0 version of this function, no semantic space is
#' downloaded automatically -- CRAN policy prohibits a package silently
#' authenticating with and downloading files from an external service at
#' runtime. Bring your own pre-trained (e.g. GloVe, word2vec) or custom word
#' embeddings instead
#'
#' @param response_matrix Matrix or data frame.
#' For \code{type = "fluency"}: participants (rows) by responses (columns),
#' one word per cell, in the order each participant gave them. Row names, if
#' present, are used as participant IDs.
#' For \code{type = "free"}: long-format data with columns named
#' \code{"ID"}, \code{"Cue"}, and \code{"Response"}, one response per row, in
#' the order each participant gave them for each cue
#'
#' @param embeddings Matrix or data frame.
#' A word-embedding matrix: one row per vocabulary term (row names are the
#' terms themselves), one column per embedding dimension. Not bundled or
#' downloaded by this package -- supply your own pre-trained or custom
#' vectors. A response with no matching row is treated as missing (excluded
#' from the distances it would have contributed to, rather than corrupting
#' every flow value it touches)
#'
#' @param min_response Numeric.
#' Minimum number of responses a participant (\code{type = "fluency"}) or
#' participant-cue pair (\code{type = "free"}) must have to compute forward
#' flow for it. Defaults to \code{3}
#'
#' @param max_response Numeric.
#' Maximum number of responses to use, keeping only the first \code{n}.
#' Useful for decoupling forward flow from fluency (participants who simply
#' produced more responses). Defaults to \code{NULL}, which uses every
#' response
#'
#' @param type Character.
#' \code{"fluency"} or \code{"free"}. Defaults to \code{"fluency"}
#'
#' @param cores Numeric.
#' Number of computer processing cores to use. Defaults to \code{1}
#'
#' @return For \code{type = "fluency"}, a list with:
#' \item{mean_flow}{A data frame of each participant's average forward flow}
#' \item{response_flow}{A list, one element per participant, of that
#' participant's response-by-response forward flow}
#'
#' For \code{type = "free"}, a list with:
#' \item{mean_flow}{A data frame of each participant's average forward flow,
#' across all of their cues}
#' \item{mean_response_flow}{A data frame of each participant-cue pair's
#' average forward flow}
#' \item{response_flow}{A list, one element per participant (itself a list
#' with one element per cue), of that cue's response-by-response forward
#' flow}
#'
#' @examples
#' # A tiny toy embedding space and toy fluency data -- illustrates the
#' # interface only; use real pre-trained vectors (GloVe, word2vec, etc.) in
#' # practice
#' set.seed(1)
#' words <- c("dog", "cat", "wolf", "car", "truck", "bus")
#' embeddings <- matrix(rnorm(length(words) * 5), nrow = length(words))
#' row.names(embeddings) <- words
#'
#' responses <- matrix(
#'   c("dog", "wolf", "cat", "car", "truck", "bus"),
#'   nrow = 2, byrow = TRUE, dimnames = list(c("P1", "P2"), NULL)
#' )
#'
#' result <- forward_flow(responses, embeddings, min_response = 2)
#' result$mean_flow
#'
#' @references
#' Gray, K., Anderson, S., Chen, E. E., Kelly, J. M., Christian, M. S., Patrick, J., ... & Lewis, K. (2019).
#' "Forward flow": A new measure to quantify free thought and predict creativity.
#' \emph{American Psychologist}, \emph{74}(5), 539-554.
#'
#' Beaty, R. E., Zeitlen, D. C., Baker, B. S., & Kenett, Y. N. (2021).
#' Forward flow and creative thought: Assessing associative cognition and its role in divergent thinking.
#' \emph{Thinking Skills and Creativity}, 100859.
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com> & Brendan Baker <bsb5477@psu.edu>
#'
#' @export
# Forward flow -- see NEWS.md for why this no longer auto-downloads a semantic space
# Updated 07.09.2026
forward_flow <- function(response_matrix, embeddings, min_response = 3, max_response = NULL, type = c("fluency", "free"), cores = 1)
{

  validated <- forward_flow_errors(response_matrix, embeddings, min_response, max_response, type, cores)

  if(validated$type == "fluency"){
    return(forward_flow_fluency(validated$response_matrix, validated$embeddings, min_response, max_response, cores))
  }

  return(forward_flow_free(validated$response_matrix, validated$embeddings, min_response, max_response, cores))

}

#' @noRd
# One participant's (or one participant-cue's) response-by-response forward
# flow -- the semantic distance of each response to the mean of every
# response before it. A response with no matching `embeddings` row becomes
# an all-`NA` (then all-zero via `cosine()`) vector, which comes out as
# `NaN` similarity and is excluded by `na.rm = TRUE` below, rather than
# corrupting every flow value it touches
# Updated 07.09.2026
response_flow <- function(responses, embeddings)
{

  responses <- tolower(responses)
  n <- length(responses)

  if(n <= 1){
    return(list(flow = NA_real_, mean_flow = NA_real_))
  }

  vectors <- t(embeddings[match(responses, rownames(embeddings)), , drop = FALSE])

  # One cosine similarity matrix for the whole sequence, computed once --
  # each response's forward flow only ever needs its own row against the
  # columns that precede it, so there's no need to recompute this per response
  similarities <- cosine(vectors)

  flow <- vapply(2:n, function(i) mean(1 - similarities[i, seq_len(i - 1)], na.rm = TRUE), numeric(1))

  # A response with zero matched vocabulary among the responses before it (or
  # a participant/cue with zero matched vocabulary at all) hits `mean()` of
  # an empty, fully-`na.rm`'d vector, which is `NaN`, not `NA` -- normalized
  # here so "no data" is always `NA`, never a stray `NaN` in the output
  flow[is.nan(flow)] <- NA_real_
  mean_flow <- mean(flow, na.rm = TRUE)
  mean_flow <- swiftelse_scalar(is.nan(mean_flow), NA_real_, mean_flow)

  return(list(flow = flow, mean_flow = mean_flow))

}

#' @noRd
# `type = "fluency"`: one row per participant, one raw response per column
# Updated 07.09.2026
forward_flow_fluency <- function(response_matrix, embeddings, min_response, max_response, cores)
{

  ids <- rownames(response_matrix)

  if(is.null(ids)){
    ids <- as.character(seq_len(nrow(response_matrix)))
  }

  response_list <- lapply(seq_len(nrow(response_matrix)), function(i){

    # `response_matrix[i, ]` is already a plain vector -- `response_matrix`
    # was coerced to a matrix by `response_matrix_error()`, and indexing a
    # single row out of a matrix always drops to a vector
    responses <- stats::na.omit(response_matrix[i, ])

    if(!is.null(max_response)){
      responses <- utils::head(responses, max_response)
    }

    # Below `min_response`: `character(0)` short-circuits to `NA` inside
    # `response_flow()`'s own `n <= 1` check, same as any other participant
    # with too few usable responses
    if(length(responses) < min_response){
      return(character(0))
    }

    return(responses)

  })

  # Shrinking `embeddings` down to only the vocabulary actually used keeps
  # memory flat regardless of how large the full embedding matrix is
  vocabulary <- unique(tolower(unlist(response_list)))
  shrunk <- embeddings[rownames(embeddings) %in% vocabulary, , drop = FALSE]

  results <- parallel_process(
    length(response_list),
    function(i) response_flow(response_list[[i]], shrunk),
    cores = cores
  )

  mean_flow <- data.frame(
    ID = ids,
    FF = vapply(results, function(result) result$mean_flow, numeric(1)),
    stringsAsFactors = FALSE
  )

  response_flow_list <- lapply(results, function(result) result$flow)
  names(response_flow_list) <- ids

  return(list(mean_flow = mean_flow, response_flow = response_flow_list))

}

#' @noRd
# `type = "free"`: long-format ID/Cue/Response data, one response per row.
# Every participant-cue pair meeting `min_response` becomes one item to
# parallelize over (there are usually far more of these than participants)
# Updated 07.09.2026
forward_flow_free <- function(response_matrix, embeddings, min_response, max_response, cores)
{

  ids <- unique(response_matrix$ID)
  n_rows <- nrow(response_matrix)

  # Group row indices by participant, then by cue within each participant, in
  # one `split()` pass over the table -- re-scanning the *entire* table once
  # per participant (`response_matrix$ID == id`, as this used to do) is
  # effectively quadratic in the number of rows for a dataset with many
  # participants (benchmarked ~50x faster at 2,400 participants / 360,000
  # rows, identical output)
  rows_by_id <- split(seq_len(n_rows), factor(response_matrix$ID, levels = ids))

  # Pre-allocated to the maximum possible size (one entry per row) and
  # trimmed at the end -- growing these with `c()`/`[[<-` one participant-cue
  # pair at a time would reintroduce the same quadratic cost this fix removes
  cue_responses <- vector("list", n_rows)
  cue_ids <- character(n_rows)
  cue_names <- character(n_rows)
  n_groups <- 0L

  for(id in names(rows_by_id)){

    rows <- rows_by_id[[id]]
    cue_levels <- unique(response_matrix$Cue[rows])
    rows_by_cue <- split(rows, factor(response_matrix$Cue[rows], levels = cue_levels))

    for(cue in names(rows_by_cue)){

      responses <- response_matrix$Response[rows_by_cue[[cue]]]

      if(!is.null(max_response)){
        responses <- utils::head(responses, max_response)
      }

      if(length(responses) >= min_response){
        n_groups <- n_groups + 1L
        cue_responses[[n_groups]] <- responses
        cue_ids[n_groups] <- id
        cue_names[n_groups] <- cue
      }

    }

  }

  cue_responses <- cue_responses[seq_len(n_groups)]
  cue_ids <- cue_ids[seq_len(n_groups)]
  cue_names <- cue_names[seq_len(n_groups)]

  vocabulary <- unique(tolower(unlist(cue_responses)))
  shrunk <- embeddings[rownames(embeddings) %in% vocabulary, , drop = FALSE]

  results <- parallel_process(
    length(cue_responses),
    function(i) response_flow(cue_responses[[i]], shrunk),
    cores = cores
  )

  mean_response_flow <- data.frame(
    ID = cue_ids,
    Cue = cue_names,
    FF = vapply(results, function(result) result$mean_flow, numeric(1)),
    stringsAsFactors = FALSE
  )

  # Every participant gets a row, even one whose every cue fell below
  # `min_response` (`FF` is `NA` for them, rather than the participant
  # silently disappearing from the output)
  by_id <- if(nrow(mean_response_flow) > 0){
    stats::aggregate(FF ~ ID, data = mean_response_flow, FUN = mean, na.rm = TRUE)
  }else{
    data.frame(ID = character(0), FF = numeric(0), stringsAsFactors = FALSE)
  }

  mean_flow <- merge(data.frame(ID = ids, stringsAsFactors = FALSE), by_id, by = "ID", all.x = TRUE)

  # Same fix as the grouping step above: group each participant's qualifying
  # (id, cue) result indices once via `split()`, instead of re-scanning
  # `cue_ids` (length = number of qualifying participant-cue pairs) once per
  # participant
  result_index_by_id <- split(seq_along(cue_ids), factor(cue_ids, levels = ids))

  response_flow_list <- vector("list", length(ids))
  names(response_flow_list) <- ids

  for(id in ids){
    # `[[` on a list indexes *positionally* when given a numeric value --
    # `ids` can itself be numeric (e.g. integer participant IDs), so an
    # unconverted `id` here would silently look up the wrong participant (or
    # throw "subscript out of bounds" once an ID exceeds the number of
    # participants) instead of doing the name-based lookup this needs
    id_name <- as.character(id)
    id_index <- result_index_by_id[[id_name]]
    per_cue <- lapply(results[id_index], function(result) result$flow)
    names(per_cue) <- cue_names[id_index]
    response_flow_list[[id_name]] <- per_cue
  }

  return(list(mean_flow = mean_flow, mean_response_flow = mean_response_flow, response_flow = response_flow_list))

}

#' @noRd
# Argument errors for `forward_flow()`
# Updated 07.09.2026
forward_flow_errors <- function(response_matrix, embeddings, min_response, max_response, type, cores)
{

  type <- match.arg(type, c("fluency", "free"))

  if(type == "fluency"){

    response_matrix <- response_matrix_error(response_matrix, "forward_flow")

  }else{

    object_error(response_matrix, c("matrix", "data.frame"), "forward_flow")

    if(is.matrix(response_matrix)){
      response_matrix <- as.data.frame(response_matrix, stringsAsFactors = FALSE)
    }

    if(!all(c("ID", "Cue", "Response") %in% colnames(response_matrix))){
      stop(
        "In `forward_flow()`: `type = \"free\"` requires columns \"ID\", \"Cue\", and \"Response\" in `response_matrix`.",
        call. = FALSE
      )
    }

  }

  object_error(embeddings, c("matrix", "data.frame"), "forward_flow")

  if(is.data.frame(embeddings)){
    embeddings <- as.matrix(embeddings)
  }

  typeof_error(embeddings, "numeric", "forward_flow")

  if(is.null(rownames(embeddings))){
    stop(
      "In `forward_flow()`: `embeddings` must have row names identifying each vocabulary term.",
      call. = FALSE
    )
  }

  rownames(embeddings) <- tolower(rownames(embeddings))

  length_error(min_response, 1, "forward_flow")
  typeof_error(min_response, "numeric", "forward_flow")
  range_error(min_response, c(1, Inf), "forward_flow")

  if(!is.null(max_response)){
    length_error(max_response, 1, "forward_flow")
    typeof_error(max_response, "numeric", "forward_flow")
    range_error(max_response, c(1, Inf), "forward_flow")
  }

  length_error(cores, 1, "forward_flow")
  typeof_error(cores, "numeric", "forward_flow")
  range_error(cores, c(1, Inf), "forward_flow")

  return(list(response_matrix = response_matrix, embeddings = embeddings, type = type))

}
