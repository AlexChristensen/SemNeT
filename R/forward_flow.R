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
#' flow for it (at least 2: the first response has no earlier response to be
#' measured against). Blank-like responses (\code{""}, \code{" "},
#' \code{"NA"}) are treated as missing, and responses are counted before any
#' not found in \code{embeddings} are dropped. A response a participant
#' repeats has a distance of 0 to its earlier occurrence, and is kept.
#' Defaults to \code{3}
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
# Forward flow ----
# Updated 07.09.2026
forward_flow <- function(response_matrix, embeddings, min_response = 3, max_response = NULL, type = c("fluency", "free"), cores = 1)
{

  # Argument errors
  validated <- forward_flow_errors(response_matrix, embeddings, min_response, max_response, type, cores)

  # Check for fluency data
  if(validated$type == "fluency"){

    # Return fluency forward flow
    return(forward_flow_fluency(validated$response_matrix, validated$embeddings, min_response, max_response, cores))

  }

  # Return free association forward flow
  return(forward_flow_free(validated$response_matrix, validated$embeddings, min_response, max_response, cores))

}

#' @noRd
# Response flow ----
# Updated 07.09.2026
response_flow <- function(responses, embeddings)
{

  # Lower case responses
  responses <- tolower(responses)

  # Get number of responses
  n <- length(responses)

  # Check for one response or fewer
  if(n <= 1){

    # Return undefined flow
    return(list(flow = NA_real_, mean_flow = NA_real_))

  }

  # Get vectors of responses
  vectors <- t(embeddings[match(responses, rownames(embeddings)), , drop = FALSE])

  # Get similarities between responses
  similarities <- cosine(vectors)

  # Get flow of each response
  flow <- vapply(2:n, function(i) mean(1 - similarities[i, seq_len(i - 1)], na.rm = TRUE), numeric(1))

  # Set undefined flow to missing
  flow[is.nan(flow)] <- NA_real_

  # Get mean flow
  mean_flow <- mean(flow, na.rm = TRUE)

  # Set undefined mean flow to missing
  mean_flow <- swiftelse_scalar(is.nan(mean_flow), NA_real_, mean_flow)

  # Return flow
  return(list(flow = flow, mean_flow = mean_flow))

}

#' @noRd
# Forward flow of fluency data ----
# Updated 07.09.2026
forward_flow_fluency <- function(response_matrix, embeddings, min_response, max_response, cores)
{

  # Get participant IDs
  ids <- rownames(response_matrix)

  # Check for no IDs
  if(is.null(ids)){

    # Number participants
    ids <- as.character(seq_len(nrow(response_matrix)))

  }

  # Loop over participants
  response_list <- lapply(seq_len(nrow(response_matrix)), function(i){

    # Replace bad responses with missing
    responses <- bad_response(response_matrix[i, ])

    # Remove missing responses
    responses <- responses[!is.na(responses)]

    # Check for maximum responses
    if(!is.null(max_response)){

      # Keep first responses
      responses <- utils::head(responses, max_response)

    }

    # Check for too few responses
    if(length(responses) < min_response){

      # Return no responses
      return(character(0))

    }

    # Return responses
    return(responses)

  })

  # Get vocabulary
  vocabulary <- unique(tolower(unlist(response_list)))

  # Keep embeddings of vocabulary
  shrunk <- embeddings[rownames(embeddings) %in% vocabulary, , drop = FALSE]

  # Compute flow of each participant
  results <- parallel_process(
    length(response_list),
    function(i) response_flow(response_list[[i]], shrunk),
    cores = cores
  )

  # Set up mean flow
  mean_flow <- data.frame(
    ID = ids,
    FF = vapply(results, function(result) result$mean_flow, numeric(1)),
    stringsAsFactors = FALSE
  )

  # Get flow of each response
  response_flow_list <- lapply(results, function(result) result$flow)

  # Name flow by participant
  names(response_flow_list) <- ids

  # Return flow
  return(list(mean_flow = mean_flow, response_flow = response_flow_list))

}

#' @noRd
# Forward flow of free association data ----
# Updated 07.09.2026
forward_flow_free <- function(response_matrix, embeddings, min_response, max_response, cores)
{

  # Get participant IDs
  ids <- unique(response_matrix$ID)

  # Get number of rows
  n_rows <- nrow(response_matrix)

  # Split rows by participant
  rows_by_id <- split(seq_len(n_rows), factor(response_matrix$ID, levels = ids))

  # Initialize responses of cues
  cue_responses <- vector("list", n_rows)

  # Initialize IDs of cues
  cue_ids <- character(n_rows)

  # Initialize names of cues
  cue_names <- character(n_rows)

  # Initialize number of groups
  n_groups <- 0L

  # Loop over participants
  for(id in names(rows_by_id)){

    # Get rows of participant
    rows <- rows_by_id[[id]]

    # Get cues of participant
    cue_levels <- unique(response_matrix$Cue[rows])

    # Split rows by cue
    rows_by_cue <- split(rows, factor(response_matrix$Cue[rows], levels = cue_levels))

    # Loop over cues
    for(cue in names(rows_by_cue)){

      # Get responses to cue
      responses <- response_matrix$Response[rows_by_cue[[cue]]]

      # Check for maximum responses
      if(!is.null(max_response)){

        # Keep first responses
        responses <- utils::head(responses, max_response)

      }

      # Check for enough responses
      if(length(responses) >= min_response){

        # Update number of groups
        n_groups <- n_groups + 1L

        # Store responses
        cue_responses[[n_groups]] <- responses

        # Store ID
        cue_ids[n_groups] <- id

        # Store cue
        cue_names[n_groups] <- cue

      }

    }

  }

  # Trim responses
  cue_responses <- cue_responses[seq_len(n_groups)]

  # Trim IDs
  cue_ids <- cue_ids[seq_len(n_groups)]

  # Trim cues
  cue_names <- cue_names[seq_len(n_groups)]

  # Get vocabulary
  vocabulary <- unique(tolower(unlist(cue_responses)))

  # Keep embeddings of vocabulary
  shrunk <- embeddings[rownames(embeddings) %in% vocabulary, , drop = FALSE]

  # Compute flow of each participant-cue
  results <- parallel_process(
    length(cue_responses),
    function(i) response_flow(cue_responses[[i]], shrunk),
    cores = cores
  )

  # Set up mean flow of cues
  mean_response_flow <- data.frame(
    ID = cue_ids,
    Cue = cue_names,
    FF = vapply(results, function(result) result$mean_flow, numeric(1)),
    stringsAsFactors = FALSE
  )

  # Average flow across cues
  by_id <- if(nrow(mean_response_flow) > 0){

    # Aggregate by participant
    stats::aggregate(FF ~ ID, data = mean_response_flow, FUN = mean, na.rm = TRUE)

  }else{

    # Set empty result
    data.frame(ID = character(0), FF = numeric(0), stringsAsFactors = FALSE)

  }

  # Keep every participant
  mean_flow <- merge(data.frame(ID = ids, stringsAsFactors = FALSE), by_id, by = "ID", all.x = TRUE)

  # Split results by participant
  result_index_by_id <- split(seq_along(cue_ids), factor(cue_ids, levels = ids))

  # Initialize flow of responses
  response_flow_list <- vector("list", length(ids))

  # Name flow by participant
  names(response_flow_list) <- ids

  # Loop over participants
  for(id in ids){

    # Get name of participant
    id_name <- as.character(id)

    # Get results of participant
    id_index <- result_index_by_id[[id_name]]

    # Get flow of each cue
    per_cue <- lapply(results[id_index], function(result) result$flow)

    # Name flow by cue
    names(per_cue) <- cue_names[id_index]

    # Store flow of participant
    response_flow_list[[id_name]] <- per_cue

  }

  # Return flow
  return(list(mean_flow = mean_flow, mean_response_flow = mean_response_flow, response_flow = response_flow_list))

}

#' @noRd
# Errors ----
# Updated 13.10.2026
forward_flow_errors <- function(response_matrix, embeddings, min_response, max_response, type, cores)
{

  # 'type' errors
  length_error(type, c(1, 2), "forward_flow")
  typeof_error(type, "character", "forward_flow")

  # Match type
  type <- match.arg(type, c("fluency", "free"))

  # 'response_matrix' errors
  if(type == "fluency"){

    # Check for response matrix
    response_matrix <- response_matrix_error(response_matrix, "forward_flow")

  }else{

    # Check for matrix or data frame
    object_error(response_matrix, c("matrix", "data.frame"), "forward_flow")

    # Check for matrix
    if(is.matrix(response_matrix)){

      # Convert to data frame
      response_matrix <- as.data.frame(response_matrix, stringsAsFactors = FALSE)

    }

    # Check for required columns
    if(!all(c("ID", "Cue", "Response") %in% colnames(response_matrix))){

      # Send error
      stop(
        "In `forward_flow()`: `type = \"free\"` requires columns \"ID\", \"Cue\", and \"Response\" in `response_matrix`.",
        call. = FALSE
      )

    }

  }

  # 'embeddings' errors
  object_error(embeddings, c("matrix", "data.frame"), "forward_flow")

  # Check for data frame
  if(is.data.frame(embeddings)){

    # Convert to matrix
    embeddings <- as.matrix(embeddings)

  }

  # Check for numeric
  typeof_error(embeddings, "numeric", "forward_flow")

  # Check for row names
  if(is.null(rownames(embeddings))){

    # Send error
    stop(
      "In `forward_flow()`: `embeddings` must have row names identifying each vocabulary term.",
      call. = FALSE
    )

  }

  # Lower case row names
  rownames(embeddings) <- tolower(rownames(embeddings))

  # Check for duplicate row names
  if(anyDuplicated(rownames(embeddings)) > 0){

    # Send error
    stop(
      "In `forward_flow()`: `embeddings` has row names that are the same once lower-cased (e.g., \"Apple\" and \"apple\").",
      call. = FALSE
    )

  }

  # 'min_response' errors
  length_error(min_response, 1, "forward_flow")
  typeof_error(min_response, "numeric", "forward_flow")
  range_error(min_response, c(2, Inf), "forward_flow")

  # 'max_response' errors
  if(!is.null(max_response)){
    length_error(max_response, 1, "forward_flow")
    typeof_error(max_response, "numeric", "forward_flow")
    range_error(max_response, c(1, Inf), "forward_flow")
  }

  # 'cores' errors
  length_error(cores, 1, "forward_flow")
  typeof_error(cores, "numeric", "forward_flow")
  range_error(cores, c(1, Inf), "forward_flow")

  # Return validated input (in case of data frames or matched 'type')
  return(list(response_matrix = response_matrix, embeddings = embeddings, type = type))

}
