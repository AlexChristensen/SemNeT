# Tests for forward_flow()

make_embeddings <- function(){
  words <- c("dog", "cat", "wolf", "car", "truck", "bus")
  set.seed(1)
  embeddings <- matrix(rnorm(length(words) * 5), nrow = length(words))
  row.names(embeddings) <- words
  return(embeddings)
}

test_that("forward_flow computes fluency mean/response flow per participant", {

  embeddings <- make_embeddings()

  responses <- matrix(
    c("dog", "wolf", "cat", "car", "truck", "bus"),
    nrow = 2, byrow = TRUE, dimnames = list(c("P1", "P2"), NULL)
  )

  result <- forward_flow(responses, embeddings, min_response = 2)

  expect_true(is.data.frame(result$mean_flow))
  expect_equal(result$mean_flow$ID, c("P1", "P2"))
  expect_equal(names(result$response_flow), c("P1", "P2"))
  expect_length(result$response_flow$P1, 2)

  # Second response's flow is exactly `1 - cosine(response_2, response_1)`
  expect_equal(unname(result$response_flow$P1[1]), 1 - cosine(embeddings["wolf", ], embeddings["dog", ]))

})

test_that("forward_flow returns NA flow for participants below min_response", {

  embeddings <- make_embeddings()

  responses <- matrix(
    c("dog", NA, NA, "car", "truck", "bus"),
    nrow = 2, byrow = TRUE, dimnames = list(c("P1", "P2"), NULL)
  )

  result <- forward_flow(responses, embeddings, min_response = 2)

  expect_true(is.na(result$mean_flow$FF[result$mean_flow$ID == "P1"]))
  expect_true(is.na(result$response_flow$P1))

})

test_that("forward_flow's min_response/max_response actually apply to the fluency path", {

  embeddings <- make_embeddings()

  # P1 has only 2 non-NA responses -- below min_response = 3, must be NA
  # even though 2 responses would otherwise be enough to compute a flow value
  responses <- matrix(
    c("dog", "wolf", NA, "car", "truck", "bus"),
    nrow = 2, byrow = TRUE, dimnames = list(c("P1", "P2"), NULL)
  )

  result <- forward_flow(responses, embeddings, min_response = 3)

  expect_true(is.na(result$mean_flow$FF[result$mean_flow$ID == "P1"]))
  expect_false(is.na(result$mean_flow$FF[result$mean_flow$ID == "P2"]))

  # max_response = 2 truncates P2 to its first 2 responses ("car", "truck"),
  # so its flow should match calling forward_flow on that shorter sequence
  # directly rather than the full 3-response one
  truncated <- forward_flow(responses[, 1:2, drop = FALSE], embeddings, min_response = 2)
  capped <- forward_flow(responses, embeddings, min_response = 2, max_response = 2)

  expect_equal(
    capped$mean_flow$FF[capped$mean_flow$ID == "P2"],
    truncated$mean_flow$FF[truncated$mean_flow$ID == "P2"]
  )

})

test_that("forward_flow normalizes NaN (all-unmatched vocabulary) to NA", {

  embeddings <- make_embeddings()

  responses <- matrix(c("spaceship", "alien", "robot"), nrow = 1, dimnames = list("P1", NULL))
  result <- forward_flow(responses, embeddings, min_response = 2)

  expect_true(is.na(result$mean_flow$FF))
  expect_false(is.nan(result$mean_flow$FF))
  expect_true(all(is.na(result$response_flow$P1)))
  expect_false(any(is.nan(result$response_flow$P1)))

})

test_that("forward_flow excludes unmatched vocabulary instead of propagating NA everywhere", {

  embeddings <- make_embeddings()

  responses <- matrix(
    c("dog", "spaceship", "wolf"),
    nrow = 1, dimnames = list("P1", NULL)
  )

  result <- forward_flow(responses, embeddings, min_response = 2)

  # "spaceship" has no embedding row, but "wolf" vs. "dog" alone should still
  # produce a real (non-NA) distance for the final response
  expect_false(is.na(result$response_flow$P1[2]))

})

test_that("forward_flow computes free-association mean/response flow per participant-cue", {

  embeddings <- make_embeddings()

  response_matrix <- data.frame(
    ID = c("P1", "P1", "P1", "P2", "P2", "P2"),
    Cue = c("animals", "animals", "animals", "vehicles", "vehicles", "vehicles"),
    Response = c("dog", "wolf", "cat", "car", "truck", "bus"),
    stringsAsFactors = FALSE
  )

  result <- forward_flow(response_matrix, embeddings, min_response = 2, type = "free")

  expect_true(is.data.frame(result$mean_flow))
  expect_true(is.data.frame(result$mean_response_flow))
  expect_equal(sort(result$mean_flow$ID), c("P1", "P2"))
  expect_equal(result$response_flow$P1$animals, result$response_flow$P1[["animals"]])
  expect_length(result$response_flow$P1$animals, 2)

})

test_that("forward_flow's free-association participants with no qualifying cue still appear (as NA)", {

  embeddings <- make_embeddings()

  response_matrix <- data.frame(
    ID = c("P1", "P2", "P2"),
    Cue = c("animals", "vehicles", "vehicles"),
    Response = c("dog", "car", "truck"),
    stringsAsFactors = FALSE
  )

  result <- forward_flow(response_matrix, embeddings, min_response = 2, type = "free")

  expect_true("P1" %in% result$mean_flow$ID)
  expect_true(is.na(result$mean_flow$FF[result$mean_flow$ID == "P1"]))
  expect_length(result$response_flow$P1, 0)

})

test_that("forward_flow_free's split()-based grouping matches a naive re-scan reference", {

  # Guards the grouping-loop optimization: split() must group rows into
  # exactly the same participant-cue sequences (same order, same content) as
  # a naive `response_matrix$ID == id` re-scan would
  set.seed(2)
  embeddings <- make_embeddings()
  words <- rownames(embeddings)

  ids <- paste0("P", 1:8)
  cues <- c("animals", "vehicles")

  response_matrix <- do.call(rbind, lapply(ids, function(id){
    do.call(rbind, lapply(cues, function(cue){
      data.frame(
        ID = id, Cue = cue,
        Response = sample(words, sample(1:4, 1), replace = TRUE),
        stringsAsFactors = FALSE
      )
    }))
  }))

  optimized <- forward_flow(response_matrix, embeddings, min_response = 2, type = "free")

  naive_groups <- list()
  for(id in unique(response_matrix$ID)){
    participant <- response_matrix[response_matrix$ID == id, , drop = FALSE]
    for(cue in unique(participant$Cue)){
      responses <- participant$Response[participant$Cue == cue]
      if(length(responses) >= 2){
        naive_groups[[paste(id, cue)]] <- response_flow(responses, embeddings)
      }
    }
  }

  for(key in names(naive_groups)){
    id <- strsplit(key, " ")[[1]][1]
    cue <- strsplit(key, " ")[[1]][2]
    expect_equal(optimized$response_flow[[id]][[cue]], naive_groups[[key]]$flow)
  }

})

test_that("forward_flow validates its input", {

  embeddings <- make_embeddings()
  responses <- matrix(c("dog", "wolf", "cat"), nrow = 1, dimnames = list("P1", NULL))

  no_names <- embeddings
  row.names(no_names) <- NULL
  expect_error(forward_flow(responses, no_names), "row names")

  expect_error(forward_flow(responses, embeddings, cores = 0), "forward_flow")
  expect_error(forward_flow(responses, embeddings, min_response = -1), "forward_flow")

  expect_error(
    forward_flow(data.frame(ID = 1, Response = "dog"), embeddings, type = "free"),
    "\"ID\", \"Cue\", and \"Response\""
  )

})
