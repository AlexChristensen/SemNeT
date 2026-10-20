# Tests for forward_flow()

# Make embeddings ----
# Updated 13.10.2026
make_embeddings <- function(){

  # Set words
  words <- c("dog", "cat", "wolf", "car", "truck", "bus")

  # Set seed
  set.seed(1)

  # Sample embeddings
  embeddings <- matrix(rnorm(length(words) * 5), nrow = length(words))

  # Name embeddings
  row.names(embeddings) <- words

  # Return embeddings
  return(embeddings)

}

test_that("forward_flow computes fluency mean/response flow per participant", {

  # Make embeddings
  embeddings <- make_embeddings()

  # Set up responses
  responses <- matrix(
    c("dog", "wolf", "cat", "car", "truck", "bus"),
    nrow = 2, byrow = TRUE, dimnames = list(c("P1", "P2"), NULL)
  )

  # Compute forward flow
  result <- forward_flow(responses, embeddings, min_response = 2)

  # Check for data frame
  expect_true(is.data.frame(result$mean_flow))

  # Check IDs
  expect_equal(result$mean_flow$ID, c("P1", "P2"))

  # Check names of flow
  expect_equal(names(result$response_flow), c("P1", "P2"))

  # Check length of flow
  expect_length(result$response_flow$P1, 2)

  # Check flow of second response
  expect_equal(unname(result$response_flow$P1[1]), 1 - cosine(embeddings["wolf", ], embeddings["dog", ]))

})

test_that("forward_flow returns NA flow for participants below min_response", {

  # Make embeddings
  embeddings <- make_embeddings()

  # Set up responses
  responses <- matrix(
    c("dog", NA, NA, "car", "truck", "bus"),
    nrow = 2, byrow = TRUE, dimnames = list(c("P1", "P2"), NULL)
  )

  # Compute forward flow
  result <- forward_flow(responses, embeddings, min_response = 2)

  # Check for missing mean flow
  expect_true(is.na(result$mean_flow$FF[result$mean_flow$ID == "P1"]))

  # Check for missing flow
  expect_true(is.na(result$response_flow$P1))

})

test_that("forward_flow's min_response/max_response actually apply to the fluency path", {

  # Make embeddings
  embeddings <- make_embeddings()

  # Set up responses
  responses <- matrix(
    c("dog", "wolf", NA, "car", "truck", "bus"),
    nrow = 2, byrow = TRUE, dimnames = list(c("P1", "P2"), NULL)
  )

  # Compute forward flow
  result <- forward_flow(responses, embeddings, min_response = 3)

  # Check for missing flow of first participant
  expect_true(is.na(result$mean_flow$FF[result$mean_flow$ID == "P1"]))

  # Check for flow of second participant
  expect_false(is.na(result$mean_flow$FF[result$mean_flow$ID == "P2"]))

  # Compute forward flow of shorter sequence
  truncated <- forward_flow(responses[, 1:2, drop = FALSE], embeddings, min_response = 2)

  # Compute forward flow with maximum responses
  capped <- forward_flow(responses, embeddings, min_response = 2, max_response = 2)

  # Check flow of second participant
  expect_equal(
    capped$mean_flow$FF[capped$mean_flow$ID == "P2"],
    truncated$mean_flow$FF[truncated$mean_flow$ID == "P2"]
  )

})

test_that("forward_flow normalizes NaN (all-unmatched vocabulary) to NA", {

  # Make embeddings
  embeddings <- make_embeddings()

  # Set up responses
  responses <- matrix(c("spaceship", "alien", "robot"), nrow = 1, dimnames = list("P1", NULL))

  # Compute forward flow
  result <- forward_flow(responses, embeddings, min_response = 2)

  # Check for missing mean flow
  expect_true(is.na(result$mean_flow$FF))

  # Check for no undefined mean flow
  expect_false(is.nan(result$mean_flow$FF))

  # Check for missing flow
  expect_true(all(is.na(result$response_flow$P1)))

  # Check for no undefined flow
  expect_false(any(is.nan(result$response_flow$P1)))

})

test_that("forward_flow excludes unmatched vocabulary instead of propagating NA everywhere", {

  # Make embeddings
  embeddings <- make_embeddings()

  # Set up responses
  responses <- matrix(
    c("dog", "spaceship", "wolf"),
    nrow = 1, dimnames = list("P1", NULL)
  )

  # Compute forward flow
  result <- forward_flow(responses, embeddings, min_response = 2)

  # Check for flow of last response
  expect_false(is.na(result$response_flow$P1[2]))

})

test_that("forward_flow computes free-association mean/response flow per participant-cue", {

  # Make embeddings
  embeddings <- make_embeddings()

  # Set up responses
  response_matrix <- data.frame(
    ID = c("P1", "P1", "P1", "P2", "P2", "P2"),
    Cue = c("animals", "animals", "animals", "vehicles", "vehicles", "vehicles"),
    Response = c("dog", "wolf", "cat", "car", "truck", "bus"),
    stringsAsFactors = FALSE
  )

  # Compute forward flow
  result <- forward_flow(response_matrix, embeddings, min_response = 2, type = "free")

  # Check mean flow
  expect_true(is.data.frame(result$mean_flow))

  # Check mean flow of cues
  expect_true(is.data.frame(result$mean_response_flow))

  # Check IDs
  expect_equal(sort(result$mean_flow$ID), c("P1", "P2"))

  # Check access to flow
  expect_equal(result$response_flow$P1$animals, result$response_flow$P1[["animals"]])

  # Check length of flow
  expect_length(result$response_flow$P1$animals, 2)

})

test_that("forward_flow's free-association participants with no qualifying cue still appear (as NA)", {

  # Make embeddings
  embeddings <- make_embeddings()

  # Set up responses
  response_matrix <- data.frame(
    ID = c("P1", "P2", "P2"),
    Cue = c("animals", "vehicles", "vehicles"),
    Response = c("dog", "car", "truck"),
    stringsAsFactors = FALSE
  )

  # Compute forward flow
  result <- forward_flow(response_matrix, embeddings, min_response = 2, type = "free")

  # Check for participant
  expect_true("P1" %in% result$mean_flow$ID)

  # Check for missing flow
  expect_true(is.na(result$mean_flow$FF[result$mean_flow$ID == "P1"]))

  # Check for no flow of cues
  expect_length(result$response_flow$P1, 0)

})

test_that("forward_flow_free's split()-based grouping matches a naive re-scan reference", {

  # Set seed
  set.seed(2)

  # Make embeddings
  embeddings <- make_embeddings()

  # Get words
  words <- rownames(embeddings)

  # Set IDs
  ids <- paste0("P", 1:8)

  # Set cues
  cues <- c("animals", "vehicles")

  # Sample responses
  response_matrix <- do.call(rbind, lapply(ids, function(id){

    # Loop over cues
    do.call(rbind, lapply(cues, function(cue){

      # Sample responses to cue
      data.frame(
        ID = id, Cue = cue,
        Response = sample(words, sample(1:4, 1), replace = TRUE),
        stringsAsFactors = FALSE
      )

    }))

  }))

  # Compute forward flow
  optimized <- forward_flow(response_matrix, embeddings, min_response = 2, type = "free")

  # Initialize naive groups
  naive_groups <- list()

  # Loop over participants
  for(id in unique(response_matrix$ID)){

    # Get rows of participant
    participant <- response_matrix[response_matrix$ID == id, , drop = FALSE]

    # Loop over cues
    for(cue in unique(participant$Cue)){

      # Get responses to cue
      responses <- participant$Response[participant$Cue == cue]

      # Check for enough responses
      if(length(responses) >= 2){

        # Compute flow
        naive_groups[[paste(id, cue)]] <- response_flow(responses, embeddings)

      }

    }

  }

  # Loop over groups
  for(key in names(naive_groups)){

    # Get ID
    id <- strsplit(key, " ")[[1]][1]

    # Get cue
    cue <- strsplit(key, " ")[[1]][2]

    # Check flow
    expect_equal(optimized$response_flow[[id]][[cue]], naive_groups[[key]]$flow)

  }

})

test_that("forward_flow_free handles non-sequential numeric IDs correctly", {

  # Make embeddings
  embeddings <- make_embeddings()

  # Set up responses
  response_matrix <- data.frame(
    ID = c(7, 7, 7, 12, 12, 12, 20, 20, 20),
    Cue = "animals",
    Response = c("dog", "wolf", "cat", "car", "truck", "bus", "dog", "wolf", "cat"),
    stringsAsFactors = FALSE
  )

  # Compute forward flow
  result <- forward_flow(response_matrix, embeddings, min_response = 2, type = "free")

  # Check IDs
  expect_setequal(result$mean_flow$ID, c(7, 12, 20))

  # Check names of flow
  expect_setequal(names(result$response_flow), c("7", "12", "20"))

  # Check that identical responses give identical flow
  expect_equal(result$response_flow[["7"]]$animals, result$response_flow[["20"]]$animals)

  # Check that different responses give different flow
  expect_false(isTRUE(all.equal(result$response_flow[["7"]]$animals, result$response_flow[["12"]]$animals)))

})

test_that("forward_flow treats blank-like placeholders as missing and validates its inputs", {

  # Make embeddings
  embeddings <- make_embeddings()

  # Set up clean responses
  clean <- matrix(c("dog", "wolf", "cat", NA, NA), nrow = 1, dimnames = list("P1", NULL))

  # Set up padded responses
  padded <- matrix(c("dog", "wolf", "cat", " ", "NA"), nrow = 1, dimnames = list("P1", NULL))

  # Check that placeholders are missing
  expect_equal(
    forward_flow(padded, embeddings, min_response = 3)$mean_flow,
    forward_flow(clean, embeddings, min_response = 3)$mean_flow
  )

  # Check for error with one response
  expect_error(forward_flow(clean, embeddings, min_response = 1), "forward_flow")

  # Add duplicate term
  ambiguous <- rbind(embeddings, embeddings["dog", , drop = FALSE])

  # Name duplicate term
  row.names(ambiguous)[7] <- "Dog"

  # Check for error with ambiguous terms
  expect_error(forward_flow(clean, ambiguous, min_response = 2), "lower-cased")

})

test_that("forward_flow free association treats placeholders as missing and keeps participant order", {

  # Make embeddings
  embeddings <- make_embeddings()

  # Set up clean and padded responses
  clean <- data.frame(
    ID = c("b", "b", "b", "a", "a", "a"), Cue = "animals",
    Response = c("dog", "wolf", "cat", "car", "truck", "bus"), stringsAsFactors = FALSE
  )
  padded <- data.frame(
    ID = c("b", "b", "b", "b", "b", "a", "a", "a"), Cue = "animals",
    Response = c("dog", "", "wolf", "NA", "cat", "car", "truck", "bus"), stringsAsFactors = FALSE
  )

  # Compute forward flow
  clean_result <- forward_flow(clean, embeddings, min_response = 3, type = "free")
  padded_result <- forward_flow(padded, embeddings, min_response = 3, type = "free")

  # Check that placeholders are missing
  expect_equal(padded_result$mean_flow, clean_result$mean_flow)

  # Check that participants keep their order
  expect_identical(padded_result$mean_flow$ID, c("b", "a"))

  # Check that placeholders do not count toward the minimum
  short <- data.frame(ID = "a", Cue = "animals", Response = c("dog", "", "NA"), stringsAsFactors = FALSE)
  expect_true(is.na(forward_flow(short, embeddings, min_response = 2, type = "free")$mean_flow$FF))

  # Check for error with missing embedding values
  broken <- embeddings
  broken[1, 1] <- NA
  expect_error(forward_flow(clean, broken, min_response = 3, type = "free"), "missing or infinite")

})
