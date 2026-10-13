# Tests for cn()

# Data with known co-occurrence ----
# Updated 13.10.2026
ab_data <- function(hits)
{

  # Initialize filler counter
  filler <- 0

  # Generate unique fillers
  next_filler <- function(n){

    # Get filler words
    out <- paste0("w", filler + seq_len(n))

    # Update filler counter
    filler <<- filler + n

    # Return filler words
    out

  }

  # Loop over participants
  rows <- lapply(seq_len(200), function(i){

    # Check for adjacent responses
    if(i <= hits){

      # Place words next to each other
      c("a", "b", next_filler(4))

    }else{

      # Place words apart
      c("a", next_filler(3), "b", next_filler(1))

    }

  })

  # Return data
  do.call(rbind, rows)

}

test_that("cn's enrich = TRUE option only adds edges, never removes them", {

  # Get data
  data <- open.clean[which(open.group == "Low"), ]

  # Estimate base network
  base_net <- cn(data)

  # Estimate enriched network
  enriched_net <- suppressWarnings(cn(data, enrich = TRUE))

  # Check dimensions
  expect_equal(dim(base_net), dim(enriched_net))

  # Check that edges are kept
  expect_true(all(base_net[base_net == 1] == enriched_net[base_net == 1]))

  # Check that edges are added
  expect_true(sum(enriched_net) >= sum(base_net))

})

test_that("cn's alpha is the level of the co-occurrence test (it used to have no effect)", {

  # Get data
  data <- open.clean[which(open.group == "Low"), ]

  # Estimate strict network
  strict <- cn(data, alpha = .001)

  # Estimate loose network
  loose <- cn(data, alpha = .2)

  # Check that looser alpha keeps more edges
  expect_gt(sum(loose), sum(strict))

  # Check that looser alpha keeps all nodes
  expect_true(all(rownames(strict) %in% rownames(loose)))

})

test_that("cn's co-occurrence test is Goni et al.'s two-sided Clopper-Pearson interval", {

  # Check interval for 4 hits
  expect_equal(binom.test(4, 200)$conf.int[1:2], c(0.0055, 0.0504), tolerance = 1e-3)

  # Check interval for 2 hits
  expect_equal(binom.test(2, 200)$conf.int[1:2], c(0.0012, 0.0357), tolerance = 1e-2)

  # Check interval for 91 hits
  expect_equal(binom.test(91, 200)$conf.int[1:2], c(0.38, 0.52), tolerance = 2e-2)

  # Estimate network below chance
  not_linked <- stat_cooccur(ab_data(132))

  # Estimate network above chance
  linked <- stat_cooccur(ab_data(136))

  # Check for no edge
  expect_equal(not_linked["a", "b"], 0)

  # Check for edge
  expect_equal(linked["a", "b"], 1)

})

test_that("cn's chance probability matches Goni et al. (2011)", {

  # Set mean number of responses
  N <- 31.57

  # Set window
  l <- 2

  # Check basal probability
  expect_equal((2 / (N * (N - 1))) * ((l * N) - (l * (l + 1) / 2)), 0.1246, tolerance = 1e-3)

})

test_that("cn handles whitespace and 'NA' placeholders like true missing responses", {

  # Get clean data
  clean <- ab_data(150)

  # Add placeholders
  padded <- cbind(clean, " ", "NA")

  # Check edge
  expect_equal(stat_cooccur(padded)["a", "b"], stat_cooccur(clean)["a", "b"])

  # Check nodes
  expect_identical(rownames(stat_cooccur(padded)), rownames(stat_cooccur(clean)))

})

test_that("cn handles a single participant and one-response participants", {

  # Set up single participant
  one <- matrix(c("dog", "cat", "cow"), nrow = 1)

  # Check for no edges
  expect_equal(unname(stat_cooccur(one)), matrix(0, 3, 3))

  # Set up one response each
  single_responses <- matrix(c("dog", "cat", "cow"), ncol = 1)

  # Check for error
  expect_error(stat_cooccur(single_responses), "more than one response")

})
