# Tests for bootstrap_SemNeT()

test_that("bootstrap_SemNeT's tmfg case-resampling equates every group to the same nodes each iteration", {

  # Set seed
  set.seed(3)

  # Get low group
  low <- open.clean[which(open.group == "Low"), ]

  # Get high group
  high <- open.clean[which(open.group == "High"), ]

  # Perform bootstrap
  result <- suppressMessages(
    bootstrap_SemNeT(low = low, high = high, method = "tmfg", type = "case", iter = 6, cores = 1, keep_networks = TRUE)
  )

  # Compare nodes of groups in each iteration
  matched <- vapply(seq_len(6), function(i){
    ncol(result$groups$low$networks[[i]]) == ncol(result$groups$high$networks[[i]])
  }, logical(1))

  # Check for matched nodes
  expect_true(all(matched))

})

test_that("bootstrap_SemNeT's node-drop type keeps the requested proportion of nodes", {

  # Set seed
  set.seed(4)

  # Get low group
  low <- finalize(open.binary[which(open.group == "Low"), ], 2)

  # Get high group
  high <- finalize(open.binary[which(open.group == "High"), ], 2)

  # Equate groups
  equated <- equate(low = low, high = high)

  # Perform bootstrap
  result <- suppressMessages(
    bootstrap_SemNeT(low = equated$low, high = equated$high, method = "tmfg", type = "node", prop = .5, iter = 5, cores = 1, keep_networks = TRUE)
  )

  # Check proportion
  expect_equal(result$meta$prop, .5)

  # Check number of nodes
  expect_true(all(vapply(result$groups$low$networks, ncol, numeric(1)) == floor(ncol(equated$low) * .5)))

})

test_that("bootstrap_SemNeT stores each group's observed (full-data) measures and per-iteration node counts", {

  # Set seed
  set.seed(1)

  # Get low group
  low <- open.clean[which(open.group == "Low"), ]

  # Get high group
  high <- open.clean[which(open.group == "High"), ]

  # Perform bootstrap
  result <- suppressMessages(bootstrap_SemNeT(low = low, high = high, method = "nrw", type = "case", iter = 6, cores = 1))

  # Check number of node counts
  expect_length(result$groups$low$nodes, 6)

  # Check observed results
  expect_named(result$groups$low$observed, c("measures", "edges", "nodes"))

  # Check observed measures
  expect_named(result$groups$low$observed$measures, c("aspl", "cc", "q"))

  # Estimate network of full data
  direct <- nrw(low)

  # Check observed ASPL
  expect_equal(unname(result$groups$low$observed$measures["aspl"]), as.numeric(aspl(direct)))

  # Check observed clustering
  expect_equal(unname(result$groups$low$observed$measures["cc"]), as.numeric(cc(direct)))

  # Check observed nodes
  expect_equal(result$groups$low$observed$nodes, nrow(direct))

  # Check observed edges
  expect_equal(result$groups$low$observed$edges, unname(network_counts(direct)["edges"]))

})

test_that("bootstrap_SemNeT's observed statistics follow the same finalize + equate pipeline as tmfg case bootstraps", {

  # Set seed
  set.seed(1)

  # Get low group
  low <- open.clean[which(open.group == "Low"), ]

  # Get high group
  high <- open.clean[which(open.group == "High"), ]

  # Perform bootstrap
  result <- suppressMessages(bootstrap_SemNeT(low = low, high = high, method = "tmfg", type = "case", iter = 4, cores = 1))

  # Check that nodes match
  expect_equal(result$groups$low$observed$nodes, result$groups$high$observed$nodes)

  # Check for no missing measures
  expect_false(anyNA(result$groups$low$observed$measures))

})

test_that("bootstrap_SemNeT is reproducible given a seed, regardless of cores", {

  # Get low group
  low <- open.clean[which(open.group == "Low"), ]

  # Get high group
  high <- open.clean[which(open.group == "High"), ]

  # Set up bootstrap
  run <- function(seed, cores = 1, ...){
    suppressMessages(bootstrap_SemNeT(low = low, high = high, iter = 8, cores = cores, seed = seed, ...))
  }

  # Perform bootstrap
  one <- run(1)

  # Check measures with same seed
  expect_identical(one$groups$low$measures, run(1)$groups$low$measures)

  # Check edges with same seed
  expect_identical(one$groups$high$edges, run(1)$groups$high$edges)

  # Check measures with more cores
  expect_identical(one$groups$low$measures, run(1, cores = 2)$groups$low$measures)

  # Check measures with different seed
  expect_false(identical(one$groups$low$measures, run(2)$groups$low$measures))

  # Check stored seed
  expect_identical(one$meta$seed, 1)

  # Set seed
  set.seed(99)

  # Check independence of R's random state
  expect_identical(one$groups$low$measures, run(1)$groups$low$measures)

  # Get response order
  ordered <- responses2binary(open.clean)$order

  # Set up node bootstrap
  nodes <- function(seed){
    suppressMessages(bootstrap_SemNeT(
      low = ordered[which(open.group == "Low"), 1:40], high = ordered[which(open.group == "High"), 1:40],
      method = "pf", type = "node", iter = 6, seed = seed
    ))
  }

  # Check node bootstrap with same seed
  expect_identical(nodes(3)$groups$low$measures, nodes(3)$groups$low$measures)

  # Check node bootstrap with different seed
  expect_false(identical(nodes(3)$groups$low$measures, nodes(4)$groups$low$measures))

  # Check message without seed
  expect_message(
    bootstrap_SemNeT(low = low, iter = 2, method = "pf"), "not be reproducible"
  )

})

test_that("node bootstrap drops the same nodes from every group in an iteration", {

  # Set seed
  set.seed(1)

  # Make group
  make_group <- function(){

    # Sample responses
    group <- matrix(rbinom(60 * 24, 1, .35), nrow = 60, ncol = 24)

    # Name responses
    colnames(group) <- paste0("word", 1:24)

    # Return group
    group

  }

  # Make group with response order
  make_ordered_group <- function(){

    # Get group
    group <- make_group()

    # Loop over participants
    for(i in seq_len(nrow(group))){

      # Get responses given
      said <- which(group[i, ] == 1)

      # Order responses
      group[i, said] <- sample(seq_along(said))

    }

    # Return group
    group

  }

  # Perform bootstrap
  result <- suppressMessages(bootstrap_SemNeT(
    a = make_group(), b = make_group(), method = "tmfg", type = "node", prop = .5,
    iter = 6, keep_networks = TRUE, seed = 1
  ))

  # Loop over iterations
  for(i in 1:6){

    # Check that nodes match
    expect_identical(colnames(result$groups$a$networks[[i]]), colnames(result$groups$b$networks[[i]]))

  }

  # Check that iterations drop different nodes
  expect_gt(length(unique(lapply(result$groups$a$networks, colnames))), 1)

  # Make reordered group
  reordered <- make_ordered_group()

  # Check that reordered nodes are matched by name
  expect_no_error(suppressMessages(bootstrap_SemNeT(
    a = make_ordered_group(), b = reordered[, rev(colnames(reordered))], method = "pf", type = "node", iter = 2, seed = 1
  )))

  # Make group with other nodes
  other <- make_ordered_group()

  # Rename nodes
  colnames(other) <- paste0("other", 1:24)

  # Check for error with different nodes
  expect_error(
    bootstrap_SemNeT(a = make_ordered_group(), b = other, method = "pf", type = "node", iter = 2, seed = 1),
    "same nodes"
  )

})

test_that("only correlation-type similarities retry bootstrap draws, and an always-given response fails fast", {

  # Check cosine
  expect_false(needs_variation("tmfg", "cosine"))

  # Check correlation
  expect_true(needs_variation("tmfg", "cor"))

  # Check phi
  expect_true(needs_variation("tmfg", "phi"))

  # Check other method
  expect_false(needs_variation("cn", "cor"))

  # Set up data
  data <- cbind(rep(1, 6), c(1, 0, 1, 0, 1, 0))

  # Check sample without retry
  expect_identical(
    generate_bootstrap_sample(data, seed = 5, retry = FALSE, iter = 3),
    data[shuffle_replace(seq_len(6), seed = 5), , drop = FALSE]
  )

  # Set up data
  everyone <- matrix(rbinom(40 * 6, 1, .5), 40, 6)

  # Add response given by everyone
  everyone[, 2] <- 1

  # Name responses
  colnames(everyone) <- paste0("w", 1:6)

  # Check for error with correlation
  expect_error(
    bootstrap_SemNeT(a = everyone, b = everyone, sim = "cor", iter = 2, type = "node", seed = 1),
    "every participant"
  )

  # Check for no error with cosine
  expect_no_error(suppressWarnings(suppressMessages(
    bootstrap_SemNeT(a = everyone, b = everyone, sim = "cosine", iter = 2, type = "node", prop = .8, seed = 1)
  )))

})

test_that("node bootstrap of raw responses drops words, not response positions", {

  # Get raw responses
  raw <- open.clean[1:30, ]

  # Bootstrap nodes of identical vocabularies
  result <- suppressMessages(bootstrap_SemNeT(
    a = raw, b = raw, method = "nrw", type = "node", prop = .5, iter = 3, keep_networks = TRUE, seed = 1
  ))

  # Get vocabulary
  vocabulary <- colnames(responses2binary(raw)$order)

  # Check that the nodes of every network are words of the vocabulary
  nodes <- unlist(lapply(result$groups$a$networks, rownames))
  expect_true(all(nodes %in% vocabulary))

  # Check that at most half of the vocabulary is kept
  expect_true(all(result$groups$a$nodes <= floor(length(vocabulary) * .5)))

})
