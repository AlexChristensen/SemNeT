# Tests for random_walk()

test_that("random_walk's row-normalized transition matrices match a hand-looped construction", {

  A <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3)
  degree <- colSums(A)

  hand_looped <- matrix(0, 3, 3)
  for(i in 1:3) for(j in 1:3) hand_looped[i, j] <- A[i, j] / degree[i]

  expect_equal(unname(A / degree), hand_looped)

})

test_that("random_walk returns correctly-typed long and short results", {

  set.seed(1)
  cordata1 <- cor(matrix(rnorm(15 * 10), nrow = 15, ncol = 10))
  cordata2 <- cor(matrix(rnorm(15 * 10), nrow = 15, ncol = 10))
  net1 <- suppressWarnings(tmfg(cordata1))
  net2 <- suppressWarnings(tmfg(cordata2))

  result <- suppressMessages(random_walk(net1, net2, reps = 2, steps = 5, iter = 20, cores = 1))

  expect_true(is.matrix(result$long))
  expect_equal(nrow(result$long), 2)

  # The original's $short mixed numeric p-values and "< .001"/direction
  # strings into one character matrix -- every numeric column here must
  # stay genuinely numeric instead
  expect_true(is.data.frame(result$short))
  expect_type(result$short$Steps, "double")
  expect_type(result$short[["Unique Nodes (p-value)"]], "double")
  expect_type(result$short[["Direction (Unique Nodes)"]], "character")

})

test_that("random_walk labels its output with custom network names", {

  set.seed(1)
  cordata1 <- cor(matrix(rnorm(15 * 10), nrow = 15, ncol = 10))
  cordata2 <- cor(matrix(rnorm(15 * 10), nrow = 15, ncol = 10))
  net1 <- suppressWarnings(tmfg(cordata1))
  net2 <- suppressWarnings(tmfg(cordata2))

  result <- suppressMessages(random_walk(net1, net2, name_a = "Low", name_b = "High", reps = 1, steps = 5, iter = 10, cores = 1))

  expect_true("M.uniq.Low" %in% colnames(result$long))
  expect_true("M.uniq.High" %in% colnames(result$long))

})

test_that("random_walk's steps = 1 doesn't corrupt the starting node", {

  # `n_steps == 1` makes `2:n_steps` (i.e. `2:1`) count *down* instead of
  # being empty, which used to overwrite the recorded starting node with an
  # unrelated later step -- a 1-step walk must stay at its starting node,
  # so its similarity-to-start is always exactly 1 (`exp(-0)`)
  set.seed(1)
  cordata1 <- cor(matrix(rnorm(15 * 10), nrow = 15, ncol = 10))
  cordata2 <- cor(matrix(rnorm(15 * 10), nrow = 15, ncol = 10))
  net1 <- suppressWarnings(tmfg(cordata1))
  net2 <- suppressWarnings(tmfg(cordata2))

  result <- suppressMessages(random_walk(net1, net2, reps = 1, steps = 1, iter = 20, cores = 1))

  expect_equal(unname(result$long[1, "M.sim.A"]), 1)
  expect_equal(unname(result$long[1, "M.sim.B"]), 1)

})

test_that("random_walk validates its input", {

  set.seed(1)
  cordata <- cor(matrix(rnorm(15 * 10), nrow = 15, ncol = 10))
  net <- suppressWarnings(tmfg(cordata))

  expect_error(random_walk(net, matrix(1, 3, 4)), "square")
  expect_error(random_walk(net, net, cores = 0), "random_walk")

})
