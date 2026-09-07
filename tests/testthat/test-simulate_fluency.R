# Tests for simulate_fluency()

test_that("simulate_fluency returns a binary matrix of the requested shape", {

  result <- simulate_fluency(nodes = 20, cases = 50)

  expect_equal(dim(result), c(50, 20))
  expect_true(all(result %in% c(0, 1)))

})

test_that("simulate_fluency never leaves a node with zero responses", {

  set.seed(1)
  result <- simulate_fluency(nodes = 30, cases = 40)

  expect_true(all(colSums(result) > 0))

})

test_that("simulate_fluency matches a fixed-seed reference from the original algorithm", {

  orig_sim_fluency <- function(nodes, cases, random = FALSE) {
    samp.freq <- if(random) sample(SemNeT::animals.freq, size = nodes, replace = TRUE) else SemNeT::animals.freq
    sum.resp <- rpois(nodes, samp.freq) + 1
    props <- sum.resp / (max(sum.resp) * 1.05)
    resp.mat <- matrix(0, nrow = cases, ncol = nodes)
    for(i in 1:length(props)) {
      resp.mat[, i] <- sample(c(0, 1), cases, replace = TRUE, prob = c(1 - props[i], props[i]))
    }
    if(any(colSums(resp.mat) == 0)) {
      target <- which(colSums(resp.mat) == 0)
      for(i in 1:length(target)) resp.mat[sample(cases, 1), target[i]] <- 1
    }
    resp.mat
  }

  set.seed(42)
  old_result <- orig_sim_fluency(nodes = 50, cases = 100)

  set.seed(42)
  new_result <- simulate_fluency(nodes = 50, cases = 100)

  expect_identical(old_result, new_result)

})

test_that("simulate_fluency errors when nodes exceeds available frequencies without random = TRUE", {

  expect_error(simulate_fluency(nodes = 400), "exceeds the number of available")

  # random = TRUE has no such limit
  expect_no_error(simulate_fluency(nodes = 400, cases = 20, random = TRUE))

})

test_that("simulate_fluency validates its input", {

  expect_error(simulate_fluency(nodes = 0), "simulate_fluency")
  expect_error(simulate_fluency(cases = -5), "simulate_fluency")
  expect_error(simulate_fluency(random = "yes"), "simulate_fluency")

})
