# Tests for plot.permutation_SemNeT()

# Make fake permutation ----
# Updated 22.10.2026
make_plot_perm <- function(measures = c("aspl", "cc", "q"), observed = .5)
{

  # Set seed
  set.seed(1)

  # Get null differences (first is the observed difference)
  differences <- sapply(measures, function(measure) c(observed, rnorm(1999, sd = .2)))
  differences <- matrix(differences, ncol = length(measures), dimnames = list(NULL, measures))

  # Set up result
  result <- data.frame(
    low = rep(3, length(measures)), high = rep(2.5, length(measures)),
    p_value = vapply(seq_along(measures), function(i) mean(abs(differences[, i]) >= abs(observed)), numeric(1)),
    alternative = "low != high", row.names = measures, stringsAsFactors = FALSE
  )

  # Set up output
  output <- list(
    result = result, networks = NULL, method = "tmfg", seed = 1,
    differences = if(length(measures) == 1) unname(differences[, 1]) else differences,
    diagnostics = NULL, alpha = .05
  )

  # Set class
  class(output) <- "permutation_SemNeT"

  # Return output
  return(output)

}

# Get layer data of plot
# Updated 22.10.2026
plot_layer_data <- function(plot, geom)
{

  # Find the layer
  layer <- Filter(function(layer) inherits(layer$geom, geom), plot$layers)

  # Return its data
  if(length(layer) == 0) NULL else layer[[1]]$data

}

test_that("plot.permutation_SemNeT draws one panel per measure with the p-value, null range, and observed difference", {

  skip_if_not_installed("ggplot2")

  # Get object
  perm <- make_plot_perm()

  # Get plot
  plot <- plot(perm)

  # Check the plot builds
  expect_s3_class(ggplot2::ggplot_build(plot), "ggplot_built")

  # Check panel labels include p-values
  labels <- levels(plot$data$measure)
  expect_length(labels, 3)
  expect_true(all(grepl("p [=<]", labels)))
  expect_match(labels[1], "^ASPL")

  # Get range data
  range_data <- plot_layer_data(plot, "GeomErrorbar")

  # Check range of first measure
  expect_equal(
    c(range_data$lower[1], range_data$upper[1]),
    unname(quantile(perm$differences[, "aspl"], c(.025, .975)))
  )

  # Check observed difference is drawn
  observed_layer <- Filter(function(layer) identical(layer$aes_params$shape, 23), plot$layers)
  expect_length(observed_layer, 1)
  expect_equal(observed_layer[[1]]$data$observed, rep(.5, 3))

})

test_that("plot.permutation_SemNeT handles a single measure and a subset of measures", {

  skip_if_not_installed("ggplot2")

  # Check single measure (vector of differences)
  single <- plot(make_plot_perm("q"))
  expect_equal(nlevels(single$data$measure), 1)
  expect_s3_class(ggplot2::ggplot_build(single), "ggplot_built")

  # Check subset of measures
  subset <- plot(make_plot_perm(), measures = c("cc", "q"))
  expect_equal(nlevels(subset$data$measure), 2)
  expect_match(levels(subset$data$measure)[1], "^CC")

})

test_that("plot.permutation_SemNeT's level changes the null range and null_range = FALSE drops it", {

  skip_if_not_installed("ggplot2")

  # Get ranges
  narrow <- plot_layer_data(plot(make_plot_perm(), level = .5), "GeomErrorbar")
  wide <- plot_layer_data(plot(make_plot_perm(), level = .99), "GeomErrorbar")

  # Check wider level widens the range
  expect_true(all(wide$upper - wide$lower > narrow$upper - narrow$lower))

  # Check no range layer
  expect_null(plot_layer_data(plot(make_plot_perm(), null_range = FALSE), "GeomErrorbar"))

})

test_that("plot.permutation_SemNeT validates its arguments", {

  skip_if_not_installed("ggplot2")

  # Get object
  perm <- make_plot_perm()

  # Check errors
  expect_error(plot(perm, measures = "foo"), "measures tested")
  expect_error(plot(make_plot_perm("aspl"), measures = "cc"), "measures tested")
  expect_error(plot(perm, measures = 1))
  expect_error(plot(perm, null_range = "yes"))
  expect_error(plot(perm, level = 1), "between 0 and 1")
  expect_error(plot(perm, level = 0), "between 0 and 1")
  expect_error(plot(perm, level = c(.9, .95)))

})
