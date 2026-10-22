# Tests for plot.bootstrap_SemNeT()

# Make fake bootstrap ----
# Updated 22.10.2026
make_plot_boot <- function(type = "case", observed = TRUE)
{

  # Set seed
  set.seed(1)

  # Set values of groups
  values <- list(low = rnorm(2000, mean = 2.0, sd = .1), high = rnorm(2000, mean = 2.3, sd = .1))

  # Loop over groups
  groups <- lapply(values, function(aspl){

    # Return group
    list(
      networks = NULL, measures = rbind(aspl = aspl, cc = aspl / 10, q = aspl / 100),
      edges = rep(10, length(aspl)), nodes = rep(20, length(aspl)),
      observed = if(observed){
        list(measures = c(aspl = median(aspl), cc = median(aspl) / 10, q = median(aspl) / 100), edges = 10, nodes = 20)
      },
      summary = NULL
    )

  })

  # Set up bootstrap
  boot <- list(groups = groups, meta = list(prop = NULL, iter = 2000, type = type, method = "nrw"))

  # Set class
  class(boot) <- "bootstrap_SemNeT"

  # Return bootstrap
  return(boot)

}

# Get interval data of plot
# Updated 22.10.2026
plot_ci_data <- function(plot)
{

  # Find the interval layer
  layer <- Filter(function(layer) inherits(layer$geom, "GeomErrorbar"), plot$layers)

  # Return its data
  if(length(layer) == 0) NULL else layer[[1]]$data

}

test_that("plot.bootstrap_SemNeT draws a normal interval around the observed value for each group and measure", {

  skip_if_not_installed("ggplot2")

  # Get plot
  plot <- plot(make_plot_boot())

  # Get interval data
  ci_data <- plot_ci_data(plot)

  # Check one interval per group and measure
  expect_equal(nrow(ci_data), 6)

  # Get ASPL of low group
  low <- ci_data[ci_data$group == "low" & ci_data$measure == "ASPL", ]

  # Get bootstrap values
  values <- make_plot_boot()$groups$low$measures["aspl", ]

  # Check interval is centred on the observed value with the bootstrap SE
  expect_equal(low$center, median(values))
  expect_equal(low$upper - low$lower, 2 * qnorm(.975) * sd(values))

  # Check the plot builds
  expect_s3_class(ggplot2::ggplot_build(plot), "ggplot_built")

})

test_that("plot.bootstrap_SemNeT's percentile interval uses the bootstrap percentiles and ci_level", {

  skip_if_not_installed("ggplot2")

  # Get interval data
  ci_data <- plot_ci_data(plot(make_plot_boot(), ci_type = "percentile", ci_level = .9))

  # Get bootstrap values
  values <- make_plot_boot()$groups$high$measures["aspl", ]

  # Get interval of high group
  high <- ci_data[ci_data$group == "high" & ci_data$measure == "ASPL", ]

  # Check interval
  expect_equal(c(high$lower, high$upper), unname(quantile(values, c(.05, .95))))

  # Check a wider level widens the interval
  wide <- plot_ci_data(plot(make_plot_boot(), ci_type = "percentile", ci_level = .99))
  expect_true(all(wide$upper - wide$lower > ci_data$upper - ci_data$lower))

})

test_that("plot.bootstrap_SemNeT falls back without observed values", {

  skip_if_not_installed("ggplot2")

  # Get interval data
  ci_data <- plot_ci_data(plot(make_plot_boot(observed = FALSE)))

  # Check intervals are still drawn around the bootstrap mean
  values <- make_plot_boot()$groups$low$measures["aspl", ]
  low <- ci_data[ci_data$group == "low" & ci_data$measure == "ASPL", ]
  expect_equal(low$center, mean(values))

  # Check bias-corrected falls back to percentile
  bc <- plot_ci_data(plot(make_plot_boot(observed = FALSE), ci_type = "bc"))
  percentile <- plot_ci_data(plot(make_plot_boot(observed = FALSE), ci_type = "percentile"))
  expect_equal(bc$lower, percentile$lower)

})

test_that("plot.bootstrap_SemNeT can drop the interval and skips it for node bootstraps", {

  skip_if_not_installed("ggplot2")

  # Check no interval layer without 'ci'
  expect_null(plot_ci_data(plot(make_plot_boot(), ci = FALSE)))

  # Check node bootstrap message and no interval
  expect_message(node <- plot(make_plot_boot(type = "node")), "no interval is drawn")
  expect_null(plot_ci_data(node))

})

test_that("plot.bootstrap_SemNeT validates its interval arguments", {

  skip_if_not_installed("ggplot2")

  # Get bootstrap
  boot <- make_plot_boot()

  # Check errors
  expect_error(plot(boot, ci = "yes"))
  expect_error(plot(boot, ci_level = 1), "between 0 and 1")
  expect_error(plot(boot, ci_level = 0), "between 0 and 1")
  expect_error(plot(boot, ci_level = c(.9, .95)))
  expect_error(plot(boot, ci_type = "wald"))

})

test_that("plot.bootstrap_SemNeT's intervals ignore non-finite bootstrap values", {

  skip_if_not_installed("ggplot2")

  # Get bootstrap
  boot <- make_plot_boot()

  # Set non-finite values
  boot$groups$low$measures["aspl", c(3, 7)] <- NaN

  # Loop over interval types
  for(type in c("normal", "bc", "percentile")){

    # Get interval data (the plot drops non-finite values with a warning when built)
    ci_data <- plot_ci_data(plot(boot, ci_type = type))

    # Check finite interval
    low <- ci_data[ci_data$group == "low" & ci_data$measure == "ASPL", ]
    expect_true(all(is.finite(c(low$lower, low$upper, low$center))), info = type)

  }

})
