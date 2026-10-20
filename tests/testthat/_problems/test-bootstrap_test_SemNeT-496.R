# Extracted from test-bootstrap_test_SemNeT.R:496

# prequel ----------------------------------------------------------------------
make_fake_boot <- function(values, type = "case", observed = "median")
{

  # Loop over groups
  groups <- lapply(names(values), function(group_name){

    # Get ASPL of group
    aspl <- values[[group_name]]

    # Set up measures
    measures <- rbind(aspl = aspl, cc = aspl / 10, q = aspl / 100)

    # Get observed ASPL
    obs <- if(identical(observed, "median")){

      # Use median
      stats::median(aspl, na.rm = TRUE)

    }else if(is.list(observed)){

      # Use given value
      observed[[group_name]]

    }else{

      # Use no value
      NA

    }

    # Return group
    list(
      networks = NULL, measures = measures, edges = rep(10, length(aspl)), nodes = rep(20, length(aspl)),
      observed = if(is.na(obs)) NULL else list(measures = c(aspl = obs, cc = obs / 10, q = obs / 100), edges = 10, nodes = 20),
      summary = NULL
    )

  })

  # Name groups
  names(groups) <- names(values)

  # Set up bootstrap
  boot <- list(
    groups = groups,
    meta = list(prop = if(type == "node") .5 else NULL, iter = length(values[[1]]), type = type, method = "nrw")
  )

  # Set class
  class(boot) <- "bootstrap_SemNeT"

  # Return bootstrap
  return(boot)

}

# test -------------------------------------------------------------------------
set.seed(22)
a <- rnorm(1000, 2.0, .1)
b <- rnorm(1000, 2.1, .1)
boot <- make_fake_boot(list(a = a, b = b))
d <- a - b
table <- bootstrap_test_SemNeT(boot, measures = "aspl")$results$Case$aspl$table
expect_false("mde" %in% names(table))
expect_error(bootstrap_test_SemNeT(boot, measures = "aspl", power = .9), "unused argument")
