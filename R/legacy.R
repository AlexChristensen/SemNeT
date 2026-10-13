#' @noRd
# Legacy names of `...` ----
# Updated 07.09.2026
legacy_dot_names <- function(dots_call)
{

  # Get arguments of call
  args <- as.list(dots_call)[-1]

  # Check for no arguments
  if(length(args) == 0){

    # Return no names
    return(character(0))

  }

  # Get given names
  given_names <- names(dots_call)[-1]

  # Check for no names
  if(is.null(given_names)){

    # Set empty names
    given_names <- rep("", length(args))

  }

  # Get deparsed expressions
  deparsed <- vapply(args, function(arg) paste(deparse(arg), collapse = ""), character(1))

  # Return given names or expressions
  return(ifelse(nzchar(given_names), given_names, deparsed))

}

#' @rdname tmfg
#' @export
# Legacy `tmfg` ----
# Updated 07.09.2026
TMFG <- function(data, depend = FALSE)
{

  # Return network
  return(tmfg(data, depend = depend))

}

#' @rdname aspl
#' @export
# Legacy `aspl` ----
# Updated 07.09.2026
ASPL <- function(A, weighted = FALSE)
{

  # Return average shortest path length
  return(aspl(A, weighted = weighted))

}

#' @rdname cc
#' @export
# Legacy `cc` ----
# Updated 07.09.2026
CC <- function(A, weighted = FALSE)
{

  # Return clustering coefficient
  return(cc(A, weighted = weighted))

}

#' @rdname cn
#' @export
# Legacy `cn` ----
# Updated 07.09.2026
CN <- function(data, window = 2, alpha = .05, enrich = FALSE)
{

  # Return network
  return(cn(data, window = window, alpha = alpha, enrich = enrich))

}

#' @rdname nrw
#' @export
# Legacy `nrw` ----
# Updated 07.09.2026
NRW <- function(data, type = c("num", "prop"), threshold = 0)
{

  # Return network
  return(nrw(data, type = type, threshold = threshold))

}

#' @rdname pf
#' @export
# Legacy `pf` ----
# Updated 07.09.2026
PF <- function(data)
{

  # Return network
  return(pf(data))

}

#' @rdname q
#' @export
# Legacy `q` ----
# Updated 07.09.2026
Q <- function(A)
{

  # Return modularity
  return(q(A))

}

#' @title Simulate Verbal Fluency Data (Legacy)
#'
#' @description Backward-compatible wrapper for \code{\link{simulate_fluency}}
#' -- \code{sim.fluency()} was renamed \code{simulate_fluency()} in SemNeT 2.0.0
#'
#' @param nodes Numeric.
#' See \code{\link{simulate_fluency}}. Unlike \code{simulate_fluency()}, has
#' no default -- matches the original, which always required this argument
#'
#' @param cases Numeric.
#' See \code{\link{simulate_fluency}}. Unlike \code{simulate_fluency()}, has
#' no default -- matches the original, which always required this argument
#'
#' @param random Boolean.
#' See \code{\link{simulate_fluency}}
#'
#' @return See \code{\link{simulate_fluency}}
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Legacy `simulate_fluency` ----
# Updated 07.09.2026
sim.fluency <- function(nodes, cases, random = FALSE)
{

  # Return simulated data
  return(simulate_fluency(nodes = nodes, cases = cases, random = random))

}

#' @title Semantic Network Measures (Legacy)
#'
#' @description Backward-compatible wrapper for
#' \code{\link{semantic_network_measures}} -- \code{semnetmeas()} was renamed
#' \code{semantic_network_measures()} in SemNeT 2.0.0
#'
#' @param A Matrix or data frame.
#' See \code{\link{semantic_network_measures}}
#'
#' @param meas Character.
#' One or more of \code{"ASPL"}, \code{"CC"}, \code{"Q"}.
#' Defaults to all three
#'
#' @param weighted Boolean.
#' See \code{\link{semantic_network_measures}}
#'
#' @return A named numeric vector, named to match \code{meas} (uppercase),
#' as in the original
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Legacy `semantic_network_measures` ----
# Updated 07.09.2026
semnetmeas <- function(A, meas = c("ASPL", "CC", "Q"), weighted = FALSE)
{

  # Compute measures
  result <- semantic_network_measures(A, measures = tolower(meas), weighted = weighted)

  # Upper case names
  names(result) <- toupper(names(result))

  # Return measures
  return(result)

}

#' @title Response Pattern Analysis (Legacy)
#'
#' @description Backward-compatible wrapper for \code{\link{response_analysis}}
#' -- \code{response.analysis()} was renamed \code{response_analysis()} in
#' SemNeT 2.0.0
#'
#' @param ... Matrices or data frames.
#' Response matrices, e.g. \code{response.analysis(low, high)} -- names are
#' captured from the given expressions when not passed as named arguments,
#' matching the original's own \code{substitute()}-based name capture
#'
#' @return See \code{\link{response_analysis}}
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Legacy `response_analysis` ----
# Updated 07.09.2026
response.analysis <- function(...)
{

  # Get groups
  dots <- list(...)

  # Name groups
  names(dots) <- legacy_dot_names(substitute(list(...)))

  # Return analysis
  return(do.call(response_analysis, dots))

}

#' @title Random Walk Simulation (Legacy)
#'
#' @description Backward-compatible wrapper for \code{\link{random_walk}} --
#' \code{randwalk()} was renamed \code{random_walk()} in SemNeT 2.0.0.
#' \strong{Output changed:} the Wilcoxon \emph{p}-values are gone (they only
#' reflected how many walks were simulated) and \code{$short} is a data frame
#' of differences, Cohen's \emph{d}, and directions; see
#' \code{\link{random_walk}}
#'
#' @param A,B Matrix or data frame.
#' Two networks to compare -- names are captured from the given expressions,
#' matching the original's own \code{substitute()}-based name capture
#'
#' @param reps Numeric.
#' See \code{\link{random_walk}}
#'
#' @param steps Numeric.
#' See \code{\link{random_walk}}
#'
#' @param iter Numeric.
#' See \code{\link{random_walk}}
#'
#' @param cores Numeric.
#' See \code{\link{random_walk}}. Unlike the original, defaults to
#' sequential processing rather than auto-detecting cores -- matches
#' \code{random_walk()}'s own default and this rebuild's dependency trim
#' (no more direct \code{parallel::detectCores()} calls outside
#' \code{parallel_process()})
#'
#' @return See \code{\link{random_walk}}
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Legacy `random_walk` ----
# Updated 07.09.2026
randwalk <- function(A, B, reps = 20, steps = 10, iter = 10000, cores)
{

  # Get name of first network
  name_a <- deparse(substitute(A))

  # Get name of second network
  name_b <- deparse(substitute(B))

  # Check for `$` in first name
  if(grepl("$", name_a, fixed = TRUE)){

    # Keep last segment
    name_a <- utils::tail(strsplit(name_a, "$", fixed = TRUE)[[1]], 1)

  }

  # Check for `$` in second name
  if(grepl("$", name_b, fixed = TRUE)){

    # Keep last segment
    name_b <- utils::tail(strsplit(name_b, "$", fixed = TRUE)[[1]], 1)

  }

  # Set up arguments
  args <- list(A = A, B = B, name_a = name_a, name_b = name_b, reps = reps, steps = steps, iter = iter)

  # Check for cores
  if(!missing(cores)){

    # Add cores
    args$cores <- cores

  }

  # Return random walk
  return(do.call(random_walk, args))

}

#' @title Random Network Test (Legacy)
#'
#' @description Backward-compatible wrapper for
#' \code{\link{random_network_test}} -- \code{randnet.test()} was renamed
#' \code{random_network_test()} in SemNeT 2.0.0.
#' \strong{Output changed:} the \emph{p}-values are now Monte Carlo
#' \emph{p}-values, computed for Erdos-Renyi and degree-preserving random
#' networks (\code{p_value}, \code{p_value_degree}, ...), and a network's edge
#' count is the number of nonzero cells (weights are no longer summed); see
#' \code{\link{random_network_test}}
#'
#' @param ... Matrices or data frames.
#' Networks to test, e.g. \code{randnet.test(low, high)} -- names are
#' captured from the given expressions when not passed as named arguments,
#' matching the original's own \code{substitute()}-based name capture
#'
#' @param iter Numeric.
#' See \code{\link{random_network_test}}
#'
#' @param cores Numeric.
#' See \code{\link{random_network_test}}. Unlike the original, defaults to
#' sequential processing rather than auto-detecting cores (see
#' \code{\link{randwalk}}'s equivalent note)
#'
#' @return See \code{\link{random_network_test}}
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Legacy `random_network_test` ----
# Updated 07.09.2026
randnet.test <- function(..., iter, cores)
{

  # Get networks
  dots <- list(...)

  # Name networks
  names(dots) <- legacy_dot_names(substitute(list(...)))

  # Check for missing iterations
  if(missing(iter)){

    # Set default iterations
    iter <- 1000

  }

  # Check for missing cores
  if(missing(cores)){

    # Set default cores
    cores <- 1

  }

  # Return test
  return(do.call(random_network_test, c(dots, list(iter = iter, cores = cores))))

}

#' @title Bootstrapped Semantic Network Analysis (Legacy)
#'
#' @description Backward-compatible wrapper for
#' \code{\link{bootstrap_SemNeT}} -- \code{bootSemNeT()} was renamed
#' \code{bootstrap_SemNeT()} in SemNeT 2.0.0. The returned object gets both
#' the old class (\code{"bootSemNeT"}, so \code{plot()} still dispatches to
#' \code{\link{plot.bootSemNeT}}) and the new one (\code{"bootstrap_SemNeT"},
#' so it can also be passed straight into \code{\link{bootstrap_test_SemNeT}}
#' or \code{\link{bootstrap_test_SemNeT}}'s own legacy wrapper,
#' \code{\link{test.bootSemNeT}})
#'
#' @param ... Matrices or data frames.
#' Response matrices, one per group, e.g. \code{bootSemNeT(low, high)} --
#' names are captured from the given expressions when not passed as named
#' arguments, matching the original's own \code{substitute()}-based name
#' capture
#'
#' @param method Character.
#' See \code{\link{bootstrap_SemNeT}}. Unlike the original -- which had no
#' working default for this argument at all (\code{switch()} on an
#' unresolved multi-value default silently required a real value every
#' call) -- omitting it now falls through to \code{bootstrap_SemNeT()}'s own
#' default, \code{"tmfg"}
#'
#' @param methodArgs List.
#' See \code{\link{bootstrap_SemNeT}}'s \code{method_args}. \code{minCase}
#' inside \code{methodArgs} is pulled out into \code{minimum_cases}, matching
#' where the original actually used it
#'
#' @param type Character.
#' See \code{\link{bootstrap_SemNeT}}. Same no-working-default situation as
#' \code{method} above; omitting it now defaults to \code{"case"}
#'
#' @param prop Numeric.
#' See \code{\link{bootstrap_SemNeT}}
#'
#' @param sim Character.
#' See \code{\link{bootstrap_SemNeT}}
#'
#' @param weighted Boolean.
#' Unused -- was already dead code in the original \code{bootSemNeT()}
#'
#' @param iter Numeric.
#' See \code{\link{bootstrap_SemNeT}}
#'
#' @param cores Numeric.
#' See \code{\link{bootstrap_SemNeT}}. Unlike the original, defaults to
#' sequential processing rather than auto-detecting cores (see
#' \code{\link{randwalk}}'s equivalent note)
#'
#' @return See \code{\link{bootstrap_SemNeT}}
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Legacy `bootstrap_SemNeT` ----
# Updated 07.09.2026
bootSemNeT <- function(..., method = c("CN", "NRW", "PF", "TMFG"),
                        methodArgs = list(), type = c("case", "node"),
                        prop = .50, sim, weighted = FALSE, iter = 1000, cores)
{

  # Get groups
  dots <- list(...)

  # Name groups
  names(dots) <- legacy_dot_names(substitute(list(...)))

  # Set up arguments
  args <- dots

  # Check for method
  if(!missing(method)){

    # Match method
    args$method <- tolower(match.arg(method, c("CN", "NRW", "PF", "TMFG")))

  }

  # Check for type
  if(!missing(type)){

    # Match type
    args$type <- match.arg(type, c("case", "node"))

  }

  # Check for missing similarity
  if(missing(sim)){

    # Set default similarity
    sim <- "cosine"

  }

  # Check for missing cores
  if(missing(cores)){

    # Set default cores
    cores <- 1

  }

  # Check for minimum cases
  if("minCase" %in% names(methodArgs)){

    # Move minimum cases to argument
    args$minimum_cases <- methodArgs$minCase

    # Remove from method arguments
    methodArgs$minCase <- NULL

  }

  # Set method arguments
  args$method_args <- methodArgs

  # Set proportion
  args$prop <- prop

  # Set similarity
  args$sim <- sim

  # Set iterations
  args$iter <- iter

  # Set cores
  args$cores <- cores

  # Perform bootstrap
  result <- do.call(bootstrap_SemNeT, args)

  # Set legacy class
  class(result) <- c("bootSemNeT", class(result))

  # Return result
  return(result)

}

#' @title Statistical Tests for \code{\link{bootSemNeT}} (Legacy)
#'
#' @description Backward-compatible wrapper for
#' \code{\link{bootstrap_test_SemNeT}} -- \code{test.bootSemNeT()} was
#' renamed \code{bootstrap_test_SemNeT()} in SemNeT 2.0.0.
#'
#' \strong{Legacy warning:} before SemNeT 2.1.0, this function ran
#' \emph{t}-tests/ANOVAs/ANCOVAs on the bootstrap replicates themselves, which
#' is statistically invalid (the replicates are not independent observations,
#' and the bootstrap standard deviation -- already a standard error -- was
#' divided by \code{sqrt(iter)}, inflating test statistics by roughly
#' \code{sqrt(iter)}). It now always warns and returns the results of
#' \code{bootstrap_test_SemNeT()}: by default a Wald test of each contrast
#' using the bootstrap standard error (with normal intervals), or percentile
#' intervals, which have no \emph{p}-value.
#' Results from earlier versions should be recomputed. Only accepts
#' objects produced by this package's own \code{\link{bootSemNeT}} /
#' \code{\link{bootstrap_SemNeT}}, not a raw object saved from SemNeT
#' \code{< 2.0.0}
#'
#' @param ... One or more \code{bootSemNeT}/\code{bootstrap_SemNeT} objects.
#' See \code{\link{bootstrap_test_SemNeT}}
#'
#' @param test Character.
#' \code{"ANCOVA"}, \code{"ANOVA"}, or \code{"t-test"} (the default). Kept
#' only for backward compatibility: the parametric test is \strong{not} run
#' (it is invalid). The comparisons returned are determined by
#' \code{groups}/\code{formula} (see \code{\link{bootstrap_test_SemNeT}})
#'
#' @param measures Character.
#' One or more of \code{"ASPL"}, \code{"CC"}, \code{"Q"}.
#' Defaults to all three
#'
#' @param formula Character.
#' See \code{\link{bootstrap_test_SemNeT}}
#'
#' @param groups Matrix or data frame.
#' See \code{\link{bootstrap_test_SemNeT}}
#'
#' @return See \code{\link{bootstrap_test_SemNeT}}
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Legacy `bootstrap_test_SemNeT` ----
# Updated 10.10.2026
test.bootSemNeT <- function(..., test = c("ANCOVA", "ANOVA", "t-test"),
                             measures = c("ASPL", "CC", "Q"),
                             formula = NULL, groups = NULL)
{

  # Check for missing test
  if(missing(test)){

    # Set default test
    test <- "t-test"

  }else{

    # Match test
    test <- match.arg(test, c("ANCOVA", "ANOVA", "t-test"))

  }

  # Check for missing measures
  if(missing(measures)){

    # Set default measures
    measures <- c("aspl", "cc", "q")

  }else{

    # Match measures
    measures <- tolower(match.arg(measures, c("ASPL", "CC", "Q"), several.ok = TRUE))

  }

  # Send warning on invalid test
  warning(
    "`test.bootSemNeT()`: this function previously ran a ", test, " on the bootstrap replicates, which is statistically invalid ",
    "(the replicates were treated as independent observations and the bootstrap standard deviation, already a standard ",
    "error, was divided by sqrt(`iter`), inflating test statistics by roughly sqrt(`iter`)).\n",
    "Results are now bootstrap confidence intervals of within-iteration group contrasts from ",
    "`bootstrap_test_SemNeT()`; previously published results should be recomputed.\n",
    "See Tu, Chodorow, Brooks, Ezrina, & Valian (under review), 'Inflated Statistics Lead to Incorrect Inference: ",
    "How (Not) to Make Cognitive Network Group Comparisons'.",
    call. = FALSE
  )

  # Check for ANCOVA
  if(test == "ANCOVA"){

    # Send warning on covariate
    warning(
      "`test.bootSemNeT()`: `test = \"ANCOVA\"`'s edge-count covariate no longer applies; unadjusted group contrasts are returned.",
      call. = FALSE
    )

  }

  # Return tests
  return(bootstrap_test_SemNeT(..., measures = measures, formula = formula, groups = groups))

}

#' @title Plot for \code{\link{bootSemNeT}} (Legacy)
#'
#' @description Backward-compatible wrapper for
#' \code{\link{plot.bootstrap_SemNeT}} -- dispatches for objects returned by
#' the legacy \code{\link{bootSemNeT}}, which carry the old class alongside
#' the new one
#'
#' @param ... An object from \code{\link{bootSemNeT}} (only the first
#' element is used, matching the original)
#'
#' @param groups Character.
#' Labels for groups in the order they were entered into
#' \code{\link{bootSemNeT}}. Defaults to \code{NULL} (keep the names already
#' on the object)
#'
#' @param measures Character.
#' One or more of \code{"ASPL"}, \code{"CC"}, \code{"Q"}.
#' Defaults to all three
#'
#' @return See \code{\link{plot.bootstrap_SemNeT}}
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Legacy `plot.bootstrap_SemNeT` ----
# Updated 07.09.2026
plot.bootSemNeT <- function(..., groups = NULL, measures = c("ASPL", "CC", "Q"))
{

  # Get input
  input <- list(...)

  # Get bootstrap result
  x <- input[[1]]

  # Check for group names
  if(!is.null(groups)){

    # Rename groups
    names(x$groups) <- groups

  }

  # Check for missing measures
  if(missing(measures)){

    # Set default measures
    measures <- c("aspl", "cc", "q")

  }else{

    # Match measures
    measures <- tolower(match.arg(measures, c("ASPL", "CC", "Q"), several.ok = TRUE))

  }

  # Return plot
  return(plot.bootstrap_SemNeT(x, measures = measures))

}

#' @title Compare Graphs (Legacy)
#'
#' @description Backward-compatible wrapper for \code{\link{compare_networks}}
#' -- \code{compare_nets()} was renamed \code{compare_networks()} in SemNeT
#' 2.0.0
#'
#' @param ... Matrices or data frames.
#' Networks to plot, e.g. \code{compare_nets(low, high)} -- names are
#' captured from the given expressions when not passed as named arguments,
#' matching the original's own \code{substitute()}-based name capture
#'
#' @param title List.
#' See \code{\link{compare_networks}}
#'
#' @param config Character.
#' See \code{\link{compare_networks}}
#'
#' @param placement Character.
#' See \code{\link{compare_networks}}
#'
#' @param weighted Boolean.
#' See \code{\link{compare_networks}}
#'
#' @param qgraph.args List.
#' See \code{\link{compare_networks}}'s \code{qgraph_args}
#'
#' @return See \code{\link{compare_networks}}
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Legacy `compare_networks` ----
# Updated 07.09.2026
compare_nets <- function(..., title, config, placement = c("match", "default"),
                          weighted = FALSE, qgraph.args = list())
{

  # Get networks
  dots <- list(...)

  # Name networks
  names(dots) <- legacy_dot_names(substitute(list(...)))

  # Set up arguments
  args <- dots

  # Check for title
  if(!missing(title)){

    # Add title
    args$title <- title

  }

  # Set configuration
  args$config <- tolower(if(missing(config)) "spring" else config)

  # Set placement
  args$placement <- if(missing(placement)) "default" else match.arg(placement, c("match", "default"))

  # Set weights
  args$weighted <- weighted

  # Set {qgraph} arguments
  args$qgraph_args <- qgraph.args

  # Return plot
  return(do.call(compare_networks, args))

}
