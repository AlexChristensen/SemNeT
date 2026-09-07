#' @noRd
# Every pre-2.0.0 exported name that changed lives in this one file, as a
# thin wrapper translating old arguments/call conventions and calling
# straight into its new-named replacement -- keeps the whole compatibility
# surface auditable in one place (see REFACTOR_PLAN.md's legacy-wrapper
# audit table). Functions whose old name already matches the new one
# (`similarity`, `finalize`, `equate`, `convert2igraph`, `convert2cytoscape`)
# need no wrapper and aren't repeated here.
# Updated 07.09.2026
legacy_dot_names <- function(dots_call)
{
  # `dots_call` must be the result of calling `substitute(list(...))`
  # directly inside the wrapper below -- `substitute()` only ever sees the
  # calling function's own `...`, so every wrapper computes this itself and
  # hands the already-substituted call in here for processing
  args <- as.list(dots_call)[-1]

  # Nothing in '...' -- let the caller's own validation report that
  if(length(args) == 0){
    return(character(0))
  }

  # Explicit names (e.g. `bootSemNeT(low = a, high = b)`) win outright;
  # anything left unnamed falls back to the deparsed expression itself
  # (e.g. plain `bootSemNeT(low, high)`), matching every pre-2.0.0 function's
  # own `as.character(substitute(list(...)))`-based name capture
  given_names <- names(dots_call)[-1]

  if(is.null(given_names)){
    given_names <- rep("", length(args))
  }

  deparsed <- vapply(args, function(arg) paste(deparse(arg), collapse = ""), character(1))

  return(ifelse(nzchar(given_names), given_names, deparsed))
}

#' @rdname tmfg
#' @export
# Legacy name for `tmfg()` -- merged onto the same Rd page (`@rdname`) rather
# than getting its own, since `TMFG.Rd`/`tmfg.Rd` would only differ by case,
# which `R CMD check`'s portable-file-names check rejects outright
# Updated 07.09.2026
TMFG <- function(data, depend = FALSE)
{
  return(tmfg(data, depend = depend))
}

#' @rdname aspl
#' @export
# Legacy name for `aspl()` -- see the `@rdname` note on `TMFG()` above
# Updated 07.09.2026
ASPL <- function(A, weighted = FALSE)
{
  return(aspl(A, weighted = weighted))
}

#' @rdname cc
#' @export
# Legacy name for `cc()` -- see the `@rdname` note on `TMFG()` above
# Updated 07.09.2026
CC <- function(A, weighted = FALSE)
{
  return(cc(A, weighted = weighted))
}

#' @rdname cn
#' @export
# Legacy name for `cn()` -- see the `@rdname` note on `TMFG()` above
# Updated 07.09.2026
CN <- function(data, window = 2, alpha = .05, enrich = FALSE)
{
  return(cn(data, window = window, alpha = alpha, enrich = enrich))
}

#' @rdname nrw
#' @export
# Legacy name for `nrw()` -- see the `@rdname` note on `TMFG()` above
# Updated 07.09.2026
NRW <- function(data, type = c("num", "prop"), threshold = 0)
{
  return(nrw(data, type = type, threshold = threshold))
}

#' @rdname pf
#' @export
# Legacy name for `pf()` -- see the `@rdname` note on `TMFG()` above
# Updated 07.09.2026
PF <- function(data)
{
  return(pf(data))
}

#' @rdname q
#' @export
# Legacy name for `q()` -- see the `@rdname` note on `TMFG()` above
# Updated 07.09.2026
Q <- function(A)
{
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
# Legacy name for `simulate_fluency()`
# Updated 07.09.2026
sim.fluency <- function(nodes, cases, random = FALSE)
{
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
# Legacy name for `semantic_network_measures()`
# Updated 07.09.2026
semnetmeas <- function(A, meas = c("ASPL", "CC", "Q"), weighted = FALSE)
{
  result <- semantic_network_measures(A, measures = tolower(meas), weighted = weighted)
  names(result) <- toupper(names(result))
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
# Legacy name for `response_analysis()`
# Updated 07.09.2026
response.analysis <- function(...)
{
  dots <- list(...)
  names(dots) <- legacy_dot_names(substitute(list(...)))

  return(do.call(response_analysis, dots))
}

#' @title Random Walk Simulation (Legacy)
#'
#' @description Backward-compatible wrapper for \code{\link{random_walk}} --
#' \code{randwalk()} was renamed \code{random_walk()} in SemNeT 2.0.0
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
# Legacy name for `random_walk()`
# Updated 07.09.2026
randwalk <- function(A, B, reps = 20, steps = 10, iter = 10000, cores)
{
  # Deparse the given expressions for labeling, same as the original --
  # `data$low`-style expressions are shortened to just their last segment
  name_a <- deparse(substitute(A))
  name_b <- deparse(substitute(B))

  if(grepl("$", name_a, fixed = TRUE)){
    name_a <- utils::tail(strsplit(name_a, "$", fixed = TRUE)[[1]], 1)
  }
  if(grepl("$", name_b, fixed = TRUE)){
    name_b <- utils::tail(strsplit(name_b, "$", fixed = TRUE)[[1]], 1)
  }

  args <- list(A = A, B = B, name_a = name_a, name_b = name_b, reps = reps, steps = steps, iter = iter)

  if(!missing(cores)){
    args$cores <- cores
  }

  return(do.call(random_walk, args))
}

#' @title Random Network Test (Legacy)
#'
#' @description Backward-compatible wrapper for
#' \code{\link{random_network_test}} -- \code{randnet.test()} was renamed
#' \code{random_network_test()} in SemNeT 2.0.0
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
# Legacy name for `random_network_test()`
# Updated 07.09.2026
randnet.test <- function(..., iter, cores)
{
  dots <- list(...)
  names(dots) <- legacy_dot_names(substitute(list(...)))

  if(missing(iter)){
    iter <- 1000
  }
  if(missing(cores)){
    cores <- 1
  }

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
# Legacy name for `bootstrap_SemNeT()`
# Updated 07.09.2026
bootSemNeT <- function(..., method = c("CN", "NRW", "PF", "TMFG"),
                        methodArgs = list(), type = c("case", "node"),
                        prop = .50, sim, weighted = FALSE, iter = 1000, cores)
{
  dots <- list(...)
  names(dots) <- legacy_dot_names(substitute(list(...)))

  args <- dots

  # The original never actually had a usable default for either of these
  # (see the roxygen note above) -- when omitted, just let
  # `bootstrap_SemNeT()`'s own default take over instead of forwarding an
  # unresolved, differently-ordered choice vector
  if(!missing(method)){
    args$method <- tolower(match.arg(method, c("CN", "NRW", "PF", "TMFG")))
  }
  if(!missing(type)){
    args$type <- match.arg(type, c("case", "node"))
  }

  if(missing(sim)){
    sim <- "cosine"
  }
  if(missing(cores)){
    cores <- 1
  }

  # `minCase` lived inside `methodArgs` in the original, only ever read for
  # the case-resample + TMFG combination -- now a first-class argument
  if("minCase" %in% names(methodArgs)){
    args$minimum_cases <- methodArgs$minCase
    methodArgs$minCase <- NULL
  }

  args$method_args <- methodArgs
  args$prop <- prop
  args$sim <- sim
  args$iter <- iter
  args$cores <- cores

  result <- do.call(bootstrap_SemNeT, args)
  class(result) <- c("bootSemNeT", class(result))

  return(result)
}

#' @title Statistical Tests for \code{\link{bootSemNeT}} (Legacy)
#'
#' @description Backward-compatible wrapper for
#' \code{\link{bootstrap_test_SemNeT}} -- \code{test.bootSemNeT()} was
#' renamed \code{bootstrap_test_SemNeT()} in SemNeT 2.0.0. Only accepts
#' objects produced by this package's own \code{\link{bootSemNeT}} /
#' \code{\link{bootstrap_SemNeT}}, not a raw object saved from SemNeT
#' \code{< 2.0.0}
#'
#' @param ... One or more \code{bootSemNeT}/\code{bootstrap_SemNeT} objects.
#' See \code{\link{bootstrap_test_SemNeT}}
#'
#' @param test Character.
#' See \code{\link{bootstrap_test_SemNeT}}
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
# Legacy name for `bootstrap_test_SemNeT()`
# Updated 07.09.2026
test.bootSemNeT <- function(..., test = c("ANCOVA", "ANOVA", "t-test"),
                             measures = c("ASPL", "CC", "Q"),
                             formula = NULL, groups = NULL)
{
  # The original's own missing-check hard-coded "t-test" as the real
  # default, ignoring the formal argument's listed order -- match that
  # exactly rather than trusting `match.arg()`'s "first listed" rule
  if(missing(test)){
    test <- "t-test"
  }else{
    test <- match.arg(test, c("ANCOVA", "ANOVA", "t-test"))
  }

  if(missing(measures)){
    measures <- c("aspl", "cc", "q")
  }else{
    measures <- tolower(match.arg(measures, c("ASPL", "CC", "Q"), several.ok = TRUE))
  }

  return(bootstrap_test_SemNeT(..., test = test, measures = measures, formula = formula, groups = groups))
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
# Legacy name for `plot.bootstrap_SemNeT()`
# Updated 07.09.2026
plot.bootSemNeT <- function(..., groups = NULL, measures = c("ASPL", "CC", "Q"))
{
  input <- list(...)
  x <- input[[1]]

  if(!is.null(groups)){
    names(x$groups) <- groups
  }

  if(missing(measures)){
    measures <- c("aspl", "cc", "q")
  }else{
    measures <- tolower(match.arg(measures, c("ASPL", "CC", "Q"), several.ok = TRUE))
  }

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
# Legacy name for `compare_networks()`
# Updated 07.09.2026
compare_nets <- function(..., title, config, placement = c("match", "default"),
                          weighted = FALSE, qgraph.args = list())
{
  dots <- list(...)
  names(dots) <- legacy_dot_names(substitute(list(...)))

  args <- dots

  if(!missing(title)){
    args$title <- title
  }

  args$config <- tolower(if(missing(config)) "spring" else config)

  # The original's own missing-check hard-coded "default" here, ignoring
  # the formal argument's listed order -- match that exactly
  args$placement <- if(missing(placement)) "default" else match.arg(placement, c("match", "default"))

  args$weighted <- weighted
  args$qgraph_args <- qgraph.args

  return(do.call(compare_networks, args))
}
