# SemNeT Shiny App: Rebuild (+ core memory-lean pass)

See `/home/alextops/.claude/plans/i-would-like-to-jolly-hoare.md` for the full,
approved plan and its rationale. `REFACTOR_PLAN.md` covers the completed core-package
rebuild (Phases 0-5); this document tracks the follow-up work: a memory-lean pass on
3 core files (Phase 6), then the Shiny app rebuild itself (Phase 7).

## Phase 6 — Core package memory-lean pass

| File | Change | Status |
|---|---|---|
| `R/bootstrap_SemNeT.R` | Add `keep_networks = FALSE`; `bootstrap_iteration()` computes `edges` per-iteration, drops `network` unless requested | Done |
| `R/bootstrap_test_SemNeT.R` | `build_measure_frame()` reads `$edges` instead of recomputing from `$networks` | Done |
| `R/permutation_SemNeT.R` | Add `keep_networks = FALSE`; move measure computation into `permutation_iteration()`; `previous=` requires original call used `keep_networks = TRUE` | Done |
| `R/random_network_test.R` | Internal-only: reduce each random network to measures inside the iteration instead of materializing the whole batch first (no schema/behavior change) | Done |
| `tests/testthat/test-bootstrap_SemNeT.R` | Add `keep_networks = TRUE` to the 3 assertions touching `$networks`; add a default-is-NULL/`edges`-always-present test | Done |
| `tests/testthat/test-permutation_SemNeT.R` | Add `keep_networks = TRUE` to the 2 tests touching `$networks`/`previous` reuse; add default-NULL + `previous`-requires-`keep_networks` tests | Done |
| `NEWS.md` | New "Memory" entry describing `keep_networks` + the `random_network_test()` internal optimization | Done |
| Verification | `devtools::test()`: 380/380 passing. `R CMD check --as-cran`: 0 errors/0 warnings/0 notes | Done |

## Phase 7 — Shiny app rebuild

Architecture note: Shiny's `runApp()` prefers `server.R` over `app.R` when both
exist in a directory, so a manual `app.R` sourcing loop for the modules never
actually ran. Switched to Shiny's own built-in autoload convention instead:
`inst/Shiny/global.R` (sourced once, replaces `app.R`) plus every module file
living directly under `inst/Shiny/R/` (auto-sourced, non-recursively, before
`ui.R`/`server.R` — no manual `source()` loop needed). `app.R` was deleted.

| Module | Calls | Status |
|---|---|---|
| `inst/Shiny/global.R` | `library(SemNeT)`, `library(shinyBS)` (see bugs below), `progressr::handlers()`, `options(shiny.maxRequestSize=...)` | Done |
| `inst/Shiny/ui.R` | top-level `navbarPage` assembling module `_ui()`s | Done |
| `inst/Shiny/server.R` | session-scoped `reactiveValues()` `state`, calls module `_server()`s | Done |
| `R/mod_load_data.R` | `read_uploaded_file()`, bundled example data | Done |
| `R/mod_network_estimation.R` | `similarity()`, `tmfg()`/`cn()`/`nrw()`/`pf()`, `finalize()`, `equate()`, `responses_to_binary()`, `semantic_network_measures()`, `compare_networks()` | Done |
| `R/mod_random_network.R` | `random_network_test()` via `ExtendedTask` | Done |
| `R/mod_bootstrap.R` | `bootstrap_SemNeT()` (`keep_networks=FALSE`), `bootstrap_test_SemNeT()`, `plot.bootstrap_SemNeT()` via `ExtendedTask` | Done |
| `R/mod_permutation.R` (new tab) | `permutation_SemNeT()` via `ExtendedTask` (two tasks: run + reuse) | Done |
| `R/mod_random_walk.R` | `random_walk()` via `ExtendedTask` | Done |
| `R/mod_spreading_activation.R` | `spreadr::spreadr()` via `ExtendedTask` + live `qgraph` frame rendering (no `recordPlot()`/`animation` package needed) | Done |
| `R/mod_export.R` | `downloadHandler()`s over `state` | Done |
| `R/mod_shared.R` | `max_app_cores()`, `warn_if_slow()`, `task_status_ui()`, `plot_spreading_frame()` | Done |
| `R/shiny_helpers.R` (package-level) | `read_uploaded_file()` | Done |
| `R/SemNeTShiny.R` | `requireNamespace()` checks for Shiny-only Suggests | Done |
| `DESCRIPTION` | Added Shiny-only `Suggests` (dropped `animation` — no longer needed, see above) | Done |

### Real bugs found and fixed via actual browser testing (Playwright + headless Chrome)
- `compare_networks(..., title = names(networks))` errored (`title` must be a
  `list`, not a vector) — fixed by omitting `title` entirely and relying on
  its own default (`as.list(names(networks))` when missing).
- `permutation_SemNeT()` labels its result columns via `substitute()` on its
  own call-site argument names — inside an `ExtendedTask` closure those are
  always literally `sample_1`/`sample_2`, not the chosen group names. Fixed
  by tracking the real names separately and relabeling the displayed table.
- `shinyBS::bsModal()`'s CSS/JS (`addResourcePath("sbs", ...)`) is only
  registered by its own `.onAttach` hook, which fires via `library(shinyBS)`
  only — not `shinyBS::` namespace access. Without it the modal's assets
  404'd and the modal silently never opened. Fixed by attaching `shinyBS` in
  `global.R`.
- `spreadr::spreadr()` requires a non-negative adjacency matrix; a TMFG
  network (built from correlations) can have negative edges. Fixed by
  spreading over `abs(network)` (magnitude, not sign, determines connection
  strength for this purpose).
- Every `ExtendedTask`-based tab was left permanently showing "Running..."
  if the background task errored, since `task$result()` re-throws and
  nothing was reading it safely. Fixed with a shared `task_status_ui()`
  helper (checks `task$status()` before ever calling `task$result()`) applied
  to all 5 tabs that run one.
- Verified with a deliberately negative-edge network that this task-error
  path now surfaces a real "Failed: ..." message instead of hanging.

### Verification performed
- `devtools::test()`: 380/380 passing (core package unaffected).
- `R CMD check --as-cran`: run after Phase 7 changes (see below for result).
- Ran the app for real (`shiny::runApp()` + headless Chrome via Playwright,
  not just code review) through the full golden path: Load Data → Network
  Estimation (TMFG) → Random Network → Bootstrap (+ test + plot) →
  Permutation (+ "test another measure" reuse, confirmed near-instant since
  it skips resimulation) → Random Walk → Spreading Activation (set
  activations via the modal, run, animate the time slider) → Export
  (downloaded an `.rds` and a `.csv`, confirmed non-empty on disk).
- Confirmed zero console errors/failed network requests on a clean run.

## Deferred / Dropped
- "Auto-detect pre-existing `textcleaner`/`group` objects in your R global
  environment" (original Load Data tab) — dropped, incompatible with per-session
  isolation. File upload + bundled example data are the two input paths.

## Bugs confirmed in the original, not to reproduce
- `compare_netShiny`'s plot-grid remainder formula diverged from `compare_nets`'s.
- `bootSemNeTShiny`'s `eq` read via lexical scoping from the caller's global env.
- Stale-cache `if(exists(paste(percents[i]), envir = globalenv())) next`.
- Dead self-referencing `resultShiny$equated <<- eq` in `save_master`.
- `R/methods.R`'s duplicate-of-a-duplicate `plot.compareShiny`/`plot.animateShiny`.

## Real bug found post-launch, via the actual installed package (2026-09-10)

User report: launching the real app (`SemNeTShiny()`, i.e. `library(SemNeT)` +
`shiny::runApp()` on the installed package) and running TMFG threw "could not find
function `responses_to_binary`". Every prior Phase 7 verification used
`devtools::load_all()` in the same session as `shiny::runApp()`, and `load_all()`
attaches package internals (not just exports) -- masking exactly this class of bug.

Audited every call from `inst/Shiny/` to a package-internal (non-exported)
function and found three real instances, all invisible under a real
`library(SemNeT)` install:
- `mod_network_estimation.R` called `responses_to_binary()` directly (the
  reported bug) -- fixed by exporting it properly (full roxygen docs, its own
  up-front validation via `response_matrix_error()`).
- `mod_load_data.R` called `read_uploaded_file()` directly -- same fix, exported
  with generalized (non-Shiny-specific) documentation and error text.
- `mod_bootstrap.R` called the S3 method `plot.bootstrap_SemNeT()` directly by
  name instead of the generic `plot()` -- fixed by calling `plot(...)` instead,
  which dispatches correctly via the method's existing `S3method()` registration
  regardless of export status (the idiomatic fix; exporting an S3 method by name
  for direct calling is not the normal R convention).

Verified all three via a real `R CMD INSTALL` + clean `library(SemNeT)` session
(no `load_all()`), reproducing the exact reported error first, then confirming
the fix. Also re-verified live in a real browser (Playwright + system Chrome,
`claude-in-chrome` not connected in this environment) against the actual running
app: Load Data -> Network Estimation (TMFG) now renders both networks and the
measures table with no error, matching the fix.

## Real progress bar added, and a second severe bug found in the process (2026-09-10)

User request: the app only ever showed static "Running..." text during a long
analysis, with no indication of actual progress. Investigating this uncovered a
much more serious, previously-undetected bug first.

**Background execution was never actually working.** `global.R` never called
`future::plan()`, so `promises::future_promise()` (used by every `ExtendedTask`)
fell back to `future::sequential` -- meaning every "background" task actually ran
synchronously in the main R process, **blocking Shiny's entire event loop** (no
UI updates, unresponsive to any input, not even serving a fresh page load) for
the task's full duration. Confirmed directly: launching the real app and running
a 1000-iteration bootstrap pegged the main R process at 60%+ CPU while
`page.goto()` on a *fresh* browser tab timed out completely. This had gone
undetected through all of Phase 7's own testing because 258-participant real
data at 1000 iterations takes ~5 minutes (confirmed directly:
`bootstrap_SemNeT(iter = 1000, cores = 1)` on the bundled `open.clean` example
takes 291 seconds) -- Phase 7's tests used short waits and small iteration
counts, checking only that the *final* result rendered correctly, never that the
UI stayed interactive *during* a realistically long run. Fixed by adding
`future::plan(future::multisession, workers = max_app_cores())` to `global.R`
(which had to move `max_app_cores()`'s definition there too, since `global.R` is
sourced before `inst/Shiny/R/*.R` -- see its own comment). Verified via the same
reproduction: the app now stays fully responsive (tab switches during a run
measured at ~0.03s) while a background task legitimately runs for minutes.

**`progressr::handler_shiny()` cannot report progress from an `ExtendedTask`,
at all, ever, in this architecture.** Confirmed via direct, isolated
experimentation (a minimal standalone Shiny app, independent of SemNeT): a
`progressr::progressor()` created in the main session and passed as an argument
into `future_promise()` (progressr's own documented pattern for cross-process
relay) delivered *nothing* back to the main session -- not to `handler_shiny()`,
and not even to a plain file-writing handler (0 bytes written, confirmed
directly). Root cause: progressr's relay is built around a caller that
*synchronously blocks* waiting on the future (e.g. a console session's
`future::value()`); `promises::future_promise()`'s `later()`-based polling
never does that kind of blocking wait, so the relay mechanism progressr relies
on never triggers.

Fixed with a different, directly-verified-working mechanism instead of
`progressr` for this specific case: `parallel_process()` (helpers.R) now checks
`getOption("SemNeT.progress_file")` and, if set, writes `"<current> <total>"` to
that file after every iteration -- a plain file being a channel that needs no
cross-process relay at all, just ordinary filesystem access from both the
worker (writing) and the main session (reading). No exported function's
signature changed; this is purely additive and a no-op by default (option
unset). Every `ExtendedTask` that runs through `parallel_process()` (Bootstrap,
Permutation x2, Random Network, Random Walk) now creates a fresh per-invocation
file via `mod_shared.R`'s `new_progress_file()`, sets
`options(SemNeT.progress_file = ...)` as the very first line *inside* its own
`future_promise()` body (so no cross-process option propagation is needed
either -- it's set directly in the worker, from an argument that was properly
passed in), and renders a real percentage bar via `progress_status_ui()`, fed
by `progress_poller()`'s `shiny::reactivePoll()` (400ms). Spreading Activation
(a single `spreadr::spreadr()` call, not something we iterate over) keeps the
plain `task_status_ui()` message, since there's no meaningful per-iteration
count to report there.

One known, accepted limitation: a function that calls `parallel_process()` more
than once per task (`bootstrap_SemNeT()` once per group; `random_walk()` once
per step level) resets the bar's denominator at each call, so the percentage
visibly drops back down when the next group/level begins rather than
continuing as one unified 0-100% run. Verified live in the real running app
(Playwright + system Chrome) on Bootstrap and Permutation: the bar updates
continuously with real, accurate iteration counts throughout a genuine
multi-second run, then gives way cleanly to the results table on completion.

## Real bugs found post-launch, via actual user reports (2026-09-12)

**The progress bar jumped around instead of climbing steadily** (up to 100%,
back down to 36%, back up, ...) -- reported by the user running a real
bootstrap with `cores > 1`. The prior verification above ("the bar updates
continuously with real, accurate iteration counts") only ever exercised
`cores = 1`, where `future_lapply()` completes iterations strictly in order --
that's why the bug wasn't caught then. Root cause: `parallel_process()` wrote
the *raw iteration index* `i` to the progress file after each completion
(`writeLines(paste(i, iterations), progress_file)`), but with `cores > 1`,
`future_lapply()`'s parallel workers finish iterations out of order -- worker B
can finish iteration 900 before worker A finishes iteration 100 -- so the
displayed value reflected whichever worker most recently wrote, not how many
iterations had actually completed. Fixed by tracking a completion *count*
instead of echoing the index: every completed iteration now appends exactly
one byte to a sibling `<file>.progress` file (`cat("1", file = ..., append =
TRUE)`), and `progress_poller()` reads that file's *size* as the current
count. A byte count can only go up, regardless of which worker writes it or in
what order -- it's structurally immune to the out-of-order-completion problem
that broke the raw-index approach. Verified with a Playwright run against the
real installed app (`cores = 6`, 1500 iterations): 224 polled readings across
the full run, zero non-monotonic transitions.

**Setting a tab's "Cores" input above 1 threw a hard error**: "Attempting to
set up N localhost parallel workers with only 1 CPU cores available for this R
process ... The hard limit is set to 300%" (from `parallelly`'s
`checkNumberOfLocalWorkers()`). Root cause: `parallel_process()`'s own
`cores > 1` branch tried to start a *second*, nested `future::multisession`
pool inside the very worker process that `global.R`'s app-level pool had
already spawned for that `ExtendedTask` -- two layers of `multisession`,
one nested inside the other. `parallelly` deliberately caps a worker
process's own `availableCores()` (usually to 1) specifically to catch this
kind of recursive worker explosion before it happens, and correctly refused.
This is exactly the risk this plan's Phase 7 design flagged as unconfirmed
("`future`'s nested-plan behavior across a fresh worker process is expected
to work cleanly ... but confirm rather than assume") -- now confirmed, and it
does not work cleanly without an explicit cap. Fixed in `parallel_process()`
by capping the requested `cores` to `future::availableCores()` before
switching plans: inside a nested worker this collapses to 1 (sequential,
still correct, just not internally parallel one level deeper); at the
top level (interactive/console use, where `availableCores()` correctly
reflects the real host) the user's requested `cores` passes through
unchanged.

**`max_app_cores()`'s cap was lowered from this plan's original design, per
direct request.** The cap of `min(detectCores() - 1, 8)` above was this
plan's own precaution for a shared, multi-user hosted deployment; for this
deployment it's unwanted friction, so it's now simply `detectCores() - 1`
(the full host, minus one core left for the OS/main Shiny process). The
nested-worker cap immediately above stays in place regardless -- it protects
correctness (no crash on a nested nested-pool attempt), not host fairness,
so it's needed at any core-cap size.

## Raincloud plot restored for `plot.bootstrap_SemNeT()` (2026-09-12)

The core-package memory-lean pass's dependency trim had gone one step too
far: dropping `dplyr`/`plyr`/`magrittr`/`scales` (glue the original used
*incidentally*, for one grouped min/max, one sort-by-column call, and default
axis breaks) was the right call, but the same pass also replaced the actual
plot -- a raincloud plot (half-violin + boxplot + jittered raw values) built
on `ggplot2` -- with a plain base-graphics `boxplot()`, on the reasoning that
it showed "the same bootstrapped spread-and-median information." Per direct
feedback: that reasoning doesn't apply to plotting, which the trim should
have left alone. Reverted: `R/plot.bootstrap_SemNeT.R` now ports the
original's hand-rolled `ggplot2` flat-violin `ggproto` geom, rewritten
against base R only (`ave()`/`order()` in place of the `dplyr`/`plyr` calls
the original used), with `ggplot2` re-added as a `Suggests`-only dependency
(checked via `requireNamespace()` right before drawing, matching how the
Shiny app's own optional dependencies are handled -- the core package still
loads without it).

One real bug surfaced while porting, caught by actually rendering the plot
rather than only checking `expect_no_error()`: the flat-violin geom's
`setup_data()` computed each violin's bounding box by grouping on data group
alone. That was harmless in the original (each call plotted one measure,
never faceted), but this port facets all three measures (ASPL/CC/Q) into one
plot via `facet_wrap(scales = "free_x")` -- grouping by data group alone
pools a group's rows *across every facet panel*, so each panel's supposedly
"free" axis range got contaminated by the other panels' (very different)
scales, and every panel rendered with the same, widest-measure axis
regardless of `scales = "free_x"`. Fixed by grouping on `(PANEL, group)`
together. Verified by actually rendering the plot (not just checking for
errors) before and after the fix, and end-to-end live in the running app
(Playwright): the Bootstrap tab's "Generate Bootstrap Plots" button produces
a correctly-scaled three-panel raincloud plot per group.

## Execution cadence
Phase 6 first, full verification, stop for check-in. Then Phase 7 module-by-module,
each checked against the app running in a real browser, final end-to-end pass last.
