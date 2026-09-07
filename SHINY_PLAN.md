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

## Execution cadence
Phase 6 first, full verification, stop for check-in. Then Phase 7 module-by-module,
each checked against the app running in a real browser, final end-to-end pass last.
