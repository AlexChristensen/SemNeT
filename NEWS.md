# SemNeT 2.0.0

Full rebuild of the package's internals (see `REFACTOR_PLAN.md` for the
phase-by-phase development record). Every pre-2.0.0 exported name still
works -- see "Backward compatibility" below -- so existing scripts do not
need to change. The Shiny app (`SemNeTShiny()`) is not part of this rebuild
and remains on the pre-2.0.0 code path.

## New functions

* `permutation_SemNeT()` -- the old `permSemNeT()` was an internal,
  undocumented, unexported function with three real argument-passing bugs
  (see "Bug fixes" below). It is now fully validated, documented, and
  exported for the first time.
* `forward_flow()` -- re-added after being dropped entirely in 1.4.4's CRAN
  resurrection (its `ff_function()` helper called `googledrive::drive_auth()`/
  `drive_download()` to silently fetch pretrained semantic-space vectors at
  runtime, a hard CRAN policy violation). This version never touches the
  network: pass your own word-embedding matrix (`embeddings`, rows named by
  vocabulary term) instead of naming a built-in space. Internals were
  otherwise rewritten to match the rest of the 2.0.0 codebase -- validated
  arguments, `parallel_process()` instead of a hand-managed
  `parallel::makeCluster()`, and the existing `cosine()` helper instead of a
  new `LSAfun` dependency.

## Real bugs fixed

* **`equate()`**: two groups were compared by vocabulary *size*, not actual
  shared words -- two similarly-sized groups with almost no overlap could
  receive zero equating. Fixed to compute the true shared vocabulary
  directly, with a consistent column order across every group.
* **`bootSemNeT()`/`bootstrap_SemNeT()`, `test.bootSemNeT()`/
  `bootstrap_test_SemNeT()`, `equate()`, `response.analysis()`/
  `response_analysis()`, `randnet.test()`/`random_network_test()`,
  `compare_nets()`/`compare_networks()`**: every function taking named
  groups via `...` validated that names were present but not *unique* --
  a duplicate name silently hid or corrupted a result via `$`/`[[` access
  (R always returns only the first match). Now rejected outright where a
  duplicate is a real user mistake, or disambiguated via `make.unique()`
  where two distinct objects can legitimately share a label.
* **`CC()`/`cc()`**: the weighted branch's cube root (`A^(1/3)`) returned
  `NaN` for every negative edge weight, silently breaking weighted
  clustering coefficient on any signed network. Fixed with a real-valued
  signed cube root.
* **`ASPL()`/`aspl()`**: `distance()` silently zeroed unreachable node-pair
  distances before averaging, understating ASPL for any disconnected
  network. Unreachable pairs now correctly propagate `Inf` and are excluded
  from the average.
* **`TMFG()`/`tmfg()`**: `for(e in 5:n)` iterated backwards whenever
  `n == 4`, corrupting the result; `n < 4` produced silent `NA` corruption
  instead of an error. Both fixed.
* **`NRW()`/`nrw()`**: the numeric-response detection heuristic
  (`max(range(data)) >= 1`) rejected virtually every valid ordered response
  matrix. Fixed with a correct, shared heuristic (also used by `CN()`/`cn()`).
* **`test.bootSemNeT()`/`bootstrap_test_SemNeT()`**: `match.arg(measures)`
  was missing `several.ok = TRUE`, so requesting the function's own
  documented default (all three measures at once) errored.
* **`permSemNeT()`/`permutation_SemNeT()`**: a `thresold` typo silently
  dropped `NRW`'s `threshold` argument; `enrich <- args$enrich` (assignment
  used as a bare call argument) silently dropped `CN`'s `enrich` argument;
  one sample's `NRW` call passed its threshold into a parameter named
  `window` instead of `threshold`. All three shared one root cause
  (hand-duplicated per-method argument-passing code) and are structurally
  impossible now, via one shared dispatcher.
* Several `1:n`-style ranges that silently ran backwards or indexed out of
  bounds on degenerate (empty/size-1) inputs, in both inherited and
  newly-written code -- see `REFACTOR_PLAN.md`'s "Phase 0-2 line-by-line
  review" section for the full list.
* `q()`'s modularity is now reproducible via an explicit `seed` argument,
  instead of depending on R's global RNG state via `igraph::cluster_louvain()`
  (which meant the same call could return different results depending on
  unrelated code executed earlier in a session).

## Memory

* `bootstrap_SemNeT()`/`permutation_SemNeT()` no longer keep every
  bootstrapped/permuted network in memory by default -- a full set of `iter`
  dense N x N networks per group/sample (the default is `iter = 1000`) could
  reach gigabyte scale for realistic network sizes, and the only documented
  downstream consumer (`bootstrap_test_SemNeT()`'s ANCOVA covariate) only ever
  needed a single scalar (edge count) per network. Both functions gained a
  `keep_networks` argument, defaulting to `FALSE`: `bootstrap_SemNeT()` now
  always computes each network's edge count at generation time regardless of
  this setting, and `bootstrap_test_SemNeT()` reads that instead of the (now
  usually absent) full networks; `permutation_SemNeT()`'s `previous =` reuse
  (testing a second measure without re-simulating) now requires the original
  call to have used `keep_networks = TRUE`, since that's the only case
  needing the networks kept around afterward.
* `random_network_test()` no longer materializes an entire batch of `iter`
  random networks before reducing them to measures -- each one is reduced
  immediately, since nothing it returns ever needed the raw random networks
  in the first place. No change in behavior or return shape, only in peak
  memory during the call.

## Backward compatibility

Every pre-2.0.0 exported name is kept working via `R/legacy.R`, translating
old-style arguments/call conventions into a call to the new implementation:

| Old name | New name |
|---|---|
| `TMFG()` | `tmfg()` |
| `ASPL()` | `aspl()` |
| `CC()` | `cc()` |
| `CN()` | `cn()` |
| `NRW()` | `nrw()` |
| `PF()` | `pf()` |
| `Q()` | `q()` |
| `bootSemNeT()` | `bootstrap_SemNeT()` |
| `test.bootSemNeT()` | `bootstrap_test_SemNeT()` |
| `plot.bootSemNeT()` | `plot.bootstrap_SemNeT()` |
| `randwalk()` | `random_walk()` |
| `randnet.test()` | `random_network_test()` |
| `sim.fluency()` | `simulate_fluency()` |
| `response.analysis()` | `response_analysis()` |
| `semnetmeas()` | `semantic_network_measures()` |
| `compare_nets()` | `compare_networks()` |

`similarity()`, `finalize()`, `equate()`, `convert2igraph()`, and
`convert2cytoscape()` keep their original names -- no wrapper needed.

A few narrow, documented behavior changes come with this compatibility
layer (see each wrapper's own `?help` page for details):

* `bootSemNeT()`'s `method`/`type` arguments never actually had a working
  default in the original (an unresolved multi-value default reaching
  `switch()`/`if()` would error) -- omitting them now cleanly falls through
  to `bootstrap_SemNeT()`'s own defaults (`"tmfg"`/`"case"`) instead of
  erroring.
* `bootSemNeT()`/`randwalk()`/`randnet.test()` no longer auto-detect and use
  half/most of the machine's CPU cores by default; they now default to
  sequential processing, matching their new-named replacements. Pass
  `cores =` explicitly for parallel execution.
* `finalize()`'s argument names changed from `rmat`/`minCase` to `data`/
  `minimum_cases` -- positional calls are unaffected, but a call naming
  these arguments explicitly will need updating.

## Dependency changes

* **Dropped**: `pbapply`, `dplyr`, `plyr`, `magrittr`, `ggplot2`, `scales`,
  `philentropy`. Plotting (`plot.bootstrap_SemNeT()`) now uses base
  `graphics::boxplot()`; Chebyshev distance (`pf()`) uses
  `stats::dist(method = "maximum")`; parallelism uses `future`/
  `future.apply`/`progressr` with guaranteed cleanup, replacing several
  leak-prone `parallel::makeCluster()`/`pbapply::pblapply()` blocks.
* **Added**: `future`, `future.apply`, `progressr`, `stats`, `graphics`,
  `utils` (all either new parallelism infrastructure or previously-implicit
  base-package usage now declared explicitly).
* **Unchanged**: `igraph`, `qgraph` (used by `compare_networks()`'s
  plotting), `car`, `broom`, `effects` (used by `bootstrap_test_SemNeT()`'s
  ANOVA/ANCOVA), `methods`.
* This package now vendors a small amount of compiled C (`src/`, from
  `{EGAnet}`) for Louvain community detection and signed modularity,
  replacing `igraph::cluster_louvain()` for `q()`. `NeedsCompilation` is now
  `yes`.

## Internal

* One file per exported function (function + its own input validation),
  matching `{EGAnet}`'s conventions -- replaces the previous single
  1,800+ line `utils-SemNeT.R` and several large, multi-function files.
* `bootstrap_SemNeT()` (and everything downstream of it) uses one
  consistent result schema (`list(groups = list(<name> = list(networks,
  measures, summary)), meta = list(...))`, class `"bootstrap_SemNeT"`)
  instead of the previous fragile `"XNet"/"XMeas"/"XSumm"` string-splicing
  convention that three different files independently regex-parsed.
* Numerous hot loops vectorized (`similarity()`, `tmfg()`, `cn()`,
  `random_walk()`, and others) -- see `REFACTOR_PLAN.md`'s optimization-pass
  sections for benchmarked, verified details.
* `R/methods.R` (verified 100% dead/shadowed code) removed.
