# SemNeT Refactor — Tracking Document

Full rebuild of `SemNeT` (originally CRAN v1.4.5) in this sibling directory. The
original, untouched source lives at `../SemNeT` — read-only reference, never edited.

**Goal:** single-responsibility functions, input validated/coerced up front, bugs
eliminated, hot loops optimized, structural conventions matched to `../EGAnet`.

**Full plan file:** `/home/alextops/.claude/plans/i-would-like-to-jolly-hoare.md`
(context, rationale, and the four binding product decisions below were made there).

**Execution cadence:** one phase at a time. Finish a phase, run its verification,
update this file's status table, check in before starting the next phase.

## Binding decisions (already made, do not re-litigate)
1. `permSemNeT` → fix its bugs and properly export/document it (Phase 4).
2. Shiny app (`inst/Shiny/`, `utils-SemNeTShiny.R`) → **deferred entirely**, out of
   scope for this rebuild.
3. API: new snake_case implementations are free to change internals/return shapes;
   **old exported names/signatures must keep working** as thin wrappers in `R/legacy.R`.
4. Dependency trim: aggressive. `qgraph` dropped from `convert2igraph` (direct
   `igraph::graph_from_adjacency_matrix`, per `EGAnet` precedent); `future`/
   `future.apply`/`progressr` replace base `parallel`/`pbapply`; audit all `Imports`
   at Phase 5.

## Conventions

- **One file per exported function**, named after its **new** snake_case name
  (e.g. `R/tmfg.R`, `R/bootstrap_SemNeT.R`). Each file holds: the function, its own
  `<name>_errors()` validation subroutine (called first, before any core logic), and
  any subroutine used *only* by that function.
- **`R/helpers.R`**: generic, genuinely cross-cutting primitives only — a subroutine
  moves here once it's used by ≥2 functions/files. Building up as of Phase 1:
  `object_error()`, `typeof_error()`, `length_error()`, `range_error()`,
  `adjacency_matrix_error()`, `response_matrix_error()`, `get_object_type()`,
  `swiftelse()`, `parallel_process()`, `cosine()`, `cohens_d()`.
- **`R/legacy.R`**: every pre-2.0.0 exported name that changed, as a thin wrapper
  translating old arguments and calling the new function. Names that are already
  snake_case/unchanged (`similarity`, `finalize`, `equate`, `convert2igraph`,
  `convert2cytoscape`) need no wrapper — the new function *is* the export.
- **Style**: snake_case for all new names; a comment on essentially every line
  (match `EGAnet`'s density); `match.arg()` (with `several.ok = TRUE` where multiple
  values are legitimate) instead of ad hoc string comparisons; no
  `substitute()`-based name-sniffing in new code — require named arguments.
- **Function naming — full action words, no abbreviations.** Expand abbreviated
  verbs in full: `boot` → `bootstrap`, `sim` → `simulate`, `perm` → `permutation`,
  `meas` → `measures`, `nets` → `networks`. Functions that originally ended in the
  literal `SemNeT` suffix keep that exact suffix (matches the original family:
  `bootSemNeT`, `test.bootSemNeT`, `permSemNeT`):
  - `bootSemNeT` → **`bootstrap_SemNeT`**
  - `test.bootSemNeT` → **`bootstrap_test_SemNeT`**
  - `plot.bootSemNeT` → **`plot.bootstrap_SemNeT`** (S3 method on class `bootstrap_SemNeT`)
  - `permSemNeT` → **`permutation_SemNeT`**
  - `sim.fluency` → **`simulate_fluency`**
  - `semnetmeas` → **`semantic_network_measures`**
  - `compare_nets` → **`compare_networks`**
  - `randwalk`, `randnet.test`, `response.analysis` already spell their verbs out in
    full (`random_walk`, `random_network_test`, `response_analysis`) — unchanged.
  - `convert2igraph`, `convert2cytoscape` kept as-is — matches the established
    `convert2<target>` pattern already used unchanged in `EGAnet`, not an
    accidental abbreviation.
  - `TMFG`, `ASPL`, `CC`, `CN`, `NRW`, `PF`, `Q` kept as lowercase acronyms
    (`tmfg`, `aspl`, `cc`, `cn`, `nrw`, `pf`, `q`) — these are established network-
    science technique/measure names in the literature, not abbreviated verbs, so
    "full action word" expansion doesn't apply the same way. **Flagging this call
    for confirmation** — say the word if any of these should be spelled out too
    (e.g. `clustering_coefficient` for `CC`).
- **Parallelism**: always through `parallel_process()` (one `future` plan per call,
  cleanup guaranteed) — never a raw `parallel::makeCluster`.
- **`bootstrap_SemNeT` result schema** (used by `bootstrap_SemNeT`,
  `bootstrap_test_SemNeT`, `plot.bootstrap_SemNeT`, `permutation_SemNeT` — replaces
  the old `"XNet"/"XMeas"/"XSumm"` string-splicing):
  ```r
  list(
    groups = list(<group_name> = list(networks = ..., measures = ..., summary = ...)),
    meta   = list(prop = , iter = , type = , method = )
  )
  # class(x) <- "bootstrap_SemNeT"
  ```
- **Tests**: every new function gets `tests/testthat/test-<name>.R` alongside it,
  built against the bundled fixtures (`open.clean`/`open.group`, `net.high`/`net.low`,
  `animals.freq`, `one.result`/`two.result`).

## Deferred / Dropped

| Item | Disposition | Why |
|---|---|---|
| Shiny app (`inst/Shiny/`, `R/utils-SemNeTShiny.R`) | Deferred | ~5,000 lines, ~60% of original codebase, depends on nearly everything else being stable first. Explicit product decision, not an oversight. |
| `R/methods.R` | Dropped | Verified 100% dead code — its `plot.bootSemNeT`/`plot.compareShiny`/`plot.animateShiny` are silently shadowed by later-loaded files at original alphabetical source order; none ever executed. |
| `.onload` (typo) in `zzz.R` | Dropped | Misspelled (`.onLoad` is correct), called `library.dynam()` which would error anyway since `NeedsCompilation: no`. Never actually ran. |

## Phase 0 — Scaffold

| Item | Status | Notes |
|---|---|---|
| `DESCRIPTION` | Done | v2.0.0; `Imports: methods, utils` so far, grows per phase. |
| `NAMESPACE` | Done | roxygen-generated, do not hand-edit. |
| `tests/testthat.R` + `tests/testthat/` | Done | testthat 3rd edition. |
| `data/*.RData` (9 files) | Done | Copied verbatim from original. |
| `R/<dataset>.R` doc stubs (9 files) | Done | Copied verbatim (pure roxygen, no logic). |
| `R/SemNeT-package.R` | Done | `"_PACKAGE"` sentinel, ported from original `SemNeT.R`. |
| `R/zzz.R` | Done | `.onAttach` kept (ported + snake_case internals `style_text()`/`system_check()`); dead `.onload` dropped. |
| `.Rbuildignore` | Done | |
| `NEWS.md` | Done | |
| `REFACTOR_PLAN.md` | Done | This file. |
| `tests/testthat/test-datasets.R` | Done | Placeholder smoke test so `R CMD check` has something to run; real coverage starts in Phase 1. |

**Verified:** `devtools::load_all()` succeeds, package attaches with its startup
message, all 9 datasets are visible, `devtools::check()` → 0 errors, 0 notes, 1
expected warning (`Rd cross-references`: `animals.freq.Rd`/`one.result.Rd`/
`two.result.Rd` link to `sim.fluency`/`bootSemNeT`, which don't exist yet as legacy
wrappers — resolves on its own once Phases 4-5 land; not a bug to fix now).

## Phase 1 — Shared conventions (`R/helpers.R`)

| Helper | Status | Notes |
|---|---|---|
| `get_object_type()` | Done | Tested. |
| `object_error()` | Done | Tested. |
| `typeof_error()` | Done | Tested. Collapses `integer`/`double`→`numeric` and `closure`→`function`. |
| `length_error()` | Done | Tested. |
| `range_error()` | Done | Tested. |
| `adjacency_matrix_error()` | Done | Tested. Replaces duplicated square/numeric checks in ASPL/CC/Q/TMFG. Optional `require_symmetric` flag. |
| `response_matrix_error()` | Done | Tested. Unifies NRW's `>= 1` vs. `stat.cooccur`'s `== 1` conflicting heuristics: character/factor content passes through untouched (assumed raw responses), numeric content must already be strictly binary or it errors — no silent reinterpretation either way. Note: the actual `resp2bin`/`bin2resp`/`bad.response` text-cleaning conversions are deferred to Phase 2, added to this file only once a Phase 2 function (CN/NRW/PF) actually needs them. |
| `swiftelse()` | Done | Tested. Scalar fast-path + `ifelse()` fallback for vectors. |
| `parallel_process()` | Done | Tested (sequential + a 2-core smoke test). `future`/`future.apply`/`progressr`-based, `on.exit`-guarded plan restoration. Replaces 3+ leak-prone `parallel::makeCluster` blocks. |
| `cosine()` | Done | Tested against a hand-computed value. Vectorized via `crossprod` (original had an unvectorized double loop); supports both the two-vector and whole-matrix forms. |
| `cohens_d()` | Done | Tested against a hand-computed value. Single implementation (original duplicated it in `response.analysis.R` and `utils-SemNeT.R`). |
| `bootstrap_SemNeT` schema — written up | Done | Documented in Conventions above; implementation deferred to Phase 4. |

**Verified:** `tests/testthat/test-helpers.R` (39 expectations) all pass;
`devtools::check()` → 0 errors, 0 notes, the same 1 expected (temporary) warning
carried over from Phase 0. `DESCRIPTION` `Imports` now: `methods, utils, future,
future.apply, progressr`. `stats::sd` importFrom added to `R/SemNeT-package.R`.

## Phase 2 — Core, dependency-free network math

| Original | New file | Status | Depends on | Notes |
|---|---|---|---|---|
| `similarity.R` | `similarity.R` | Done | Phase 1 | Always returns a matrix now (was matrix/`dist`/`cor` depending on `method`). The 4 contingency-table methods (angular/faith/jaccard/rr) are now one vectorized whole-matrix computation via `crossprod()` + column sums, replacing the original's per-pair for-loop — verified bit-for-bit identical against the original loop. Dropped `"phi"`'s dead, unreachable formula branch; `"phi"`/`"cor"` both just call `cor()` (mathematically identical for binary data, which is in fact all the original ever actually executed). |
| `TMFG.R` | `tmfg.R` | Done | Phase 1 | Verified bit-for-bit identical to the original for both `depend = FALSE` and `depend = TRUE`. Collapsed the 6x duplicated tetrahedron if/else into one loop over `combn(4, 2)`; vectorized the inner per-triangle max/argmax via `apply()`; vectorized both O(n²) fill loops (sparse-to-dense adjacency, and copying signed weights back onto retained edges) via matrix indexing. **Real bug fixed**: `for(e in 5:n)` iterated *backwards* (`5:4`) whenever `n == 4`, corrupting the result — now guarded so it only runs when `n > 4`. **Real bug fixed**: `n < 4` produced silent `NA` corruption (`order(...)[1:4]` on 3 elements) — now a hard `stop()`, since TMFG's tetrahedron genuinely requires 4 nodes. Dropped the `n < 9` `print()` (which never surfaced through R's condition system) in favor of a catchable `warning()` — deliberately *not* a hard error, since TMFG is still mathematically well-defined below 9 nodes and a hard stop would break legitimate small-network use that worked before. Dropped the `cliques`/`M`/`nodeTO`/`nodeFROM`/`nodeWEIGHT` variables entirely — confirmed dead code, never read after being built. |
| `Q.R` | `q.R` | Done | Phase 1 | Added validation (had none). Superseded shortly after by the vendored-C rewrite below — no longer depends on `convert2igraph`/`igraph` at all; see the Phase 2 addendum. |
| `ASPL.R` | `aspl.R` | Done | Phase 1 (`distance`) | Validates before mutating (was: mutate via `distance()`, then check square). **Real bug fixed**: `distance()` silently zeroed unreachable node-pair distances to 0 before returning, and `ASPL` then averaged those in as if the pair were *adjacent* — this systematically understated ASPL for any network with even one disconnected pair. `distance()` now returns real `Inf` for unreachable pairs, and `aspl()` excludes them from the average entirely (verified: identical to the original on fully-connected networks; on a deliberately disconnected 2-clique test network, old = 0.4286, fixed = 1, the mathematically correct value). Per-node ASPL and eccentricity (computed, then discarded, in the original) are now attached as the `"node"`/`"eccentricity"` attributes instead of being thrown away. |
| `CC.R` | `cc.R` | Done | Phase 1 | Validates before mutating (was: `diag(A) <- 0` before checking `nrow == ncol`). **Real bug fixed**: the weighted branch's `A^(1/3)` returns `NaN` for every negative edge weight, since R computes fractional powers via `exp(log(x) * p)` and `log()` of a negative number is undefined — silently broke weighted CC on any signed network (e.g. a TMFG built on raw correlations). Fixed via a real-valued signed cube root (`sign(A) * abs(A)^(1/3)`). Per-node CC (computed, then discarded, in the original as `CCi`) is now attached as the `"node"` attribute. |
| `CN.R` | `cn.R` | Done | Phase 1, convert2igraph | Uses the new `ordered_response_matrix_error()` (see Phase 1 addendum below). Internal subroutines (`cooccur`, `rel_freq`, `stat_cooccur`, `gtom`, `enrich_network`) kept private to this file since none are used elsewhere; `rel_freq` vectorized via `sapply()`/`rowSums()` in place of a nested per-word/per-participant for-loop. Verified bit-for-bit identical to the original `CN()` on the bundled `open.clean`/`open.group` fixtures. |
| `NRW.R` | `nrw.R` | Done | Phase 1 | **Real bug fixed**: the original used `max(range(data)) >= 1` to decide whether numeric input needed conversion, which rejects virtually every valid ordered response matrix (order values are 1, 2, 3, ... and so are almost always ≥ 1) — `CN`'s equivalent check correctly used `== 1`. Both now share one correct helper, `ordered_response_matrix_error()`. Verified bit-for-bit identical to the original `NRW()` on the bundled fixtures at both default and `type = "prop"` settings; verified the fixed bug by successfully running an ordered numeric matrix through `nrw()` (the original would have errored). |
| `PF.R` | `pf.R` | Done | Phase 1 (`responses_to_binary`, `binarize`) | Dropped the `philentropy` dependency entirely — Chebyshev distance is exactly `stats::dist(x, method = "maximum")` (verified numerically identical), which `stats` already provides. Renamed the confusing reused loop variable `i` (outer `for(i in 1:length(uniq.D))` vs. inner `for(i in 1:nrow(pf))`) to distinct names for clarity; on inspection this was **not actually a live bug** (the outer `i` was never read inside the loop body, only used as an iteration budget), so behavior is unchanged — converted to a `while(nrow(edges) != 0)` loop, which also skips the original's wasted no-op iterations once edges were exhausted. Dropped the `pfs` sparse-edge accumulator entirely — confirmed dead code, built via repeated `rbind()` but never read. The pre-binarization weighted network (computed, then discarded, in the original) is now attached as the `"weighted"` attribute. Verified bit-for-bit identical to the original `PF()` on `open.binary`. |
| `semnetmeas.R` | `semantic_network_measures.R` | Done | aspl, cc, q | Replaced the `get()`-by-name/`environment()` metaprogramming (which silently depended on this function's local variable names matching the requested measure names exactly) with a plain named-function lookup list + `vapply()`. Renamed measures to lowercase (`"aspl"`/`"cc"`/`"q"`) for consistency with the new function names; the legacy `semnetmeas()` wrapper (Phase 5) will translate the old uppercase names both ways. |
| `randwalk.R` | `random_walk.R` | Done | Phase 1 (`parallel_process`, `distance`) | Replaced `substitute()`-based name-sniffing with explicit `name_a`/`name_b` arguments (works correctly under `do.call()`, per the no-substitute() convention). Vectorized the transition-matrix construction (`TA[i,j] <- A[i,j]/degA[i]` in a double for-loop) into one `A / degree_a` division, verified identical to a hand-looped reference. Benefits from the `distance()` fix above: an unreachable node pair now correctly yields `exp(-Inf) == 0` similarity instead of the original's `exp(-0) == 1` (a truly unreachable pair no longer looks *maximally* similar). **Fixed the mixed string/numeric `$short` output**: the original built `$short` as a single character matrix (forced, since a matrix requires one type, and it mixed numeric p-values with `"< .001"`/direction-label strings), requiring every downstream consumer to re-parse numbers out of strings — `$short` is now a proper data.frame with genuinely numeric p-value columns and character direction columns; the `"< .001"` string substitution was dropped (numeric values are kept as-is) since it was the actual root cause. Uses one `parallel_process()` call per step-repetition level (not fully flattened into a single call across all levels — a flattened version would save some repeated `future` plan setup/teardown, but the reshaping logic needed to recover per-level groupings afterward adds correctness risk for a marginal gain, so was not pursued). |
| `randnet.test.R` | `random_network_test.R` | Done | Phase 1, semantic_network_measures | Replaced `substitute()`-based name-sniffing with a requirement that every network passed via `...` be named (e.g. `random_network_test(low = low_net, high = high_net)`), enforced in `random_network_test_errors()`. Split into validate → simulate (`generate_random_network()`, via `parallel_process()`) → format (`random_network_significance()`, returning one data.frame per network instead of a mixed-type matrix). Dropped `randnet()`'s degree-preserving/`A =` branch — confirmed used only by the deferred Shiny app, never by this function; `random_network_test()` only ever needed the `nodes =`/`edges =` (Erdos-Renyi) branch. |
| `convert2igraph.R` | `convert2igraph.R` | Done | — | Dropped the `qgraph` round-trip (and the already-dead `neural` parameter) for a direct `igraph::graph_from_adjacency_matrix()` call, per the `EGAnet` precedent. Verified identical vertex/edge counts and identical weighted adjacency matrix versus the qgraph-based original. **Caught on a follow-up comparison against `EGAnet::convert2igraph()`**: an earlier version of this function restored the matrix's original diagonal value right before a call using `diag = FALSE` — which makes the diagonal's value irrelevant regardless, so the restore was dead code (confirmed harmless, not a behavior bug, by re-verifying 0 diff against the original both before and after removing it). Also added `suppressWarnings()` around the `igraph` call itself, matching `EGAnet::convert2igraph()`'s own defensive wrapping — justified here by the fact that the *original* `CN.R` already needed to suppress warnings from this exact conversion. |
| `convert2cytoscape.R` | `convert2cytoscape.R` | Done | — | Vectorized the per-node name-substitution loop, which in the original silently coerced the entire sparse matrix from numeric to character one node at a time (a matrix requires one column type, so the first character assignment into any column recast the whole matrix) — now built once, with the zero-weight filter applied on the pre-coercion numeric weights so the boolean mask can't be corrupted by the later type change. Verified bit-for-bit identical output to the original. The "shared sparse-triplet builder with `tmfg`" noted in the original plan turned out not to apply: `TMFG.R`'s own `nodeTO`/`nodeFROM`/`nodeWEIGHT` sparse-triplet construction was itself confirmed dead code (built, never read) and was dropped in `tmfg.R` above, so there was no live code left to share. |

**Phase 1 addendum (correction):** on closer implementation, `response_matrix_error()` (Phase 1) and the numeric-response needs of `cn()`/`nrw()` turned out to require *opposite* rules, not one shared rule as originally scoped — `response_matrix_error()` fits `pf()`'s pattern (character responses, or numeric that must already be binary), while `cn()`/`nrw()` need the reverse (character responses, or numeric that must *not* already be binary, since co-occurrence windowing needs response order). Added a second, correctly-scoped validator, `ordered_response_matrix_error()`, to `helpers.R` for `cn()`/`nrw()` specifically; `response_matrix_error()` is unchanged and still used by `pf()`. Also added to `helpers.R` in this phase (used by ≥ 2 Phase 2 files, per the cross-cutting rule): `binarize()`, `distance()`, `bad_response()`, `responses_to_binary()` (was `resp2bin`), `binary_to_responses()` (was `bin2resp`).

**Verified:** `tests/testthat/` (161 expectations across all Phase 0-2 files) all pass;
`devtools::check()` → 0 errors, 0 notes, the same 1 expected (temporary) Rd
cross-reference warning carried since Phase 0 (`sim.fluency`/`bootSemNeT` links,
resolves once Phases 3-4 build those legacy names). `DESCRIPTION` `Imports` now
adds `stats` and `igraph`. Every bit-for-bit-equivalence claim above was checked
empirically against a verbatim port of the corresponding original function, not
just reasoned through.

**Naming question resolved:** all of `tmfg`/`aspl`/`cc`/`cn`/`nrw`/`pf`/`q` stay as
lowercase acronyms — confirmed by direct instruction, no changes needed.

## Phase 2 addendum — vendored compiled Louvain/modularity (`q()`)

Per direct instruction, `q()` no longer calls `igraph::cluster_louvain()`. Instead,
`SemNeT` now vendors `{EGAnet}`'s own compiled C implementation of Louvain community
detection and Gomez et al. (2009) signed modularity (same author, `EGAnet/src/`) —
this package is no longer `NeedsCompilation: no`.

**Files added:**
- `src/louvain.c`, `src/modularity.c`, `src/modularity.h`, `src/signed_modularity.c`,
  `src/xoshiro.c`, `src/xoshiro.h`, `src/nanotime.c`, `src/nanotime.h` — copied verbatim
  from `EGAnet/src/`, each with a one-line provenance comment at the top.
- `src/init.c` — SemNeT's own routine registration (only `r_louvain` and
  `r_signed_modularity`; `EGAnet`'s other `.Call` entry points, e.g. the `r_xoshiro_*`
  RNG utilities and `r_polychoric_correlation_matrix`, aren't needed here and weren't
  registered, though `xoshiro.c` was still copied in full for fidelity to the source file).
- `R/q.R` gained two new private (`@noRd`) subroutines, `louvain()` and
  `signed_modularity()` — thin `.Call()` wrappers, kept local to this file (not
  `helpers.R`) since nothing else uses them yet, per the one-file-per-function convention.
- `R/SemNeT-package.R`-level `@useDynLib SemNeT, .registration = TRUE` roxygen tag
  (declared on `q()` since that's the only current consumer) generates the
  corresponding `NAMESPACE` entry.

**Why:** three concrete problems this fixes, found and verified during implementation:
1. **Reproducibility.** `igraph::cluster_louvain()` draws on R's global RNG, so
   `q()`'s result depended on unrelated code executed earlier in a session, call
   order, and parallel worker. The compiled version's randomness depends only on an
   explicit `seed` argument (verified: identical `q()` output across 3 different
   preceding `set.seed()` states, using the same `seed = 42`). This directly benefits
   Phase 4's bootstrap loop, which will call `q()` (via `semantic_network_measures()`)
   under `parallel_process()` many times.
2. **Signed modularity.** Added `q(A, signed = TRUE)` (Gomez et al., 2009), which
   scores modularity while discounting negative edges rather than folding them into
   `abs()`. Verified against `EGAnet::modularity()` called directly on the same
   network/partition — bit-for-bit identical, both for the default unsigned score and
   the signed one.
3. **Speed** (not independently benchmarked in this session, but expected from
   compiled C vs. an R-level `igraph` call at bootstrap-scale iteration counts).

**A real bug caught during verification, not shipped:** the first version of `q()`
passed the network directly to the compiled routines including its diagonal — TMFG's
own `diag(A) <- 1` convention. Both `modularity_values()` and `signed_modularity()`
treat every nonzero cell (diagonal included) as a real edge, which inflated the
unsigned Q from 0.370 to 0.472 and produced a wildly implausible signed value (1.65,
outside modularity's normal range) on the same test network. The original's
`igraph`-based path avoided this silently via `convert2igraph()`'s `diag = FALSE`.
Fixed by explicitly zeroing the diagonal in `q()` before either C call — verified the
unsigned result then matches the old `igraph`-based value exactly (0.3703447 both ways).

**New `q()` signature:** `q(A, resolution = 1, seed = NULL, signed = FALSE)` — all
three new arguments are optional with backward-compatible defaults, so `q(A)` alone
is unaffected apart from the reproducibility change above.
`semantic_network_measures()` gained a passthrough `seed` argument for the same
reason (its own internal `q()` call needed a way to be made reproducible, now that
`set.seed()` no longer has any effect on it). `random_network_test()`'s repeated
internal `semantic_network_measures()` calls deliberately keep `seed = NULL` (the
default) — that loop *wants* independent randomness across its null-distribution
draws, not a fixed seed forcing the same partition every time.

**Verified:** all 169 test expectations pass (11 new, covering seed-reproducibility,
the diagonal fix, and the signed/unsigned distinction); `devtools::check()` →
0 errors, 0 notes, the same 1 pre-existing expected warning; `checking compiled code`
and `checking line endings in C/C++/Fortran sources/headers` both pass cleanly.

## Phase 0-2 line-by-line review (before starting Phase 3)

Per request, re-read every Phase 0-2 file with a specifically adversarial eye (not
just re-running the existing tests) before moving on. Found and fixed 6 real bugs,
5 of them a single recurring pattern that's now been checked for everywhere in the
codebase: **an R integer range like `1:n` or `1:(n-1)` silently counts *backwards*
when its upper bound is 0 or negative** (`1:0` is `c(1, 0)`, not empty), either
corrupting output silently or indexing out of bounds. Four instances of this were
inherited unmodified from the original package (never previously caught); one was
newly introduced during this refactor's own vectorization work.

1. **`stat_cooccur()` (`cn.R`), inherited bug**: with only 0 or 1 unique words in
   the whole vocabulary, `1:(n_words - 1)` and the nested `(i + 1):n_words` both
   go negative/reversed, indexing outside the 1x1 (or 0x0) adjacency matrix and
   crashing with "subscript out of bounds" instead of returning the
   already-correct all-zero result. Fixed by guarding the double loop with
   `if(n_words > 1)`.
2. **`simulate_walk()` (`random_walk.R`), inherited bug**: `random_walk(A, B,
   steps = 1)` makes `n_steps == 1`, so `2:n_steps` (`2:1`) runs the loop body
   twice in reverse instead of zero times -- silently overwriting the just-recorded
   starting node with an unrelated later step, corrupting every downstream
   similarity calculation for that step level. Verified concretely: a length-1
   numeric vector's first element got overwritten via this exact pattern in
   isolation before confirming it in the full function. Fixed by guarding with
   `if(n_steps > 1)`.
3. **`generate_random_network()` (`random_network_test.R`), inherited bug**: an
   observed network with 0 edges (`edges <- floor(sum(observed) / 2)` gives 0)
   makes `1:edges` become `c(1, 0)`; since a 0 index is silently dropped rather
   than erroring, this selected **1 edge instead of 0**, silently biasing the
   null-model comparison for a degenerate (edgeless) input. Fixed with
   `seq_len(edges)`, which is correctly empty at 0.
4. **`rel_freq()` (`cn.R`), a bug newly introduced in this refactor's own
   vectorization**: with exactly 1 participant, `sapply(vocabulary, function(word)
   rowSums(mat == word, na.rm = TRUE) > 0)` has every per-word result collapse to
   a length-1 logical, and `sapply()` then returns a plain vector instead of a
   (participants x vocabulary) matrix -- which `colMeans()` immediately after
   rejects with `'x' must be an array of at least two dimensions`. Confirmed
   `vapply()` has the identical pitfall (it also collapses when `FUN.VALUE` has
   length 1) before settling on explicitly reshaping via
   `matrix(sapply(...), nrow = nrow(mat), ncol = length(vocabulary))`, which is
   robust regardless of participant count. This is the one bug in this review
   that wasn't in the original code at all -- the original's nested for-loops had
   no equivalent failure mode.
5. **`convert2igraph()`, dead code (not a behavior bug, caught comparing against
   `EGAnet::convert2igraph()`)**: restored the matrix's original diagonal value
   right before a call using `diag = FALSE`, which makes the diagonal irrelevant
   regardless of its value -- confirmed the restore had zero effect (0 diff
   against the original both before and after removing it) before deleting it.
   Also added `suppressWarnings()` around the `igraph` call itself, justified by
   the fact the *original* `CN.R` already needed to suppress warnings from this
   exact conversion.
6. **`nrw_errors()`, dead/misleading condition (not a behavior bug)**: verified
   empirically that `missing(type)` inside `nrw_errors()` always reads `FALSE`,
   because `nrw()` always explicitly forwards `type` to it (even when `nrw()`'s
   own caller left it at the default) -- `missing()` only reflects the immediate
   call, never upstream provenance. The `!missing(type) &&` clause therefore
   always evaluated to `TRUE` and did nothing; removed it, keeping the actually-
   meaningful `length(type) == 1` check.

**Also added** (robustness gap, not a reachable bug in current code): a 0x0 matrix
passed `adjacency_matrix_error()`'s square check vacuously (0 rows == 0 columns);
every downstream network function would then silently degrade to an empty result
instead of getting a clear error. Now rejected explicitly with "must have at least
1 node."

**Audited every other `1:length()`/`1:nrow()`/`1:ncol()` pattern in the codebase**
(`pf.R`, `nrw.R`, `tmfg.R`, `helpers.R`, `convert2cytoscape.R`) for the same
reverse-range risk -- each of the rest is either mathematically guaranteed >= 1 in
context (e.g. `tmfg.R`'s `combn(4, 2)` always has exactly 6 columns; a "tied at
minimum" search in `pf.R` always matches at least itself) or degrades to a
harmless no-op when it could theoretically hit 0 (confirmed for `nrw.R`'s and
`cn.R`'s `cooccur()`'s empty-vocabulary edge cases specifically, by tracing
through what every downstream operation does with the resulting empty/NA values).

**Closed a test-coverage gap**: `distance()`, `binarize()`, `bad_response()`,
`responses_to_binary()`, `binary_to_responses()`, and
`ordered_response_matrix_error()` (all added to `helpers.R` in Phase 2) previously
had only indirect integration coverage through `aspl`/`cc`/`random_walk`/`nrw`/
`cn`/`pf`'s own tests, no direct unit tests of their own. Added 6 new test blocks
covering each directly, including the round-trip between `responses_to_binary()`
and `binary_to_responses()`.

**Verified:** all 194 test expectations pass (25 new: 6 regression tests for the
bugs above, plus the direct helper-coverage tests); `devtools::check()` →
0 errors, 0 notes, the same 1 pre-existing expected warning.

## Phase 0-2 optimization pass (before starting Phase 3)

Per request, went function by function benchmarking candidate optimizations
against the current implementation (using `microbenchmark` for isolated timings,
manual `Sys.time()` for full end-to-end calls), applying only what was measured
faster *and* validated to produce identical output. Every win below was checked
against either the original SemNeT function or a hand-computed/reference value,
not just "looks like it should be faster."

**Applied (validated wins):**

| Change | File | Result |
|---|---|---|
| `swiftelse()`'s vector branch: direct logical-index assignment into a pre-typed result vector, instead of falling back to `ifelse()` | `helpers.R` | ~2.3x faster (common no-`NA` case); falls back to `ifelse()` itself whenever `NA` is actually present, since a naive version (matching `{EGAnet}`'s own) **errors outright** on `NA` conditions (`ifelse()`'s NA-propagation can't be replicated via plain index assignment) -- caught by testing the NA case explicitly before adopting |
| `get_object_type()`: check `is.matrix()` before `is.data.frame()` | `helpers.R` | ~2.3x faster for the common case (this validator runs on every exported function's every call); safe because the two checks are mutually exclusive, so reordering doesn't change which type wins |
| `1:n` / `1:length(x)` / `1:nrow(x)` / `1:ncol(x)` → `seq_len()`/`seq_along()` sweep across every file | `tmfg.R`, `cc.R`, `cn.R`, `pf.R`, `nrw.R`, `convert2cytoscape.R`, `helpers.R` | ~15-20% faster construction (`seq_len` vs `:`, benchmarked), and permanently forecloses the reverse-iteration bug class fixed earlier this session -- the performance delta here is real but small; the value is mostly the closed-off bug class |
| `pf()`'s `same_cluster` computation: vectorized matrix indexing instead of a per-edge loop | `pf.R` | ~3-4x faster; safe because every row reads the same unchanging `clusters` snapshot (that batch's edges haven't been merged in yet) -- confirmed independent before vectorizing |
| `cn()`'s `cooccur()`: loop restructured from word-outer/participant-inner to participant-outer, iterating only over the (small) set of words *that participant actually said* | `cn.R` | ~2.5x faster on the bundled `open.clean` fixture (dominated ~95% of `cn()`'s total runtime beforehand); validated identical output including a hand-built repeated-word edge case, and re-verified bit-for-bit against a fresh port of the original `CN()`/`stat.cooccur()` after the change |
| `random_walk()`: precompute each transition matrix's row-wise cumulative sum once, outside `simulate_walk()`, instead of recomputing `cumsum()` on every single step of every single walk | `random_walk.R` | Isolated line: ~20-25% faster. End-to-end (the number that actually matters, since `cumsum` is only part of each step's cost): ~5-10% faster on the whole walk loop, benchmarked directly against the pre-change logic reconstructed inline. `simulate_walk()` is by far this package's hottest code path (`iter` x number of step levels calls per `random_walk()` invocation, easily tens of millions of steps total at typical settings), so even a modest per-step win compounds |
| `q()`'s internal `louvain()`/`signed_modularity()`: skip `as.matrix()` when the input is already a matrix | `q.R` | `as.matrix()` is a generic with real `UseMethod()` dispatch cost -- benchmarked ~10x slower than a direct `is.matrix()` check for this exact no-op case. Safe here specifically because these are `@noRd` internal functions with exactly one caller (`q()`), which always hands them an already-validated matrix -- matches the instruction that internal-only functions can bypass guards general-purpose exported functions can't |

**Benchmarked and explicitly rejected** (the point of benchmarking *before*
committing, not after):

- **`cc()`'s unweighted per-node loop → matrix-power vectorization** (`diag(A %*% A %*% A)`, the same trick already used for the weighted branch). Verified mathematically identical and ~2.5x faster at n=50 nodes -- but ~equal-to-slightly-worse at n=200, because TMFG networks are sparse by construction (a constant ~3n-6 edges regardless of n) while dense matrix multiplication is O(n^3) regardless of sparsity. The existing loop-based approach (bounded by actual degree, not node count) already fits the real workload better at realistic-to-large network sizes. Not applied.
- **`which(cumsum(x) > draw)[1]` → `findInterval(draw, cumsum(x)) + 1`** in `random_walk()`'s CDF sampling. `findInterval()`'s binary search only pays off at much larger n than any realistic fluency-network size (benchmarked *slower* at n=10-100: ~2.5-3μs vs ~3.5-4μs). It also disagrees with `which()` at the exact boundary `draw == 1` (returns an out-of-range index instead of `NA`) -- a second, independent reason not to adopt it. Not applied.
- **`pf()`: splitting the network-write from the cluster-merge loop** (vectorizing the two `pathfinder_network[...] <- weight` lines outside the loop, leaving only cluster-merging inside it). Verified identical output, but measured *no improvement* (`68μs` vs `73μs`, i.e. slightly worse) -- the extra `cbind()`/index-vector construction outweighed removing two assignments from a typically-small loop. Not applied.

**Not touched** (already optimal for their actual use, or the loop is
irreducibly sequential): `nrw()`'s co-occurrence loop was already
participant-outer with no wasted full-vocabulary sweep (unlike `cooccur()`
before its fix), so the same restructuring doesn't apply; `distance()`'s
weighted (Dijkstra) branch and `pf()`'s cluster-merge loop are both
inherently sequential (each step's decision depends on the previous one) and
would need a compiled rewrite to meaningfully speed up further, which wasn't
in scope for this pass.

**Verified:** all 194 tests still pass after every change above; `devtools::check()`
→ 0 errors, 0 notes, the same 1 pre-existing expected warning; re-ran a fresh
bit-for-bit comparison of `cn()` against a newly-ported copy of the original
`CN()`/`stat.cooccur()`/`cooccur()`/`rel.freq()` after the heaviest rewrite
(`cooccur()`) -- still exact.

## Phase 3 — Pipeline building blocks

| Original | New file | Status | Depends on | Notes |
|---|---|---|---|---|
| `sim.fluency.R` | `simulate_fluency.R` | Done | — | Validated `nodes`/`cases`/`random`. Vectorization explicitly deferred this phase (per instruction: correctness first, optimize later) — the per-node sampling loop is unchanged from the original's structure. **Real bug fixed**: with `random = FALSE` (the default) and `nodes` greater than `length(animals.freq)` (367), the original fed a too-short `lambda` into `rpois(nodes, samp.freq)`, which recycles with R's own confusing "longer object length is not a multiple of shorter object length" warning instead of a clear error — now a real validation error explaining the fix (`random = TRUE`). Verified bit-for-bit identical to the original for `nodes <= 367` at a fixed seed. |
| `response.analysis.R` | `response_analysis.R` | Done | Phase 1 (`cohens_d`), `responses_to_binary` | Generalized to true variadic `...`: both underlying tests (two-sample *t*-test, McNemar's test) are inherently pairwise, so N > 2 groups now returns every pairwise comparison (named `"<group1>_vs_<group2>"`) rather than inventing an N-group statistic; the 2-group case returns the single comparison directly, unchanged in shape from the original. Now shares `cohens_d()` instead of a locally re-implemented, identical `d()` function. Verified bit-for-bit identical to the original on `open.clean` (`Low` vs. `High`) for the 2-group case; verified the 3-group case produces exactly 3 pairwise comparisons. |
| `equate.R` | `equate.R` | Done | — | **Real, severe bug fixed**: the original decided whether two groups needed equating by comparing their vocabulary *sizes* (`length(colnames(rmatA)) != length(colnames(rmatB))`), not their actual shared words. Confirmed concretely: two groups with the same number of responses but almost no actual overlap (e.g. 3 responses each, only 1 shared) got **zero equating applied** — the while-loop condition was `FALSE` from the very first comparison, so nothing was ever filtered. This isn't a rare edge case; any two roughly similarly-sized groups (common in real balanced-group research designs) silently received unequated, misaligned data. Separately, even when the length-based approach did trigger, the two groups' results could end up in **different column orders** (each retained its own original column order after filtering), silently breaking any downstream code assuming column *i* means the same response in every group. Fixed by computing the shared vocabulary directly via `Reduce(intersect, ...)` in one pass (not the original's iterative pairwise fold-then-rebroadcast) and subsetting every group to that identical, sorted vocabulary — guaranteeing both correctness and consistent column order. This one implementation replaces the original's three near-duplicates (`equate()`, `utils-SemNeTShiny.R`'s `equateShiny()`, and `tmfg_setup()`'s inline copy) — Phase 4's `bootstrap_SemNeT`/`permutation_SemNeT` will call this `equate()` directly instead of a separate Shiny-internal copy. |
| `finalize.R` | `finalize.R` | Done | — | Added the missing guard: if no response meets `minimum_cases`, the original silently returned a useless 0-column matrix; now a clear error. **Naming judgment call, flagging for visibility**: `finalize()`'s exported name is unchanged, so (per the Conventions above) it gets no `R/legacy.R` wrapper — but its original argument names were `rmat`/`minCase`. I renamed them to `data`/`minimum_cases` for snake_case consistency with the rest of the new code, which means an old call using the *named* arguments `finalize(rmat = x, minCase = 3)` would break (positional calls, e.g. `finalize(x, 3)`, are unaffected). This is the first real instance of that tension in the refactor — `similarity()`/`convert2igraph()`/`convert2cytoscape()` never had to make this call since their original argument names already happened to be snake_case-compatible. Flagging for override if full named-argument compatibility is wanted here instead. |
| `compare_nets.R` | `compare_networks.R` | Done | `binarize` | Ported near-verbatim as planned (it's a `qgraph`-based plotting function, not something to rewrite the internals of). Re-added `qgraph` to `Imports` — this is the one function that actually needs it for plotting, per the binding decision's own carve-out. Added the cross-network consistency check the plan asked for: `placement = "match"` averages every network's layout together via `qgraph::averageLayout()`, which is meaningless unless every network has the same node count — now a clear validation error instead of a confusing failure inside `averageLayout()` itself. Tested via a null (`pdf(NULL)`) graphics device for 2 networks, 3+ networks, and `placement = "match"`, since this function's output is a plot, not a comparable return value. |

**Verified:** all 240 tests pass (46 new); `devtools::check()` → 0 errors, 0 notes,
the same 1 pre-existing expected warning (still resolves once Phase 5's
`legacy.R` builds the `sim.fluency`/`bootSemNeT` names). `DESCRIPTION` `Imports`
now also adds `graphics` and `qgraph`. Per direct instruction, this phase
prioritized correctness over performance throughout — no vectorization or
benchmarking was attempted here; that pass (matching the one already done for
Phases 0-2) is deferred to later, if wanted.

## Phase 3 optimization pass (before starting Phase 4)

Same discipline as the Phase 0-2 pass: benchmark each candidate against the
current implementation with `microbenchmark`, apply only what's measured
faster *and* validated to produce identical (or, where randomness is
involved, provably equivalent-contract) output.

Phase 3's functions are mostly one-shot pipeline/plumbing steps (called once
per analysis, not in a tight iteration loop), so the surface for a real win
was much smaller than Phases 0-2's per-node/per-edge network math. One
genuine, verified win did turn up, applied twice; everything else was
checked and correctly has no room to improve, or would trade away a
correctness guarantee for no real benefit.

**Applied (validated win):**

| Change | File | Result |
|---|---|---|
| `sort(x)` → `sort(x, method = "radix")` for the shared-vocabulary computation | `equate.R` | ~20x faster (4.6ms → 0.2ms at a 1500-word vocabulary, `microbenchmark`, n=200). Verified byte-identical ordering to the locale-aware default on every real word in the bundled fixtures (`colnames(open.binary)`, 367 words) — the two methods only disagree on ordering mixed-case/non-ASCII strings (confirmed with a synthetic "Zebra"/"Banana"/`123start` case), which this package's verbal-fluency vocabularies never contain. Safe regardless, since `equate()`'s contract is only "every group gets the *same* column order," not a specific alphabetization |
| Same `sort(..., method = "radix")` swap for `response_analysis_pair()`'s internal vocabulary vector | `response_analysis.R` | Same ~20x win at comparable vocabulary sizes. Even safer here than in `equate()`: this vocabulary only labels the rows of an internal indicator matrix used to build the McNemar contingency table — every downstream statistic (`colSums`, the 2x2 table, the test itself) is order-invariant, so any consistent ordering produces identical results |

**Benchmarked and explicitly rejected:**

- **`simulate_fluency()`'s per-node `sample()` loop → a single vectorized `rbinom()` draw across all nodes at once.** Benchmarked essentially a wash (~5.7ms loop vs. ~6.8ms vectorized at nodes=300/cases=500, `microbenchmark` n=50 — the vectorized version was not reliably faster). More importantly, it also broke bit-for-bit reproducibility: `set.seed(42)` + the loop vs. the vectorized version produce different matrices, because they consume R's RNG stream in a different order/shape. Since `simulate_fluency()`'s whole point is a `set.seed()`-reproducible simulation matching the original algorithm exactly (already relied on by `test-simulate_fluency.R`'s fixed-seed test), this would trade away a real guarantee for zero measured benefit. Not applied.

**Checked, no viable optimization found:**

- **`finalize()`** — body is `colSums(data) >= minimum_cases` + `which()`, both already single-pass C-level vectorized operations on the whole matrix; benchmarked at ~190μs for a 500x300 matrix, indistinguishable from the theoretical floor for that operation. Nothing to restructure.
- **`compare_networks()`** — its own loops iterate once per *network* (realistically 2-6), not once per node/edge; runtime is entirely dominated by `qgraph::qgraph()`'s own layout and rendering cost, which is out of this function's control. Not touched.

**Verified:** all 240 tests still pass after both applied changes; `devtools::check()`
→ 0 errors, 0 notes, the same 1 pre-existing expected warning.

## Phase 4 — Bootstrap/comparison pipeline

| Original | New file | Status | Depends on | Notes |
|---|---|---|---|---|
| `bootSemNeT.R` | `bootstrap_SemNeT.R` | Done | Phases 1-3 | One `parallel_process()` call per group (full similarity → network → measures pipeline per sample) instead of the original's 3 separate `makeCluster`/`stopCluster` stages. Named `...` groups, `match.arg` for `method`/`type`, dead `weighted` param dropped entirely. Calls the canonical (fixed) `equate()`/`finalize()` for `tmfg` + `type = "case"`, not a separate Shiny-internal copy. Per-method input validation added: `tmfg` needs numeric/binary data (raw word responses are auto-converted via `responses_to_binary()`), `cn`/`nrw` need genuine response order and reject an already-binary matrix, `pf` accepts either. **Real bug found and fixed**: the original's own comment said its retry-on-bad-sample check was for "no variance," but the code only ever tested for an all-*endorsed* column, silently missing the equally-invalid all-*unendorsed* case — broadening the check was tried and found to reject almost every realistic case-resample (rare responses are commonly entirely absent from a bootstrap draw by chance), since `finalize()` already strips those out immediately afterward; the original's narrower check turned out to be correct, just under-explained, so it was kept and documented instead. Network estimation now goes through one `do.call()`-based dispatcher shared by every method, eliminating the entire class of copy-paste, per-method argument bugs found in `permSemNeT` (see below) before it could recur here. Returns the locked schema with class `bootstrap_SemNeT`. |
| `test.bootSemNeT.R` | `bootstrap_test_SemNeT.R` | Done | bootstrap_SemNeT | **Real bug fixed**: `match.arg(measures)` was missing `several.ok = TRUE`, so requesting the function's own documented default (all 3 measures at once) errored. The original's `length(names)` vs. `length(name)` bug (a typo referencing the base `names()` function instead of the local `name` variable, silently corrupting column-slicing offsets) is moot by construction — the whole fragile, regex/position-based table-reshaping approach it lived in was replaced with a single, uniform builder keyed by the new schema's real group names. Every test type (`t-test`/`ANOVA`/`ANCOVA`) now returns the exact same 3-key shape per measure (`table`/`post_hoc`/`adjusted_means`), instead of the original's incompatible top-level shape per test. Partial eta-squared is now computed by matching the ANCOVA/ANOVA term *name* (`"Residuals"`, `"Edges"`) instead of the original's positional index-dropping, which silently produced no eta-squared at all for a plain single-factor ANOVA. Adjusted means now come from `as.data.frame(effects::allEffects(...)[[term]])` instead of manually indexing internal `$fit`/`$variables` fields, and correctly picks the fullest (e.g. interaction) term for a multi-factor design instead of the original's assumption that exactly one non-covariate term exists. ANCOVA is allowed with only 2 groups (a genuinely different test than a 2-group ANOVA, which is redundant with a t-test and still blocked). Effect sizes are reported as absolute `cohens_d()` (shared with `response_analysis()`) plus a separate text direction label, not the original's signed `d()`. Real S3 class `bootstrap_test_SemNeT` assigned. |
| `plot.bootSemNeT.R` | `plot.bootstrap_SemNeT.R` | Done | bootstrap_SemNeT | Consumes the new schema directly (`x$groups[[name]]$measures`), no regex name-recovery. **Dependency trade-off, flagging for visibility**: the original's plot was a hand-rolled `ggplot2`/`dplyr`/`magrittr` "raincloud" plot with a custom `ggproto` geom (~300 lines on its own) — replaced with a base-graphics `boxplot()` per measure, consistent with this refactor's aggressive dependency trim elsewhere (`qgraph` dropped from `convert2igraph`, `plyr`/`philentropy` dropped package-wide). Shows the same bootstrapped spread/median information without reintroducing 3 dependencies for one plot; say the word if the raincloud visual specifically is wanted back. |
| `permSemNeT.R` | `permutation_SemNeT.R` | Done | Phases 1-3 | **3 real bugs fixed**, all in the original's copy-pasted per-sample/per-method argument blocks: (1) the `thresold` typo meant `NRW`'s `threshold` argument was silently always `NULL` for permutation networks; (2) `enrich <- args$enrich` (assignment, not the intended `enrich = args$enrich`) as a bare, unnamed call argument meant `CN`'s `enrich` was never actually passed through either; (3) `net.list2`'s `NRW` call passed the threshold value into a parameter literally named `window`, not `threshold`, for `sample_2`'s networks only — a silent asymmetry between the two samples' network estimation. All three shared one root cause (hand-duplicated argument-passing code for each sample/method combination) and are structurally impossible now: one `do.call()`-based dispatcher (shared with `bootstrap_SemNeT`'s design) estimates both samples' networks through the exact same code path. Also removed a dead `lm(Measure ~ Nodes + Edges)` regression computation whose residual-based difference was computed and then immediately discarded, overwritten by the raw statistic difference two lines later. Real word responses are pooled and permuted *before* being converted to binary/equated (matching the original's actual pooling order), not after, since two groups' independently-binarized vocabularies can't be `rbind()`-ed together. Added `_errors()` validation, and **exported + documented for the first time** (was `@noRd` in the original). |

**Verified:** all 313 tests pass (49 new); `devtools::check()` → 0 errors, 0 notes,
the same 1 pre-existing expected warning (still resolves once Phase 5's `legacy.R`
builds the `sim.fluency`/`bootSemNeT` names). `DESCRIPTION` `Imports` now also adds
`car`, `broom`, and `effects` (needed for `bootstrap_test_SemNeT()`'s ANOVA/ANCOVA
type II/III sums of squares, tidy tables, and adjusted means -- the same
"genuinely needed" carve-out already applied to `qgraph` for `compare_networks()`).
Per direct instruction, this phase prioritized correctness over performance
throughout -- no benchmarking was attempted here; that pass is deferred to later,
if wanted.

## Phase 4 adversarial review (before starting Phase 4's own optimization pass)

Per request, gave Phase 4 the same adversarial re-read Phase 0-2 got before moving
on. Found and fixed 2 real bugs, both a variant of "a `...`-collected group's name
is validated for presence but not uniqueness":

1. **`bootstrap_SemNeT()`**: `samples[[group_name]]`/`results[[group_name]]` are
   both keyed by name, so `bootstrap_SemNeT(low = a, low = b, ...)` silently
   estimated the second `low`'s networks from the *first* `low`'s resampled data
   (name lookup returns the first match) and stored both under the same
   duplicated name in the output -- worse than merely losing data, since the
   output looks like two independent results when they're not. Concretely
   reproduced before fixing. Fixed with an explicit `anyDuplicated()` check --
   a duplicate name here is a real user mistake worth rejecting outright.
2. **`bootstrap_test_SemNeT()`**: two `...` objects that resolve to the same
   condition label (two case-resampled objects, or two node-drop objects at an
   identical `prop`) produced a duplicate name in `results`, and `$`/`[[`
   access -- the documented way to read a result out -- silently returned only
   the first, hiding the second entirely. Unlike the above, a duplicate label
   here isn't necessarily a user mistake (two different objects can legitimately
   share one), so this was fixed via `make.unique()` instead of rejecting it.

**Follow-up sweep** (per direct instruction, extended into Phases 2-3 once the
pattern was clear) found the identical "named but not unique" gap in every other
`...`-collecting function in the codebase:

3. **`equate()`**: two same-named groups collapsed onto one entry in the
   returned list via `$`/`[[` access. Fixed with the same uniqueness check.
4. **`response_analysis()`**: with 3+ groups, two same-named groups can produce
   two *different* pairwise comparisons landing on the identical
   `"<name>_vs_<name>"` label; `$`/`[[` access then hides one. Fixed.
5. **`random_network_test()`**: same shape via `Map()`'s name propagation onto
   the returned list. Fixed.
6. **`compare_networks()`**: added the same check for consistency, though this
   one turned out cosmetic-only on inspection -- every network here is indexed
   *positionally*, never by name, so a duplicate name can't cause silent data
   loss, only two identically-titled panels by default. Fixed anyway since the
   invariant ("every `...` name is unique") should hold everywhere it's checked.

**Also found, independently** (not the duplicate-name pattern): **`finalize()`**'s
own validator (`response_matrix_error()`) accepts raw, not-yet-binarized character
responses -- correct for `pf()`/`tmfg()`, which convert that themselves -- but
`finalize()`'s body calls `colSums()` directly, assuming numeric 0/1 content.
Passing raw responses crashed with a confusing base-R error (`'x' must be numeric
or complex`) instead of a clear one. Fixed with an explicit check and message.

Also fixed during this pass, from a fresh line-by-line re-read of Phase 4's own
files specifically: **`bootstrap_test_SemNeT()`'s custom-`formula` substitution**
(`gsub("y", "Measure", formula)`) corrupted any factor name merely *containing* a
"y" (e.g. `"yob"`), not just the standalone outcome placeholder -- e.g.
`"y ~ gf*yob"` silently became `"Measure ~ gf*Measureob"`. Fixed to a
word-boundary match (`gsub("\\by\\b", ...)`).

**Verified:** all 326 tests pass (13 new, one per bug above plus one extra
ANCOVA/Tukey coverage test); `devtools::check()` → 0 errors, 0 notes, the same 1
pre-existing expected warning.

## Phase 4 optimization pass

Same discipline as the Phase 0-3 passes: benchmark each candidate against the
current implementation with `microbenchmark`, apply only what's measured
faster *and* verified to produce identical output.

**Applied (validated wins):**

| Change | File | Result |
|---|---|---|
| `bootstrap_SemNeT()`'s TMFG no-variance retry check: `apply(candidate, 2, function(x) all(x == 1))` → `colSums(candidate) == nrow(candidate)` | `bootstrap_SemNeT.R` | ~20x faster (100 cases x 367 words: ~1.2ms → ~55us, `microbenchmark`, n=200) -- avoids an R-level function call per column. Verified identical results (safe because this only needs to hold for strictly binary data, which is guaranteed by this point). Runs on every one of `iter` generation attempts per group |
| `equate()`'s validation split out into a new `equate_core()`, called directly by `bootstrap_SemNeT()`'s `equate_bootstrap_samples()` instead of going through `do.call(equate, ...)` | `equate.R`, `bootstrap_SemNeT.R` | ~5x faster per call (~800us → ~155us, `microbenchmark`, n=300) -- skips re-running `equate_errors()`'s full validation (`response_matrix_error()`, column-name checks) on every one of `iter` draws, since these samples were already validated once, up front, by `bootstrap_SemNeT_errors()`/`finalize()`. `equate()` itself is unchanged and still validates every call |
| `bootstrap_test_SemNeT()`'s per-network edge count: `binarize(network)` + zero the diagonal + `sum()/2` → `(sum(network != 0) - sum(diag(network) != 0)) / 2` | `bootstrap_test_SemNeT.R` | ~4x faster over 200 networks (~6ms → ~1.6ms, `microbenchmark`, n=100) -- skips materializing a whole binarized matrix (plus restoring its row/column names) just to sum it and throw it away. Verified identical, mathematically equivalent (off-diagonal nonzero count / 2 either way) |
| `sample(seq_len(n), ...)` → `sample.int(n, ...)` in `bootstrap_SemNeT()`'s and `permutation_SemNeT()`'s resampling/permutation draws | `bootstrap_SemNeT.R`, `permutation_SemNeT.R` | ~15-20% faster (`microbenchmark`). Verified bit-for-bit identical RNG draws at a fixed seed for both `replace = TRUE` and `replace = FALSE` -- `sample(seq_len(n), ...)` already calls `sample.int(n, ...)` internally and then indexes back into the materialized `seq_len(n)` vector, so this just skips that redundant indexing step, not a different algorithm |

**Benchmarked and explicitly rejected:**

- **`bootstrap_iteration()`/`permutation_iteration()`'s `do.call()` dispatch → a direct function call.** Benchmarked essentially a wash (~3.7ms either way at a realistic network size, `microbenchmark`, n=100) -- the actual `similarity()`/`tmfg()`/`cn()`/`nrw()`/`pf()` computation completely dominates `do.call()`'s own overhead. Not applied; keeping `do.call()` preserves `method_args` passthrough for free.
- **`permutation_SemNeT()`'s `generate_permuted_samples()`: negative-index subsetting (`pooled[-drawn, ]`) → a precomputed logical mask.** Benchmarked no measurable difference across 1000 iterations (~180ms either way, `microbenchmark`, n=30). Not applied.
- **`summarize_bootstrap_measures()`'s `apply(measures, 1, sd, na.rm = TRUE)` → a vectorized `rowMeans`/`rowSums` formula.** ~2x faster in isolation, but this function runs once per *group* per `bootstrap_SemNeT()` call (typically 2-6 times total), not once per iteration -- the absolute saving (~70 microseconds, total, per call) is well below the threshold where the extra formula complexity is worth it. Not applied.

**Verified:** all 326 tests still pass after every applied change; `devtools::check()`
→ 0 errors, 0 notes, the same 1 pre-existing expected warning.

## Phase 5 — Plumbing & dependency audit

| Item | Status | Notes |
|---|---|---|
| `R/legacy.R` (all backward-compat wrappers) | Done | 16 wrappers, see audit table below. Shared `legacy_dot_names()` helper reproduces every pre-2.0.0 function's `as.character(substitute(list(...)))`-based name capture for `...`-collected groups, preferring an explicit name when one was given. |
| `NAMESPACE` `importFrom` audit | Done (already clean) | Already fully `importFrom`-based since Phase 0 — the original's whole-package `import(dplyr)`/`import(ggplot2)`/`import(magrittr)` were never re-added during the rebuild in the first place (dropped along with those dependencies, see below), so there was nothing left to convert. |
| Drop `qgraph` unless `compare_nets` needs it | Done (already resolved in Phase 3) | Kept — `compare_networks()`/`compare_nets()` is the one function that genuinely still needs it for plotting, per the binding decision's own carve-out. |
| Audit/drop `plyr`, `philentropy`, etc. if unused | Done (already resolved organically) | Original `Imports`: `pbapply, dplyr, plyr, magrittr, ggplot2, igraph, qgraph, scales, car, broom, effects, methods, philentropy`. New `Imports`: `methods, utils, stats, graphics, future, future.apply, progressr, igraph, qgraph, car, broom, effects`. Dropped: `pbapply` (→ `future`/`future.apply`/`progressr`), `dplyr`/`plyr`/`magrittr`/`ggplot2`/`scales` (→ base graphics `boxplot()` for `plot.bootstrap_SemNeT()`), `philentropy` (→ `stats::dist(method = "maximum")` for `pf()`, Phase 2). None of these were reintroduced at any point in the rebuild, so this audit item was already satisfied by the time Phase 5 started — confirmed by direct `Imports:` diff against the original `DESCRIPTION`, not just recollection. |
| `R CMD check --as-cran` clean pass | Done | 0 errors, 0 notes, 0 warnings — the 1 warning carried since Phase 0 (Rd cross-references to `sim.fluency`/`bootSemNeT`, which didn't exist as real link targets until this phase's `legacy.R` landed) is now gone. One real error found and fixed along the way (below). |
| `NEWS.md`/`README.md` final update | Done | `NEWS.md` rewritten with a full changelog (new functions, real bugs fixed, backward-compatibility table + narrow documented behavior changes, dependency changes, internal changes). `README.md` added (didn't exist yet in this rebuild), based on the original's, with a new "Version 2.0.0: A Rebuilt Package" section summarizing the rebuild for users. |
| Example/test runtime audit (per request) | Done | Every non-`\donttest`/`\dontrun` `@examples` block timed directly: all run in well under CRAN's 5-second-per-example guideline (max ~1.8s, most well under 1s); the genuinely heavy examples (`bootstrap_SemNeT`, `bootstrap_test_SemNeT`, `permutation_SemNeT`, `random_network_test`, `random_walk`, `plot.bootstrap_SemNeT`, all with `iter = 100, cores = 2`) were already correctly `\donttest`-wrapped in their own phases — no changes needed. Separately profiled the test suite (`devtools::test()`, ~61s total): dominated by `test-permutation_SemNeT.R` (~20.5s), `test-bootstrap_SemNeT.R`/`test-bootstrap_test_SemNeT.R` (~11s each), `test-legacy.R` (~8.3s), and `test-plot.bootstrap_SemNeT.R` (~4.2s) — all from repeated real (not toy) `method = "tmfg"` bootstrap/permutation iterations on the full bundled fixtures. This is test-suite time, not example time, and CRAN's 5-second rule specifically targets examples; total `R CMD check` duration (~4.5 minutes, most of it this test suite plus vendored-C compilation) is well within normal CRAN bounds. Left as-is rather than trimmed, to avoid weakening real-data coverage for a check duration that isn't actually a problem — flagging in case a faster local dev loop is wanted instead. |

**Design decisions made while building `R/legacy.R`, flagging for visibility:**

1. **`bootSemNeT()`'s `method`/`type` arguments never had a working default
   in the original** — verified by inspection: the original function body
   has no `match.arg()`/`missing()` handling for either, so an unresolved
   multi-value default reaching `switch(method, ...)` or `if(type == ...)`
   would error before doing anything. The legacy wrapper now lets a call
   that omits either argument fall through cleanly to
   `bootstrap_SemNeT()`'s own defaults (`"tmfg"`/`"case"`) — a strict
   improvement over the original's dead-on-arrival "default," not a
   compatibility break (there was no working old behavior to preserve).
2. **`bootSemNeT()`/`randwalk()`/`randnet.test()` no longer auto-detect
   cores by default** (the originals defaulted to
   `parallel::detectCores() - 1` or `/ 2`) — they now default to
   sequential processing (`cores = 1`), matching their new-named
   replacements and avoiding reintroducing a direct `parallel::detectCores()`
   call outside `parallel_process()`. Documented in `NEWS.md`; `cores =`
   still works exactly as before when passed explicitly.
3. **`legacy_dot_names()`** (the shared name-capture helper) prefers an
   explicit name when the caller gives one (e.g.
   `bootSemNeT(low = a, high = b)`), falling back to the deparsed
   expression only when unnamed (e.g. `bootSemNeT(low, high)`) — the
   original always used the deparsed-expression form unconditionally, so a
   named call like `bootSemNeT(x = low_data)` would have produced a group
   named `"x"`. Explicit names winning is the more useful, more predictable
   behavior and matches how every *new*-named function already works;
   flagging in case exact old-quirk fidelity is wanted instead.
4. **`test.bootSemNeT()`/`plot.bootSemNeT()` only accept objects produced by
   this package's own `bootSemNeT()`/`bootstrap_SemNeT()`**, not a raw
   object saved from SemNeT `< 2.0.0` (e.g. loaded from an old `.rds`) —
   reconstructing the old `"XNet"/"XMeas"/"XSumm"` string-splice parsing
   just to accept such objects would reintroduce the exact fragility this
   rebuild's new schema was built to eliminate. `bootSemNeT()`'s legacy
   wrapper returns an object with class `c("bootSemNeT", "bootstrap_SemNeT")`
   (both, so `plot()` dispatches to `plot.bootSemNeT` *and* the object can
   be passed straight into `bootstrap_test_SemNeT()`/its own legacy wrapper),
   so any object actually produced by this version's `bootSemNeT()` works
   correctly with both.

**Real error found and fixed by `devtools::check()`:** the first check run on
this phase's `legacy.R` failed outright (`checking for portable file names`)
because roxygen wrote separate Rd pages for the 7 lowercase-acronym pairs
(`TMFG`/`tmfg`, `ASPL`/`aspl`, `CC`/`cc`, `CN`/`cn`, `NRW`/`nrw`, `PF`/`pf`,
`Q`/`q`) — `man/TMFG.Rd` and `man/tmfg.Rd` differ only by case, which isn't a
usable pair of filenames on case-insensitive filesystems and is a hard CRAN
rejection. Fixed by giving each of those 7 legacy wrappers `@rdname
<new_name>` instead of its own full doc block, merging both onto one Rd page
(one `\usage{}` entry per function, shared `\arguments{}`/`\value{}`/etc.) —
the same pattern already used correctly by every other legacy wrapper whose
name doesn't collide by case.

**Example/test runtime, per direct request:** audited every `@examples`
block in the package (all 25 documented functions/datasets) by timing every
example that isn't already `\donttest`/`\dontrun`-wrapped directly via
`system.time()` against the real bundled fixtures. All run in well under
CRAN's 5-second-per-example guideline — worst case ~1.8s
(`random_walk()`'s hidden `\dontshow{}` smoke-test call), most under 0.5s.
The examples that legitimately take a while (`iter = 100, cores = 2` calls
in `bootstrap_SemNeT()`, `bootstrap_test_SemNeT()`, `permutation_SemNeT()`,
`random_network_test()`, `random_walk()`, `plot.bootstrap_SemNeT()`) were
already correctly `\donttest`-wrapped in their own phases, so nothing
changed there. Separately profiled the test suite itself (not covered by
CRAN's example-timing rule, but relevant to "make checks shorter"): see the
audit row in the table above.

**Verified:** all 362 tests pass (36 new, in `tests/testthat/test-legacy.R`,
covering every wrapper — direct equivalence checks against the new function
for the argument-preserving ones, and behavior/output-shape checks for the
ones with real translation logic); `devtools::check()` → 0 errors, 0 notes,
0 warnings (the long-carried Rd cross-reference warning is now resolved).

## Legacy-wrapper audit

| Old exported name | New function it calls | Status |
|---|---|---|
| `TMFG` | `tmfg` | Done |
| `ASPL` | `aspl` | Done |
| `CC` | `cc` | Done |
| `CN` | `cn` | Done |
| `NRW` | `nrw` | Done |
| `PF` | `pf` | Done |
| `Q` | `q` | Done |
| `bootSemNeT` | `bootstrap_SemNeT` | Done |
| `test.bootSemNeT` | `bootstrap_test_SemNeT` | Done |
| `plot.bootSemNeT` | `plot.bootstrap_SemNeT` | Done |
| `randwalk` | `random_walk` | Done |
| `randnet.test` | `random_network_test` | Done |
| `sim.fluency` | `simulate_fluency` | Done |
| `response.analysis` | `response_analysis` | Done |
| `semnetmeas` | `semantic_network_measures` | Done |
| `compare_nets` | `compare_networks` | Done |

*(`similarity`, `finalize`, `equate`, `convert2igraph`, `convert2cytoscape` need no
wrapper — old name already matches the new one. `permSemNeT` was never exported
before, so `permutation_SemNeT` is a first-time export with no old name to preserve.)*

## Rebuild complete

All five phases are now Done. `SemNeT` 2.0.0's internals are fully rebuilt:
single-responsibility functions with up-front validation, bugs eliminated
(and documented in `NEWS.md`), hot loops benchmarked and optimized where it
mattered, structural conventions matched to `EGAnet`, and the entire
pre-2.0.0 public API kept working through `R/legacy.R`. The Shiny app
remains the one explicitly deferred item (see "Deferred / Dropped" above).

**Follow-up work** (a core-package memory-lean pass, then the Shiny app rebuild
itself) is tracked separately in `SHINY_PLAN.md`, not in this document.

## Post-rebuild addition: `forward_flow()`

Not part of the original rebuild scope -- it had already been removed from
`../SemNeT` (in `1db4252`, "quick fix for CRAN issue #12") before this rebuild's
baseline was taken, so it was never carried over. Investigating GitHub issue #10
(`forward_flow.R`) traced the removal to a genuine CRAN violation: `ff_function()`
called `googledrive::drive_auth()`/`drive_download()` to silently fetch pretrained
semantic-space vectors from Google Drive at runtime.

Re-added on request (2026-09-07) with that mechanism deliberately not restored --
`forward_flow()` now takes a user-supplied `embeddings` matrix (rows named by
vocabulary term) and never touches the network. Everything else was rewritten to
match 2.0.0 conventions: validated arguments (`forward_flow_errors()`),
`parallel_process()` in place of a hand-managed `parallel::makeCluster()` +
`pbapply`, and the existing `cosine()` helper in place of a new `LSAfun`
dependency (dropped rather than re-added). The documented-but-dead `min_cue`
parameter from the original was not carried forward -- it was never read
anywhere in `ff_function()`, matching this rebuild's existing precedent of
dropping dead parameters (e.g. legacy `bootSemNeT()`'s `weighted`) rather than
preserving them for appearance's sake. See `R/forward_flow.R` and
`tests/testthat/test-forward_flow.R`; `NEWS.md`'s "New functions" section has the
user-facing summary.

**Bug found and fixed in the same-day review (2026-09-07), before any release:**
`min_response`/`max_response` were silently never applied on the `type = "fluency"`
path -- `forward_flow()`'s dispatcher only forwarded `response_matrix`/`embeddings`/
`cores` to `forward_flow_fluency()`, which didn't even take those two parameters.
A participant below `min_response` (but with `> 1` usable response, so not caught
by `response_flow()`'s own `n <= 1` short-circuit) got a real computed value
instead of `NA`, directly contradicting the documented contract. Caught by testing
the actual gate (2 responses vs. `min_response = 3`) rather than the `n = 1` edge
case my first test suite happened to use, which passed regardless of whether the
gate worked at all. Fixed by threading both parameters through and applying them
per participant before `response_flow()` is called, same as the `"free"` path
already did. Also normalized `NaN` (all-unmatched-vocabulary participants/cues,
from `mean()` of an empty `na.rm`'d vector) to `NA_real_` for consistency --
functionally harmless (`is.na(NaN)` is `TRUE` in R) but confusing to see in
printed output.

**Optimization pass (2026-09-07), same day, before any release:** `forward_flow_free()`'s
participant-cue grouping re-scanned the *entire* response table once per
participant (`response_matrix$ID == id`, then `participant$Cue == cue`), and its
final result-assembly loop did the same re-scan of `cue_ids` once per participant --
both effectively quadratic in row count for datasets with many participants.
Replaced with one `split()` pass each (row indices grouped by participant, then by
cue within each participant; result indices grouped by participant), with output
order preserved via explicit factor `levels = ids`. Verified `identical()` output
against the old grouping logic before replacing it, plus a dedicated regression
test (`test-forward_flow.R`) checking the split()-based grouping against a naive
re-scan reference. Benchmarked on synthetic free-association data (10 cues, 15
responses/cue): the grouping step alone went from ~7.1s to ~0.13s at 2,400
participants (360,000 rows) -- a ~54x speedup, and roughly linear instead of
quadratic scaling as participant count grows. End-to-end `forward_flow()` time
(grouping + actual similarity computation) dropped from ~4.4s to ~1.9s at 1,200
participants. Also removed a redundant `as.matrix()`/`as.vector()` round trip in
`forward_flow_fluency()`'s per-participant response extraction (`response_matrix[i, ]`
is already a plain vector once `response_matrix_error()` has coerced the input to a
matrix).

**Verification against the real GitHub issue #10/#11 data (2026-09-07):** downloaded
the actual data files attached to both issues and ran them through the rebuilt
functions directly, rather than only synthetic data.

- Issue #10 (`forward_flow()` "node stack overflow"): their `response_matrix.csv`
  (6 participants, 60 cues, 1,441-word vocabulary) ran successfully in 0.14s. The
  original crash's exact mechanism (multi-space `"all"` looping + `googledrive`
  vector loading) no longer exists in this rebuild, so it can't recur structurally
  either way. This run surfaced a **real, separate bug** in this rebuild's own code:
  `forward_flow_free()`'s `result_index_by_id[[id]]` / `response_flow_list[[id]]`
  indexed with the raw (numeric) participant ID -- `[[` on a list indexes
  *positionally* for a bare number, not by name. Their real IDs (7, 12, 20, ...)
  exceeded the 6-element list, throwing "subscript out of bounds"; smaller numeric
  IDs coinciding with their own list position (as in this file's own earlier test
  data, IDs 1/2) would have silently returned the *wrong* participant's results
  instead of erroring at all. Fixed by indexing with `as.character(id)` at both
  sites; added a regression test using deliberately non-sequential IDs (7/12/20)
  that reproduces the original failure.
- Issue #11 (Shiny Spreading Activation): ran their `HCMS_Animals_cleaned_responses.csv`
  + `group.csv` (88 HC / 89 MS participants) through the exact pipeline
  `mod_network_estimation.R` uses (auto-detect raw-vs-binary -> `responses_to_binary()`
  -> `finalize()` -> `equate()` -> `similarity()` -> `tmfg()`), producing two
  112-node networks with real animal-name `colnames()` preserved throughout. Ran
  their `HCMS_Animals_BRM.csv` (already-binary, 194 rows) through the same
  auto-detection branch directly -- `is.character(unlist(brm))` correctly
  evaluates `FALSE`, skipping `responses_to_binary()` -- producing a clean 194-node
  network. Then ran `spreadr::spreadr()` on the estimated network using real node
  names pulled from `colnames(network)` (exactly as `mod_spreading_activation.R`
  does): zero "these nodes don't exist in network" warnings, and non-zero
  activation output (max = 10) -- directly contradicting both of the original
  issue's reported symptoms.

**Broader bug-hunt pass (2026-09-07), prompted by the issue #10 finding above:**
searched the whole package for the same bug class (a loop variable drawn from
*user data values*, rather than `names()`/`...`-capture, later used as a bare `[[`
index) -- `forward_flow.R` was the only place it occurred; every other `[[i]]`/
`[[group_name]]` site in the package indexes with either an integer loop counter
(`seq_len()`/`seq_along()`) or a name genuinely guaranteed to be character
(`names()` of an R list, or a `...`-capture). Also cross-checked three complex,
previously under-tested numeric algorithms against independent references:
`distance(A, weighted = TRUE)` (a from-scratch Dijkstra implementation, only
reachable indirectly via `aspl(..., weighted = TRUE)` and never previously given a
correctness test, only a type-validation one) against `igraph::distances()` on a
random weighted network; `cc(A, weighted = TRUE)`'s signed geometric-mean formula
against a hand-derived value on a small triangle network, including a negative
edge weight; and `q()`'s vendored Louvain modularity against
`igraph::modularity()` computing the same partition independently. All three
matched their reference to floating-point precision -- no bug found in any of
them, but all three gained permanent regression tests closing a real coverage
gap (`test-aspl.R`, `test-cc.R`).
