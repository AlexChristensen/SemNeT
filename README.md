### SemNeT 2.0.0

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

# How To Install
```
if(!"devtools" %in% row.names(installed.packages())){
  install.packages("devtools")
}

devtools::install_github("AlexChristensen/SemNeT", dependencies = c("Imports", "Suggests"))
```

# How To Use
Christensen, A. P., & Kenett, Y. N. (in press). Semantic network analysis (SemNA): A tutorial on preprocessing, estimating, and analyzing semantic networks. *Psychological Methods*. https://doi.org/10.1037/met0000463 (see [PsyArXiv](https://psyarxiv.com/eht87/) for preprint)

# SemNeT
SemNeT offers researchers several tools for the analysis of their semantic network data. As a part of a module of semantic network packages, SemNeT is the most general, providing statistical analyses for all types of semantic networks.

## Version 2.0.0: A Rebuilt Package

Version 2.0.0 is a from-scratch rebuild of the package's internals, aimed at
long-term reliability rather than new features. Highlights:

* **One function, one file.** Every exported function (e.g. `tmfg()`,
  `bootstrap_SemNeT()`) now lives in its own file alongside its own input
  validation, instead of being scattered across large, multi-purpose scripts.
* **Real bugs fixed.** Several silent correctness issues in the pre-2.0.0
  codebase are fixed in this version -- inconsistent equating logic, a few
  reversed/dropped loop ranges on degenerate inputs, a signed-weight `NaN` in
  weighted clustering coefficient, duplicate-name handling across every
  function that takes named `...` groups, and more. See `NEWS.md` for the
  full list.
* **Old code keeps working.** Every pre-2.0.0 exported name and call style
  (e.g. `bootSemNeT()`, `TMFG()`, `test.bootSemNeT()`) still works, as a thin
  wrapper around its new-named replacement (e.g. `bootstrap_SemNeT()`,
  `tmfg()`, `bootstrap_test_SemNeT()`). Existing scripts do not need to
  change.
* **Fewer dependencies.** Plotting and parallelism no longer pull in
  `ggplot2`/`dplyr`/`plyr`/`magrittr`/`pbapply`/`philentropy`; network
  plotting now goes through base graphics and `future`/`future.apply` handles
  parallel bootstrapping with guaranteed cleanup.
* **Permutation testing, exported for the first time.** `permutation_SemNeT()`
  (formerly an internal, undocumented `permSemNeT()`) is now a fully
  validated, documented, exported function.

The Shiny app (`SemNeTShiny()`) is **not yet part of this rebuild** and
remains on the pre-2.0.0 code path for now.

## Forward Flow
Implements the forward flow measure introduced by Gray et al. (2019) and semantic spaces from Beaty et al. (2021).

## SemNeT Shiny
From raw data to semantic network analysis in three lines of code: The Shiny app allows for integration with [*SemNetCleaner*](https://github.com/AlexChristensen/SemNetCleaner), streamlining the SemNA pipeline:
```
# Grouping variable
group <- SemNeT::open.group

# Preprocessed data
clean <- SemNetCleaner::textcleaner(
    data = SemNetCleaner::open.animals[,-c(1,2)], type = "fluency",
    miss = 99, partBY = "row", dictionary = "animals"
)

# SemNeT Shiny app for network estimation and analyses
SemNeT::SemNeTShiny()
```

The point and click interface of the SemNeT Shiny app enables users to perform all analyses in the package as well as spreading activation analyses from the [*spreadr*](https://github.com/csqsiew/spreadr) package (Siew, 2019).

# References
Beaty, R. E., Zeitlen, D. C., Baker, B. S., & Kenett, Y. N. (2021).
Forward flow and creative thought: Assessing associative cognition and its role in divergent thinking.
*Thinking Skills and Creativity*, 100859. https://doi.org/10.1016/j.tsc.2021.100859

Christensen, A. P., Kenett, Y. N., Cotter, K. N., Beaty, R. E., & Silvia, P. J. (2018).
Remotely close associations: Openness to experience and semantic memory structure.
*European Journal of Personality*, *32*(4), 480-492. https://doi.org/10.1002/per.2157

Gray, K., Anderson, S., Chen, E. E., Kelly, J. M., Christian, M. S., Patrick, J., ... & Lewis, K. (2019).
“Forward flow”: A new measure to quantify free thought and predict creativity.
*American Psychologist*, *74*(5), 539-554. https://doi.org/10.1037/amp0000391

Kenett, Y. N., & Austerweil, J. L. (2016). Examining search processes in low and high creative individuals with random walks.
In *Paper presented at the proceedings of the 38th annual meeting of the cognitive sceince society* (pp. 313-318). Austin, TX. Retrieved from https://cogsci.mindmodeling.org/2016/papers/0066/index.html

Kenett, Y. N., Anaki, D., & Faust, M. (2014). Investigating the structure of semantic networks in low and high creative persons.
*Frontiers in Human Neuroscience*, *8*, 407. https://doi.org/10.3389/fnhum.2014.00407

Kenett, Y. N., Wechsler-Kashi, D., Kenett, D. Y., Schwartz, R. G., Ben Jacob, E., & Faust, M. (2013).
Semantic organization in children with cochlear implants: Computational analysis of verbal fluency.
*Frontiers in Psychology*, 4. https://doi.org/10.3389/fpsyg.2013.00543

Siew, C. S. Q. (2019).
spreadr: An R package to simulate spreading activation in a network.
*Behavior Research Methods*, *51*, 910-929. https://doi.org/10.3758/s13428-018-1186-5
