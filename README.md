# SemNeT

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

SemNeT is an R package for estimating and analyzing semantic networks. It is the third part of a pipeline for verbal fluency data: [*SemNetDictionaries*](https://github.com/AlexChristensen/SemNetDictionaries) and [*SemNetCleaner*](https://github.com/AlexChristensen/SemNetCleaner) preprocess the responses, and SemNeT estimates networks from the cleaned responses and compares them across groups. The tutorial for the full pipeline is

Christensen, A. P., & Kenett, Y. N. (2023). Semantic network analysis (SemNA): A tutorial on preprocessing, estimating, and analyzing semantic networks. *Psychological Methods*, *28*(4), 860-879. https://doi.org/10.1037/met0000463 (see [PsyArXiv](https://psyarxiv.com/eht87/) for the preprint)

> Notice: Tu et al. (under review) found that the group tests in `bootstrap_test_SemNeT()` and `test.bootSemNeT()` before version 2.1.0 gave inflated test statistics. A t-test, ANOVA, or ANCOVA was applied to the bootstrap samples as if they were independent observations, and the bootstrap standard error was divided by the square root of the number of iterations. The statistics were therefore larger by about that factor, and a difference became "significant" as the number of iterations increased. Results from these functions in earlier versions (including the original bootstrap analyses in the Shiny application) should be recomputed with version 2.1.0 or later. See "Changes in version 2.1.0" below.

## Installation

```r
if(!"devtools" %in% row.names(installed.packages())){
  install.packages("devtools")
}

devtools::install_github("AlexChristensen/SemNeT", dependencies = c("Imports", "Suggests"))
```

## What the package does

Four methods estimate a network from a verbal fluency response matrix:

| Function | Method | Reference |
|----------|--------|-----------|
| `tmfg()` | Triangulated Maximally Filtered Graph | Massara et al. (2016) |
| `cn()` | Community network | Goñi et al. (2011) |
| `nrw()` | Naïve random walk | Lerner et al. (2009) |
| `pf()` | Pathfinder network | Paulsen et al. (1996) |

`tmfg()` works on a similarity matrix (see `similarity()`), so responses are first converted to a binary matrix and, when groups are compared, equated to the responses that every group gave (`finalize()` and `equate()`). The other three methods use the order of the responses and take the raw response matrix.

`semantic_network_measures()` computes the average shortest path length (`aspl()`), the clustering coefficient (`cc()`), and the modularity (`q()`) of a network. These measures can be tested in several ways:

* `random_network_test()` compares a network to random networks with the same number of nodes and edges and to random networks with the same degrees.
* `bootstrap_SemNeT()` bootstraps the networks of one or more groups by resampling cases or nodes, and `bootstrap_test_SemNeT()` estimates confidence intervals of the group differences.
* `permutation_SemNeT()` tests group differences by shuffling group membership.
* `random_walk()` simulates random walks on two networks and compares them.

`bootstrap_test_SemNeT()` and `permutation_SemNeT()` should be run together, and both results should be reported.

Other functions include `response_analysis()` (compares the responses of groups), `forward_flow()` (forward flow; Gray et al., 2019, with semantic spaces from Beaty et al., 2021), `compare_networks()` (plots networks side by side), and `convert2igraph()` and `convert2cytoscape()`.

## Example

```r
library(SemNeT)

# Raw responses of the two groups in the example data
low <- open.clean[which(open.group == "Low"), ]
high <- open.clean[which(open.group == "High"), ]

# Estimate a network for each group
low_network <- nrw(low)
high_network <- nrw(high)

# Compute the network measures
semantic_network_measures(low_network, seed = 1234)
semantic_network_measures(high_network, seed = 1234)

# Bootstrap both groups and estimate the differences between them
boot <- bootstrap_SemNeT(low = low, high = high, method = "tmfg", iter = 1000, seed = 1234)
bootstrap_test_SemNeT(boot)

# Permutation test of the same difference
permutation_SemNeT(low, high, method = "tmfg", measure = c("aspl", "cc", "q"), iter = 1000, seed = 1234)
```

## Shiny application

The Shiny application runs the analyses above from a point-and-click interface and adds spreading activation from the [*spreadr*](https://github.com/csqsiew/spreadr) package (Siew, 2019). It can read a *SemNetCleaner* object from the R environment:

```r
# Grouping variable
group <- SemNeT::open.group

# Preprocessed data
clean <- SemNetCleaner::textcleaner(
    data = SemNetCleaner::open.animals[,-c(1,2)], type = "fluency",
    miss = 99, partBY = "row", dictionary = "animals"
)

# Open the application
SemNeT::SemNeTShiny()
```

## Changes in version 2.1.0

`bootstrap_test_SemNeT()` now returns bootstrap confidence intervals of the within-iteration group differences, with a Wald test when the normal interval is used (the default). The old tests are only available with `legacy = TRUE`, which always gives a warning. For a formal test of group differences, use `permutation_SemNeT()`.

Version 2.1.0 also changes some estimation methods. `pf()` now uses the response-order distance of Paulsen et al. (1996) and takes raw or ordered responses, and the co-occurrence test in `cn()` uses a two-sided confidence interval. `seed = 0` now means a random seed. See [NEWS.md](NEWS.md) for the full list of changes.

## References

Beaty, R. E., Zeitlen, D. C., Baker, B. S., & Kenett, Y. N. (2021).
Forward flow and creative thought: Assessing associative cognition and its role in divergent thinking.
*Thinking Skills and Creativity*, 100859. https://doi.org/10.1016/j.tsc.2021.100859

Christensen, A. P., Kenett, Y. N., Cotter, K. N., Beaty, R. E., & Silvia, P. J. (2018).
Remotely close associations: Openness to experience and semantic memory structure.
*European Journal of Personality*, *32*(4), 480-492. https://doi.org/10.1002/per.2157

Goñi, J., Arrondo, G., Sepulcre, J., Martincorena, I., de Mendizábal, N. V., Corominas-Murtra, B., ... & Villoslada, P. (2011).
The semantic organization of the animal category: Evidence from semantic verbal fluency and network theory.
*Cognitive Processing*, *12*, 183-196.

Gray, K., Anderson, S., Chen, E. E., Kelly, J. M., Christian, M. S., Patrick, J., ... & Lewis, K. (2019).
"Forward flow": A new measure to quantify free thought and predict creativity.
*American Psychologist*, *74*(5), 539-554. https://doi.org/10.1037/amp0000391

Kenett, Y. N., & Austerweil, J. L. (2016). Examining search processes in low and high creative individuals with random walks.
In *Proceedings of the 38th annual meeting of the Cognitive Science Society* (pp. 313-318). Austin, TX. https://cogsci.mindmodeling.org/2016/papers/0066/index.html

Kenett, Y. N., Anaki, D., & Faust, M. (2014). Investigating the structure of semantic networks in low and high creative persons.
*Frontiers in Human Neuroscience*, *8*, 407. https://doi.org/10.3389/fnhum.2014.00407

Lerner, A. J., Ogrocki, P. K., & Thomas, P. J. (2009).
Network graph analysis of category fluency testing.
*Cognitive and Behavioral Neurology*, *22*, 45-52.

Massara, G. P., Di Matteo, T., & Aste, T. (2016).
Network filtering for big data: Triangulated maximally filtered graph.
*Journal of Complex Networks*, *5*, 161-178. https://doi.org/10.1093/comnet/cnw015

Paulsen, J. S., Romero, R., Chan, A., Davis, A. V., Heaton, R. K., & Jeste, D. V. (1996).
Impairment of the semantic network in schizophrenia.
*Psychiatry Research*, *63*, 109-121.

Siew, C. S. Q. (2019).
spreadr: An R package to simulate spreading activation in a network.
*Behavior Research Methods*, *51*, 910-929. https://doi.org/10.3758/s13428-018-1186-5

Tu, C.-Y., Chodorow, M., Brooks, P. J., Ezrina, E., & Valian, V. (under review).
Inflated statistics lead to incorrect inference: How (not) to make cognitive network group comparisons.
