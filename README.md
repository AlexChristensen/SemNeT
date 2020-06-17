[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)[![Downloads Total](https://cranlogs.r-pkg.org/badges/grand-total/SemNeT?color=brightgreen)](https://cran.r-project.org/package=SemNeT) [![Downloads per month](http://cranlogs.r-pkg.org/badges/SemNeT?color=brightgreen)](https://cran.r-project.org/package=SemNeT) 

# How To Install
devtools::install_github("AlexChristensen/SemNeT")

# How To Use
[Tutorial](https://psyarxiv.com/eht87/)

Christensen, A. P., & Kenett, Y. N. (2019). Semantic network analysis (SemNA): A tutorial on preprocessing, estimating, and analyzing semantic networks. *PsyArXiv*. https://psyarxiv.com/eht87/

# SemNeT
SemNeT offers researchers several tools for the analysis of their semantic network data. As a part of a module of semantic network packages, SemNeT is the most general, providing statistical analyses for all types of semantic networks.

## SemNeT Shiny
From raw data to semantic network analysis in three lines of code: The Shiny app allows for integration with [*SemNetCleaner*](https://github.com/AlexChristensen/SemNetCleaner), streamlining the SemNA pipeline:
```
# Grouping variable
group <- SemNeT::open.group

# Preprocessed data
clean <- SemNetCleaner::textcleaner(data = SemNetCleaner::open.animals[,-c(1,2)],
                                    miss = 99, partBY = "row", dictionary = "animals")

# SemNeT Shiny app for network estimation and analyses
SemNeT::SemNeTShiny()
```

The point and click interface of the SemNeT Shiny app enables users to perform all analyses in the package as well as spreading activation analyses from the [*spreadr*](https://github.com/csqsiew/spreadr) package (Siew, 2019).

# References
Christensen, A. P., Kenett, Y. N., Cotter, K. N., Beaty, R. E., & Silvia, P. J. (2018).
Remotely close associations: Openness to experience and semantic memory structure.
*European Journal of Personality*, *32*, 480-492. https://doi.org/10.1002/per.2157

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
