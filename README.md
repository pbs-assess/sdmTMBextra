# sdmTMBextra

<!-- badges: start -->
[![R-CMD-check](https://github.com/pbs-assess/sdmTMBextra/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pbs-assess/sdmTMBextra/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

sdmTMBextra is an R package with extra utilities for working with [sdmTMB](https://github.com/pbs-assess/sdmTMB) models. Functionality includes:

- Sampling with MCMC using [rstan](https://CRAN.R-project.org/package=rstan)/[tmbstan](https://CRAN.R-project.org/package=tmbstan)
- [DHARMa](https://CRAN.R-project.org/package=DHARMa) residuals

The package was developed to reduce heavier package dependencies within the main [sdmTMB](https://github.com/pbs-assess/sdmTMB) package.

## Installation

You can install sdmTMBextra with:

``` r
# install.packages("remotes")
remotes::install_github("pbs-assess/sdmTMBextra")
```

See the examples within the help files and the main [sdmTMB vignettes](https://pbs-assess.github.io/sdmTMB/) (under 'Articles').
