# sdmTMBextra

<!-- badges: start -->
[![R-CMD-check](https://github.com/pbs-assess/sdmTMBextra/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pbs-assess/sdmTMBextra/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

sdmTMBextra is an R package with extra utilities for working with sdmTMB models. Functionality includes:

- Sampling with MCMC using rstan/tmbstan
- DHARMa residuals

The package was developed to reduce heavier package dependencies within the main sdmTMB package.

## Installation

You can install the development version of sdmTMBextra with:

``` r
# install.packages("remotes")
remotes::install_github("pbs-assess/sdmTMBextra")
```

See the examples in the sdmTMB vignettes.
