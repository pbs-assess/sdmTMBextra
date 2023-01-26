# sdmTMBstan

<!-- badges: start -->
<!-- badges: end -->

sdmTMBstan is a small package that facilitates sampling from sdmTMB models with
MCMC using rstan/tmbstan.

## Installation

You can install the development version of sdmTMBstan like so:

``` r
# install.packages("remotes")
remotes::install_github("pbs-assess/sdmTMBstan")
```

## Example

This is a basic example which shows you how to solve a common problem:

TODO: finish:

``` r
library(sdmTMB)
library(sdmTMBstan)

mesh <- make_mesh(pcod, c("X", "Y"), cutoff = 10)
fit_mle <- sdmTMB(
  present ~ depth,
  data = pcod,
  mesh = mesh,
  family = binomial(),
  priors = sdmTMBpriors(matern_s = pc_matern(range_gt = 10, sigma_lt = 2)),
  bayesian = TRUE # turns on Jacobian adjustments
)
fit_mle

fit_stan <- tmbstan::tmbstan(
  fit_mle$tmb_obj,
  iter = 200, chains = 1,
  seed = 8217
)

fit_stan

p <- predict(fit_mle, newdata = qcs_grid, mcmc_samples = extract_mcmc(fit_stan))

dim(p)
```

