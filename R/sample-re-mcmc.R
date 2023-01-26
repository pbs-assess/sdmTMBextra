#' Predict from an sdmTMB model with random effects sampled via MCMC
#'
#' Calculates predictions in link space on the original fitted data with random
#' effects estimated with MCMC via rstan/tmbstan while fixing fixed effects at
#' their MLE values. This avoids the Laplace approximation, which is useful for
#' checking residuals among other uses.
#'
#' @param object An [sdmTMB::sdmTMB()] model
#' @param model Which delta/hurdle model component?
#' @param mcmc_iter Iterations for MCMC residuals. Will take the last one.
#' @param mcmc_warmup Warmup for MCMC residuals.
#' @param print_stan_model Print the Stan model from MCMC residuals?
#' @param stan_args A list of arguments that will be passed to [rstan::sampling()].
#'
#' @examples
#'
#' library(sdmTMB)
#' mesh <- make_mesh(pcod_2011, c("X", "Y"), cutoff = 10)
#' fit <- sdmTMB(
#'   present ~ as.factor(year) + poly(depth, 3),
#'   data = pcod_2011, mesh = mesh,
#'   family = binomial()
#' )
#'
#' # quick but can have issues because of Laplace approximation:
#' r1 <- residuals(fit, type = "mle-laplace")
#' qqnorm(r1)
#' qqline(r1)
#'
#' # MCMC-based with fixed effects at MLEs; best but slowest:
#' set.seed(2938)
#' r2 <- residuals(fit, type = "mle-mcmc", mcmc_iter = 101, mcmc_warmup = 100)
#' qqnorm(r2)
#' qqline(r2)
#'
#' # Example of passing control arguments to rstan::sampling():
#' # 11 iterations used for a quick example; don't do this normally
#' stan_args <- list(control = list(adapt_delta = 0.9, max_treedepth = 12))
#' r3 <- residuals(
#'   fit, type = "mle-mcmc", mcmc_iter = 11, mcmc_warmup = 10,
#'   stan_args = stan_args
#' )

predict_mle_mcmc <- function(
    object,
    model = c(1, 2),
    mcmc_iter = 500,
    mcmc_warmup = 250,
    print_stan_model = FALSE,
    stan_args = NULL) {

  # from https://github.com/mcruf/LGNB/blob/8aba1ee2df045c2eb45e124d5a753e8f1c6e865a/R/Validation_and_Residuals.R
  # get names of random effects in the model

  obj <- object$tmb_obj
  random <- unique(names(obj$env$par[obj$env$random]))

  if (isTRUE(object$reml)) {
    msg <- c(
      "Please refit your model with `reml = FALSE` to use MCMC-MLE residuals.",
      "You can use `fit_ml <- update(your_reml_fit, reml = FALSE)` to do this."
    )
    cli::cli_abort(msg)
  }

  # get (logical) non random effects indices:
  pl <- as.list(object$sd_report, "Estimate")
  fixed <- !(names(pl) %in% random)

  # fix non-random parameters to their estimated values:
  map <- lapply(pl[fixed], function(x) factor(rep(NA, length(x))))

  # construct corresponding new function object:
  obj <- TMB::MakeADFun(obj$env$data, pl, map = map, DLL = "sdmTMB")

  # run MCMC to get posterior sample of random effects given data:
  args <- list(obj = obj, chains = 1L, iter = mcmc_iter, warmup = mcmc_warmup)
  args <- c(args, stan_args)

  samp <- do.call(tmbstan::tmbstan, args)

  if (print_stan_model) print(samp)

  obj_mle <- object
  obj_mle$tmb_obj <- obj
  obj_mle$tmb_map <- map
  pred <- predict(obj_mle, tmbstan_model = samp, model = model, nsim = 1L) # only use last
  pred
}
