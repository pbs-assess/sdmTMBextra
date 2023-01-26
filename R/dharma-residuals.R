#' DHARMa residuals
#'
#' Plot (and possibly return) DHARMa residuals. This is a wrapper function
#' around [DHARMa::createDHARMa()] to facilitate its use with [sdmTMB()] models.
#' **Note**: simulation testing suggests that these DHARMa residuals can suggest
#' problems with model fit even with properly specified models presumably due to
#' the Laplace approximation and/or spatially correlated random effects.
#' Consider the slower [residuals.sdmTMB()] with `type = "mle-mcmc"`.
#'
#' @param simulated_response Output from [simulate.sdmTMB()].
#' @param object Output from [sdmTMB()].
#' @param plot Logical.
#' @param ... Other arguments to pass to [DHARMa::createDHARMa()].
# @param fitted_column The column from the output of [predict.sdmTMB()] to pass
#   to [DHARMa::createDHARMa()]'s `fittedPredictedResponse` argument.
#'
#' @return
#' A data frame of observed and expected values is invisibly returned,
#' so you can set `plot = FALSE` and assign the output to an object if you wish
#' to plot the residuals yourself. See the examples.
#' @export
#'
#' @seealso [simulate.sdmTMB()], [residuals.sdmTMB()]
#'
#' @examples
#' if (inla_installed()) {
#' fit <- sdmTMB(density ~ as.factor(year) + s(depth, k = 3),
#'   data = pcod_2011, time = "year", mesh = pcod_mesh_2011,
#'   family = tweedie(link = "log"), spatial = "off",
#'   spatiotemporal = "off")
#'
#' # The `simulated_response` argument is first so the output from
#' # simulate() can be piped to dharma_residuals():
#' # simulate(fit, nsim = 500) %>% dharma_residuals(fit)
#'
#' s <- simulate(fit, nsim = 500)
#' dharma_residuals(s, fit)
#' r <- dharma_residuals(s, fit, plot = FALSE)
#' head(r)
#' plot(r$expected, r$observed)
#' abline(a = 0, b = 1)
#' }

dharma_residuals <- function(simulated_response, object, plot = TRUE, ...) {
  if (!requireNamespace("DHARMa", quietly = TRUE)) {
    cli_abort("DHARMa must be installed to use this function.")
  }

  assert_that(inherits(object, "sdmTMB"))
  assert_that(is.logical(plot))
  assert_that(is.matrix(simulated_response))
  assert_that(nrow(simulated_response) == nrow(object$response))
  if (isTRUE(object$family$delta)) {
    y <- ifelse(!is.na(object$response[,2]),
      object$response[,2], object$response[,1])
  } else {
    y <- object$response[,1]
  }
  y <- as.numeric(y)

  # FIXME parallel setup here?

  fitted <- fitted(object)

  # fitted <- object$family$linkinv(p[["est_non_rf"]])
  res <- DHARMa::createDHARMa(
    simulatedResponse = simulated_response,
    observedResponse = y,
    fittedPredictedResponse = fitted,
    ...
  )
  u <- res$scaledResiduals
  n <- length(u)
  m <- seq_len(n) / (n + 1)
  z <- stats::qqplot(m, u, plot.it = FALSE)
  if (plot) {
    DHARMa::plotQQunif(
      res,
      testUniformity = FALSE,
      testOutliers = FALSE, testDispersion = FALSE
    )
  }
  invisible(data.frame(observed = z$y, expected = z$x))
}

