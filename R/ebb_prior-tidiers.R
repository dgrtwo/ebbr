#' Tidy the results of beta-binomial estimation
#'
#' Tidiers for ebb_prior objects, which store hyperparameters for a prior fit on
#' some binomial data.
#'
#' @param x An object of class "ebb_prior", representing a beta-binomial distribution.
#' @param data For augment, the original data that should be appended to.
#' @param cred_level For augment, the level of the credible intervals for each
#' observation. If NULL, do not add credible intervals.
#' @param newdata For augment, a new data set.
#' @param ... Extra arguments (not used)
#'
#' @name ebb_prior_tidiers
#'
#' @return \code{augment} returns the original data frame, with additional
#' columns corresponding to the new posterior parameters of each observation.
#'
#' \code{glance} works only on ebb_prior objects fit with "mm" or "mle": that is,
#' those that have a single alpha and beta for the entire dataset. It returns
#' those parameters:
#'   \item{alpha}{Estimated alpha (shape1) parameter}
#'   \item{beta}{Estimated beta (shape2) parameter}
#'   \item{mean}{alpha / (alpha + beta): the prior mean}
#'
#' @importFrom broom tidy augment glance
#'
#' @export
tidy.ebb_prior <- function(x, ...) {
  ret <- dplyr::tibble(x$parameters)

  if (x$method != "gamlss") {
    ret$mean <- ret$alpha / (ret$alpha + ret$beta)
  }
  ret
}

#' @rdname ebb_prior_tidiers
#' @export
augment.ebb_prior <- function(x, data, cred_level = .95, newdata = NULL, ...) {
  if (missing(data)) {
    data <- stats::model.frame(x)
  }
  original <- data

  if (!is.null(newdata)) {
    data <- newdata
  }

  x_value <- lazyeval::lazy_eval(x$terms$x, data)
  n_value <- lazyeval::lazy_eval(x$terms$n, data)

  if (x$method == "gamlss") {
    ret <- dplyr::tibble(
      .mu = stats::predict(x$fit, "mu", type = "response", data = original, newdata = newdata),
      .sigma = stats::predict(x$fit, "sigma", type = "response", data = original, newdata = newdata),
      .alpha0 = .mu / .sigma,
      .beta0 = (1 - .mu) / .sigma,
      .alpha1 = x_value + .alpha0,
      .beta1 = n_value - x_value + .beta0
    )
  } else {
    ret <- dplyr::tibble(
      .alpha1 = x_value + x$parameters$alpha,
      .beta1 = n_value - x_value + x$parameters$beta)
  }

  ret <- ret %>%
    dplyr::mutate(.fitted = .alpha1 / (.alpha1 + .beta1),
                  .raw = x_value / n_value)

  if (!is.null(cred_level)) {
    ret$.low = stats::qbeta((1 - cred_level) / 2, ret$.alpha1, ret$.beta1)
    ret$.high = stats::qbeta(1 - (1 - cred_level) / 2, ret$.alpha1, ret$.beta1)
  }

  dplyr::bind_cols(data, ret)
}


#' @rdname ebb_prior_tidiers
#' @export
glance.ebb_prior <- function(x, ...) {
  if (x$method == "gamlss") {
    ret <- dplyr::tibble(df = nrow(tidy(x)))
  } else {
    ret <- broom::tidy(x)
  }
  ret %>%
    mutate(
      logLik = stats::logLik(x),
      AIC = stats::AIC(x),
      BIC = stats::BIC(x),
    )
}


#' @export
broom::tidy

#' @export
broom::augment

#' @export
broom::glance
