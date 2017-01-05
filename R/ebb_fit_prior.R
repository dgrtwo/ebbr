#' Estimate the shape parameters of a beta by fitting
#' a beta-binomial distribution
#'
#' Estimate the shape parameters of a beta that underlies
#' many pairs of success and total. This is useful for determining the hyperparameters
#' of a prior, for empirical Bayes estimation. This offers three methods for estimating them:
#' the method of moments, based only on the mean and variance of the proportions,
#' maximum likelihood estimation, based on a beta-binomial model, and
#' beta-binomial regression, which allows the mean and variance of the beta to
#' vary based on other parameters in the table.
#'
#' @param tbl A table.
#' @param x An expression for the number of successes, evaluated within the table.
#' @param n An expression for the total number of trials, evaluated within the table.
#' @param method The method for fitting the beta-binomial model. Either "mle"
#' (maximum likelihood), "mm" (method of moments), or "gamlss" (for beta-binomial
#' regression). Only "gamlss" allows the mu and sigma parameters of the beta to
#' depend on other parameters.
#' @param mu_predictors A formula that the mean of the beta may depend on. Ignored
#' if method is not "gamlss".
#' @param sigma_predictors A formula that the variance of the beta may depend on.
#' Ignored if method is not "gamlss".
#' @param mu_link Link function for mu. Ignored if method is not "gamlss".
#' @param sigma_link Link function for sigma. Ignored if method is not "gamlss".
#' @param start Starting guesses of parameters for "mle" method. If NULL, these
#' are estimated by the method of moments.
#' @param ... Extra arguments passed on to \code{\link[stats4]{mle}}
#' (if method is "mle") or to \code{\link[gamlss]{gamlss}} (if method is
#' "gamlss"). Ignored if method is "mm".
#'
#' @return An object of class "ebb_prior", which contains the estimated parameters of a
#' beta-binomial distribution:
#'   \item{parameters}{Either a one-row tbl_df with alpha and beta parameters,
#'   or (if method is "gamlss"), the coefficients for computing \code{mu} and
#'   \code{sigma}}.
#'   \item{fit}{The result of the estimation process. An "mle" object if method
#'   is "mle", a "gamlss" object if method is "gamlss", and NULL if the method is
#'   "mm".}
#'   \item{terms}{A list of the x, n, mu_predictors, and sigma_predictors terms.}
#'   \item{method}{The method used to fit the model.}
#'   \item{model}{The original data, with just the columns needed to fit the model.}
#'
#' @importFrom dplyr %>%
#'
#' @examples
#'
#' # example with baseball data
#' library(Lahman)
#' library(dplyr)
#'
#' career <- Batting %>%
#'   anti_join(Pitching, by = "playerID") %>%
#'   tbl_df() %>%
#'   filter(yearID > 1980) %>%
#'   group_by(playerID) %>%
#'   summarize(H = sum(H), AB = sum(AB)) %>%
#'   filter(AB > 0)
#'
#' # maximum likelihood estimation
#' fit <- career %>% ebb_fit_prior(H, AB)
#'
#' fit
#' tidy(fit)
#' augment(fit)
#' glance(fit)
#'
#' # method of moments
#' career %>% ebb_fit_prior(H, AB, method = "mm")
#'
#' # mm works better with pre-filtering:
#' career %>%
#'   filter(AB > 100) %>%
#'   ebb_fit_prior(H, AB, method = "mm")
#'
#' # allow mu to vary with AB using beta-binomial regression
#' # see: http://varianceexplained.org/r/bayesian_ab_baseball/
#' eb <- career %>%
#'   filter(AB > 100) %>%
#'   ebb_fit_prior(H, AB, mu_predictors = ~ log10(AB))
#'
#' tidy(eb)
#' augment(eb)
#'
#' library(ggplot2)
#' ggplot(augment(eb), aes(AB, .fitted)) +
#'   geom_point() +
#'   scale_x_log10()
#'
#' @export
ebb_fit_prior <- function(tbl, x, n, method = c("mle", "mm", "gamlss"),
                        mu_predictors = ~1,
                        sigma_predictors = ~1,
                        mu_link = "logit",
                        sigma_link = "log",
                        start = NULL, ...) {
  ebb_fit_prior_(tbl, substitute(x), substitute(n),
               method = method,
               mu_predictors = mu_predictors,
               sigma_predictors = sigma_predictors,
               mu_link = "logit",
               sigma_link = "log",
               start = start,
               ...)
}


ebb_fit_prior_ <- function(tbl, x, n,
                         method = c("mle", "mm", "gamlss"),
                         mu_predictors = ~1,
                         sigma_predictors = ~1,
                         mu_link = "logit",
                         sigma_link = "log",
                         start = NULL,
                         ...) {
  x_value <- eval(x, tbl)
  n_value <- eval(n, tbl)

  no_predictors <- (mu_predictors == ~1) && (sigma_predictors == ~1)
  if (!no_predictors) {
    method <- "gamlss"
  }

  method <- match.arg(method)

  if (method == "mm") {
    p <- x_value / n_value
    mu <- mean(p)
    vr <- stats::var(p)
    alpha <- ((1 - mu) / vr - 1 / mu) * mu ^ 2
    beta <- alpha * (1 / mu - 1)

    parameters <- dplyr::data_frame(alpha = alpha, beta = beta)
    fit <- NULL
  } else if (method == "mle") {
    mm_estimate <- ebb_fit_prior_(tbl, x, n, method = "mm")

    if (is.null(start)) {
      start <- list(alpha = mm_estimate$parameters$alpha,
                    beta = mm_estimate$parameters$beta)
    }

    ll <- function(alpha, beta) {
      -sum(VGAM::dbetabinom.ab(x_value, n_value, alpha, beta, log = TRUE))
    }

    fit <- stats4::mle(ll, start, method = "L-BFGS-B", lower = c(1e-9, 1e-9), ...)
    ab <- stats4::coef(fit)
    parameters <- dplyr::data_frame(alpha = ab[1], beta = ab[2])
  } else {
    # create a formula for beta-binomial model
    lhs <- substitute(cbind(x, n - x), list(x = x, n = n))
    form <- stats::as.formula(paste(deparse(lhs), deparse(mu_predictors)))

    fam <- eval(substitute(
      gamlss.dist::BB(mu.link = x, sigma.link = y),
      list(x = mu_link, y = sigma_link)
    ))
    utils::capture.output(
      fit <- gamlss::gamlss(form, sigma.predictors = sigma_predictors,
                            data = tbl, family = fam, ...)
    )

    parameters <- broom::tidy(fit)
  }

  # other housekeeping
  vars_used <- unique(c(all.vars(x), all.vars(n),
                        all.vars(mu_predictors), all.vars(sigma_predictors)))
  model <- tbl[, vars_used, drop = FALSE]

  terms <- list(x = x, n = n, mu_predictors = mu_predictors, sigma_predictors = sigma_predictors)

  ret <- list(parameters = parameters, fit = fit, terms = terms, method = method, model = model)
  class(ret) <- "ebb_prior"

  ret
}
