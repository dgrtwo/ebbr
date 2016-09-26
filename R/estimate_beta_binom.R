#' Estimate the shape parameters of a beta by fitting
#' a beta-binomial distribution
#'
#' Estimate the shape parameters of a beta based on a
#' set of success / total pairs, by fitting a
#' beta-binomial distribution. This is generally more
#' reliable than the method of moments done by
#' \code{\link{estimate_beta_mm}}, since it takes into
#' account that it should trust values with higher
#' totals. However, it does use the method of moments to
#' choose its starting parameters in the optimization.
#'
#' @param successes Vector of successes
#' @param totals Vector of totals
#'
#' @return A one-row data.frame with two columns:
#' \code{alpha} and \code{beta}.
#'
#' @examples
#'
#' set.seed(2015)
#' probs <- rbeta(100, 10, 50)
#' total <- round(rlnorm(100, 5, 2)) + 1
#' successes <- rbinom(100, total, probs)
#'
#' bb <- estimate_beta_binom(successes, total)
#' bb
#'
#' @export
estimate_beta_binom <- function(tbl, x, n,
                                mu_predictors = ~1,
                                sigma_predictors = ~1,
                                cred_level = .05,
                                mu.link = "logit",
                                sigma.link = "log", ...) {
  estimate_beta_binom_(tbl,
                       lazyeval::lazy(x),
                       lazyeval::lazy(n),
                       mu_predictors = mu_predictors,
                       sigma_predictors = sigma_predictors,
                       cred_level = .05,
                       mu_link = "logit",
                       sigma_link = "log",
                       ...)
}


estimate_beta_binom_ <- function(tbl, x, n,
                                 mu_predictors = ~1,
                                 sigma_predictors = ~1,
                                 cred_level = .05,
                                 mu.link = "logit",
                                 sigma.link = "log",
                                 ...) {
  # create a formula for beta-binomial model
  lhs <- substitute(cbind(x, n - x), list(x = x$expr, n = n$expr))
  form <- as.formula(paste(deparse(lhs), deparse(mu_predictors)))

  fam <- eval(substitute(
    gamlss.dist::BB(mu.link = x, sigma.link = y),
    list(x = mu.link, y = sigma.link)
  ))
  capture.output(
    fit <- gamlss::gamlss(form, sigma.predictors = sigma_predictors,
                          data = tbl, family = fam, ...)
  )

  mu <- fitted(fit, "mu")
  sigma <- fitted(fit, "sigma")

  no_predictors <- (mu_predictors == ~1) && (sigma_predictors == ~1)
  if (no_predictors) {
    mu <- mu[1]
    sigma <- sigma[1]
  }

  alpha <- mu / sigma
  beta <- (1 - mu) / sigma

  ret <- list(parameters = data_frame(alpha = alpha, beta = beta),
              fit = fit)

  class(ret) <- c("beta_binomial_dist")

  ret
}
