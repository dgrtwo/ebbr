#' Estimate proportions of various success/total counts
#'
#' @param x Vector of successes
#' @param n Vector of totals
#' @param prior_filter A logical vector indicating which
#' values should be used for computing the prior
#' @param cred_level Level of credible interval to
#' compute. If NULL, do not compute intervals.
#'
#' @return An "ebbinom" object, which contains a data.frame
#' \code{table} with the columns:
#'   \item{alpha1}{Posterior alpha (shape1) parameter}
#'   \item{beta1}{Posterior beta (shape2) parameter}
#'   \item{estimate}{Posterior shrunken estimate}
#'   \item{raw.estimate}{Estimate without shrinkage (success / total)}
#'   \item{conf.low}{Lower bound of credible interval}
#'   \item{conf.high}{Upper bound of credible interval}
#'
#' It also contains a data frame \code{prior} with the estimated
#' parameters for the beta distribution.
#'
#' @seealso ebbinom_tidiers
#'
#' @examples
#'
#' library(dplyr)
#'
#' set.seed(2015)
#'
#' true_prob <- rbeta(1000, 10, 50)
#' total <- round(rlnorm(1000, 5, 2)) + 1
#' success <- rbinom(1000, total, true_prob)
#'
#' d <- data_frame(success, total)
#'
#' result <- estimate_eb_proportion(d, success, total)
#'
#' prior <- glance(result)
#'
#' library(ggplot2)
#'
#' ggplot(result$table, aes(raw.estimate, estimate, color = log10(total))) +
#'   geom_point() +
#'   geom_abline(color = "red") +
#'   geom_hline(yintercept = prior$prior.estimate,
#'              color = "red", lty = 2)
#'
#' @export
estimate_eb_proportion <- function(tbl, x, n,
                                   mu_predictors = ~1,
                                   sigma_predictors = ~1,
                                   cred_level = .05, ...) {
  estimate_eb_proportion_(tbl, lazyeval::lazy(x),
                          lazyeval::lazy(n),
                          mu_predictors = mu_predictors,
                          sigma_predictors = sigma_predictors,
                          cred_level = cred_level, ...)
}


#' @rdname estimate_eb_proportion
#' @export
estimate_eb_proportion_ <- function(tbl, x, n, prior_filter = TRUE,
                                    mu_predictors = ~1,
                                    sigma_predictors = ~1,
                                    cred_level = .05, ...) {
  # estimate a prior
  prior <- estimate_beta_binom_(tbl[prior_filter, ], x, n,
                                mu_predictors, sigma_predictors, ...)
  x_value <- lazyeval::lazy_eval(x, tbl)
  n_value <- lazyeval::lazy_eval(n, tbl)

  ret <- dplyr::data_frame(
    alpha1 = x_value + prior$parameters$alpha,
    beta1 = n_value - x_value + prior$parameters$beta,
    estimate = alpha1 / (alpha1 + beta1),
    raw_estimate = x_value / n_value,
    conf_low = qbeta(cred_level / 2, alpha1, beta1),
    conf_high = qbeta(1 - cred_level / 2, alpha1, beta1))

  bind_cols(tbl, ret)
}
