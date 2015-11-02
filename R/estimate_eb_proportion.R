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
#' result <- estimate_eb_proportion(success, total, total > 100)
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
estimate_eb_proportion <- function(x, n, prior_filter = TRUE,
                                   cred_level = .05) {
  # estimate a prior
  prior <- estimate_beta_binom(x[prior_filter], n[prior_filter])

  ret <- list()
  ret$table <- data.frame(alpha1 = x + prior$alpha,
                          beta1 = n - x + prior$beta)
  ret$table <- transform(ret$table,
                         estimate = alpha1 / (alpha1 + beta1),
                         raw.estimate = x / n,
                         conf.low = qbeta(cred_level / 2, alpha1, beta1),
                         conf.high = qbeta(1 - cred_level / 2, alpha1, beta1))

  ret$prior <- prior
  class(ret) <- "ebbinom"

  ret
}
