#' Perform empirical Bayes shrinkage on a data frame
#'
#' Add columns to a data frame representing empirical Bayes shrinkage
#' towards an estimated beta prior. This calls the \code{\link{ebb_fit_prior}}
#' function to fit the prior, then adds the columns using
#' \code{\link{augment.ebb_prior}}. It is thus a useful wrapper when you're
#' not interested in the prior itself, but only in applying it to shrink data.
#'
#' @param tbl A table.
#' @param x Column containing number of successes.
#' @param n Column containing totals.
#' @param cred_level Level of credible interval to compute. If NULL,
#' do not compute intervals.
#' @param prior_subset An expression evaluating to a logical vector
#' indicating which values should be used for computing the prior.
#' @param ... Extra arguments passed on to \code{\link{ebb_fit_prior}},
#' such as \code{method}.
#'
#' @return An "ebb_prior" object, which contains a data.frame
#' \code{table} with the columns:
#'   \item{alpha1}{Posterior alpha (shape1) parameter}
#'   \item{beta1}{Posterior beta (shape2) parameter}
#'   \item{estimate}{Posterior shrunken estimate}
#'   \item{raw.estimate}{Estimate without shrinkage (success / total)}
#'   \item{conf.low}{Lower bound of credible interval}
#'   \item{conf.high}{Upper bound of credible interval}
#'
#' @seealso ebb_prior_tidiers
#'
#' @examples
#'
#' library(dplyr)
#' set.seed(2017)
#'
#' # simulate 200 random examples from a beta-binomial
#' obs <- 200
#' dat <- data_frame(prob = rbeta(obs, 10, 50),
#'                   total = round(rlnorm(obs, 4, 2)) + 1,
#'                   x = rbinom(obs, total, prob))
#'
#' result <- add_ebb_estimate(dat, x, total)
#' result
#'
#' # visualize the shrinkage towards the prior mean
#' library(ggplot2)
#' ggplot(result, aes(.raw, .fitted, color = log10(total))) +
#'   geom_point() +
#'   geom_abline(color = "red")
#'
#' @export
add_ebb_estimate <- function(tbl, x, n,
                               cred_level = .05,
                               prior_subset = TRUE,
                               ...) {
  add_ebb_estimate_(tbl, substitute(x),
                      substitute(n),
                      cred_level = cred_level,
                      prior_subset = substitute(prior_subset), ...)
}


#' @rdname add_ebb_estimate
#' @export
add_ebb_estimate_ <- function(tbl, x, n, prior_subset = TRUE, cred_level = .05, ...) {
  prior_subset_value <- lazyeval::lazy_eval(prior_subset, tbl)

  # estimate a prior
  prior <- ebb_fit_prior_(tbl[prior_subset_value, ], x, n, ...)

  # use the prior to update the table
  augment(prior, tbl, cred_level = cred_level, newdata = tbl)
}

