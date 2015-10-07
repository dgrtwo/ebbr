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
estimate_beta_binom <- function(successes, totals) {
  # negative log likelihood of data given alpha; beta
  ll <- function(alpha, beta) {
    -sum(VGAM::dbetabinom.ab(successes, totals, alpha, beta, log = TRUE))
  }

  # use the method of moments for starting parameters
  mm_estimate <- estimate_beta_mm(successes, totals)
  m <- stats4::mle(ll, start = list(alpha = mm_estimate$alpha,
                            beta = mm_estimate$beta),
           method = "L-BFGS-B")
  co <- unname(m@coef)
  data.frame(alpha = co[1], beta = co[2])
}
