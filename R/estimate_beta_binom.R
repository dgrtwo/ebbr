#' Estimate the shape parameters of a beta-binomial distribution
#'
#' @param successes Vector of successes
#' @param totals Vector of totals
#'
#' @export
estimate_beta_binom <- function(successes, totals) {
  # negative log likelihood of data given alpha; beta
  ll <- function(alpha, beta) {
    -sum(VGAM::dbetabinom.ab(successes, totals, alpha, beta, log = TRUE))
  }

  # use the method of moments for starting parameters
  mm_estimate <- estimate_beta_mm(successes / totals)
  m <- stats4::mle(ll, start = list(alpha = mm_estimate$alpha,
                            beta = mm_estimate$beta),
           method = "L-BFGS-B")
  co <- m@coef
  data.frame(alpha = co[1], beta = co[2])
}
