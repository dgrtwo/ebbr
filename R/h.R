#' Calculate the probability one random beta variable is greater than another
#'
#' Find the probability that \code{Beta(a, b) > Beta(c, d)}, using either
#' an exact solution or a normal approximation. Vectorized across
#' a, b, c and d.
#'
#' @param a alpha parameter for first Beta
#' @param b beta parameter for second Beta
#' @param c alpha parameter for first Beta
#' @param d beta parameter for second Beta
#' @param approx whether to use a normal approximation to the beta
#' @param log_h whether to return \code{log(h(a, b, c, d))} rather than
#' \code{h(a, b, c, d)}
#'
#' @details The "exact" solution is exact only for integer values of c.
#'
#' @source The exact version comes from Evan Miller and Chris Stucchio:
#' \url{https://www.chrisstucchio.com/blog/2014/bayesian_ab_decision_rule.html}
#'
#' John D. Cook lays out the normal approximation here:
#' \url{http://www.johndcook.com/fast_beta_inequality.pdf}.
#'
#' @examples
#'
#' h(60, 40, 50, 50)
#' h(60, 40, 50, 50, approx = TRUE)
#'
#' # compare against random simulation:
#' mean(rbeta(1e5, 60, 40) > rbeta(1e5, 50, 50))
#'
#' # it is vectorized across one or multiple arguments
#' a <- 1:19
#' h(a, 20 - a, 10, 10)
#' plot(a, h(a, 20 - a, 10, 10))
#'
#' @export
h <- function(a, b, c, d, approx = FALSE, log_h = FALSE) {
  if (approx) {
    # use normal approximation to the beta
    u1 <- a / (a + b)
    u2 <- c / (c + d)
    var1 <- a * b / ((a + b) ^ 2 * (a + b + 1))
    var2 <- c * d / ((c + d) ^ 2 * (c + d + 1))
    return(stats::pnorm(0, u2 - u1, sqrt(var1 + var2), log.p = log_h))
  }

  if (length(a) > 1 || length(b) > 1 || length(c) > 1 || length(d) > 1) {
    abcd <- cbind(a, b, c, d)
    ret <- apply(abcd, 1, function(r) h(r[1], r[2], r[3], r[4]))
  } else {
    j <- seq.int(0, round(c) - 1)
    log_vals <- lbeta(a + j, b + d) - log(d + j) - lbeta(1 + j, d) - lbeta(a, b)

    # due to floating point error it is possible to be *very* slightly below 0
    ret <- max(1 - sum(exp(log_vals)), 0)
  }

  if (log_h) {
    return(log(ret))
  }
  ret
}

