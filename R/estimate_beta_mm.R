#' Estimate parameters for the beta distribution using the
#' method of moments
#'
#' Estimate the two shape parameters (here called alpha and
#' beta) for the beta distribution using the method of
#' moments.
#'
#' @details See here for more information on the method this uses:
#'
#' \url{http://stats.stackexchange.com/questions/12232}
#'
#' @param successes Vector of successes
#' @param totals Vector of totals
#'
#' @return A one-row data.frame with two columns:
#' \code{alpha} and \code{beta}.
#'
#' set.seed(2015)
#' probs <- rbeta(100, 10, 50)
#' total <- round(rlnorm(100, 5, 2)) + 1
#' successes <- rbinom(100, total, probs)
#'
#' bb <- estimate_beta_mm(successes, total)
#' bb
#'
#' @export
estimate_beta_mm <- function(successes, totals) {
  x <- successes / totals
  mu <- mean(x)
  vr <- var(x)
  alpha <- ((1 - mu) / vr - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)

  data.frame(alpha = alpha, beta = beta)
}
