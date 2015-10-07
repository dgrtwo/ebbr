#' Estimate parameters for the beta distribution using the
#' method of moments.
#'
#' Estimate the two shape parameters (here called alpha and
#' beta) for the beta distribution using the method of
#' moments.
#'
#' @param x A numeric vector
#'
#' @return A data.frame with two values: alpha and beta.
#'
#' @export
estimate_beta_mm <- function(x) {
  mu <- mean(x)
  vr <- var(x)
  alpha <- ((1 - mu) / vr - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)

  data.frame(alpha = alpha, beta = beta)
}
