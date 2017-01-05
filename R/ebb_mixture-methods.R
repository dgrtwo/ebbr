#' Log-likelihood of an ebb_mixture object
#'
#' @param object An ebb_mixture object returned by \code{\link{ebb_fit_mixture}}.
#' @param ... Extra arguments, not used.
#'
#' @export
logLik.ebb_mixture <- function(object, ...) {
  ll <- sum(log(object$assignments$.likelihood))

  attr(ll, "df") <- 2 * nrow(object$clusters)

  if (object$vary_size) {
    attr(ll, "df") <- attr(ll, "df") + nrow(object$clusters) - 1
  }

  attr(ll, "nobs") <- nrow(object$assignments)

  ll
}


#' Print an ebb_mixture object
#'
#' @param x An ebb_mixture object returned by \code{\link{ebb_fit_mixture}}.
#' @param ... Extra arguments, not used.
#'
#' @export
print.ebb_mixture <- function(x, ...) {
  cat("A mixture of", nrow(x$clusters), "beta-binomial distributions\n\n")
  cat("Cluster parameters:\n")
  print(broom::tidy(x))
}
