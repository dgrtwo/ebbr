#' Extract the data frame with the variables needed to evaluate an
#' empirical Bayes model fit
#'
#' @param formula An ebb_prior object
#' @param ... Extra arguments, not used
#'
#' @export
model.frame.ebb_prior <- function(formula, ...) {
  formula$model
}


#' Print an ebb_prior object
#'
#' @param x An ebb_prior object
#' @param ... Extra arguments, not used
#'
#' @export
print.ebb_prior <- function(x, ...) {
  cat(paste("Empirical Bayes binomial fit with method", x$method), "\n")
  cat("Parameters:\n")
  print(x$parameters)
}


#' Log-likelihood of an ebb_prior object
#'
#' @param object An "ebb_prior" object
#' @param ... Extra arguments, not used
#'
#' @export
logLik.ebb_prior <- function(object, ...) {
  d <- stats::model.frame(object)
  x_value <- lazyeval::lazy_eval(object$terms$x, d)
  n_value <- lazyeval::lazy_eval(object$terms$n, d)

  if (object$method == "gamlss") {
    au <- broom::augment(object)
    ret <- sum(VGAM::dbetabinom.ab(x_value, n_value,
                                   au$.alpha0, au$.beta0, log = TRUE))

    attr(ret, "df") <- nrow(tidy(object))
  } else {
    a <- object$parameters$alpha
    b <- object$parameters$beta
    ret <- sum(VGAM::dbetabinom.ab(x_value, n_value, a, b, log = TRUE))
    attr(ret, "df") <- 2
  }
  attr(ret, "nobs") <- nrow(d)
  ret
}
