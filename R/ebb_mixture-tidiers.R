#' Tidy a mixture of beta-binomials
#'
#' Tidy a mixture model fit by \code{\link{ebb_fit_mixture}}. \code{tidy} returns
#' in the hyperparameters fit for each cluster, while \code{glance} returns the
#' likelihood and other parameters for the full mixture.
#'
#' @param x An ebb_mixture object, such as returned by \code{\link{ebb_fit_mixture}}.
#' @param data The original data that the mixture model was fit on
#' @param ... Extra arguments, not used
#'
#' @return \code{tidy} returns a tibble with one row for each cluster,
#' with columns
#'   \item{cluster}{A character vector with cluster IDs}
#'   \item{alpha}{alpha shape parameter for this cluster}
#'   \item{beta}{beta shape parameter for this cluster}
#'   \item{mean}{alpha / beta, the mean of the beta distribution}
#'   \item{size}{Size of this cluster in the final assignments}
#'   \item{probability}{Prior probability of being in this cluster}
#'
#' \code{glance} returns a one-row data frame with columns:
#'   \item{iter}{Number of iterations}
#'   \item{logLik}{Log-likelihood of the model, given the final
#'   "hard" assignments to each cluster}
#'   \item{AIC}{AIC of the model}
#'   \item{BIC}{BIC of the model}
#'
#' @name ebb_mixture_tidiers
#'
#' @export
tidy.ebb_mixture <- function(x, ...) {
  x$clusters
}

#' @rdname ebb_mixture_tidiers
#' @export
augment.ebb_mixture <- function(x, data, ...) {
  data$.cluster <- x$assignments$cluster
  data$.likelihood <- x$assignments$likelihood
}


#' @rdname ebb_mixture_tidiers
#' @export
glance.ebb_mixture <- function(x, ...) {
  ret <- dplyr::tibble(iter = x$num_iter)
  broom::finish_glance(ret, x)
}
