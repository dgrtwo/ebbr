#' Tidy the results of empirical Bayes shrinkage
#'
#' @param x An object of class "ebbinom"
#' @param data For augment, the original data that
#' should be appended to
#' @param ... Extra arguments (not used)
#'
#' @name ebbinom_tidiers
#'
#' @return \code{augment.ebbinom} returns the original
#' data frame (if provided as the \code{data} argument),
#' with additional columns corresponding
#' to each of the ones in \code{x$table}, including the
#' new alpha and beta posterior parameters for each
#' observation.
#'
#' \code{glance.ebbinom} returns the prior as it has been
#' estimated from the full dataset, including
#'   \item{alpha}{Estimated alpha (shape1) parameter}
#'   \item{beta}{Estimated beta (shape2) parameter}
#'   \item{prior.estimate}{alpha / (alpha + beta): the prior mean}
#'
#' @importFrom broom tidy augment glance
#'
#' @export
augment.ebbinom <- function(x, data, ...) {
  if (missing(data)) {
    return(x$table)
  }

  cbind(data, x$table)
}


#' @rdname ebbinom_tidiers
#' @export
glance.ebbinom <- function(x, ...) {
  ret <- x$prior
  ret$prior.estimate <- ret$alpha / (ret$alpha + ret$beta)
  ret
}

#' @export
"tidy"
#' @export
"augment"
#' @export
"glance"
