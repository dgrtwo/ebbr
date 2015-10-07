#' Add a layer showing a beta estimation
#'
#' @param estimates A data.frame returned from an estimation
#' function, with alpha and beta parameters
#' @param color Color of overlaid line
#' @param ... Extra arguments passed on to stat_function
#'
#' @export
geom_beta <- function(estimates, color = "red", ...) {
  fun <- function(x) dbeta(x, estimates$alpha, estimates$beta)
  ggplot2::stat_function(fun = fun, color = color, ...)
}
