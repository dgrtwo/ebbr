#' Add a layer showing a beta estimation
#'
#' @param estimates A data.frame returned from an estimation
#' function, with alpha and beta parameters
#' @param color Color of overlaid line
#' @param ... Extra arguments passed on to stat_function
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
#' library(ggplot2)
#'
#' dat <- data.frame(successes, total)
#' ggplot(dat, aes(successes / total)) +
#'     geom_histogram(aes(y = ..density..)) +
#'     geom_beta(bb)
#'
#' @export
geom_beta <- function(estimates, color = "red", ...) {
  fun <- function(x) dbeta(x, estimates$alpha, estimates$beta)
  ggplot2::stat_function(fun = fun, color = color, ...)
}
