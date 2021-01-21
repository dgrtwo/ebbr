#' Add columns representing a per-observation proportion test
#'
#' Run on the result of \code{\link{add_ebb_estimate}}, or of \code{augment}
#' on an ebb_prior object from \code{ebb_fit_prior}. This adds columns with the
#' posterior error probability (PEP) and the qvalue.
#'
#' @param tbl A table that includes .alpha1 and .beta1 parameters for each
#' observation, typically returned from \code{\link{add_ebb_estimate}} or
#' from \code{augment}.
#' @param threshold The proportion to which each observation is compared.
#' @param alternative Alternative hypothesis. For example, if the alternative
#' is "greater", the PEP will be the posterior probability that the true
#' value is lower than the threshold.
#' @param sort Optionally, whether to sort the table in order of ascending
#' posterior error probability.
#' @param approx Whether to use a normal approximation to the beta. Used
#' only when comparing to another beta.
#'
#' @examples
#'
#' library(dplyr)
#' set.seed(2017)
#'
#' obs <- 1000
#' dat <- tibble(prob = rbeta(obs, 10, 40),
#'                   total = round(rlnorm(obs, 6, 2)) + 1,
#'                   x = rbinom(obs, total, prob))
#'
#' eb <- add_ebb_estimate(dat, x, total)
#'
#' add_ebb_prop_test(eb, .25)
#' add_ebb_prop_test(eb, .25, sort = TRUE)
#' add_ebb_prop_test(eb, .3, sort = TRUE)
#' add_ebb_prop_test(eb, .4, sort = TRUE)
#'
#' # comparing the actual p to the posterior probability
#' # that p is under .25
#' library(ggplot2)
#'
#' ggplot(add_ebb_prop_test(eb, .25), aes(prob, .pep, color = log10(total))) +
#'   geom_point() +
#'   geom_vline(xintercept = .25, color = "red", lty = 2)
#'
#' @export
add_ebb_prop_test <- function(tbl, threshold,
                             alternative = c("greater", "less"),
                             sort = FALSE,
                             approx = FALSE) {
  alternative <- match.arg(alternative)

  if (!is.data.frame(tbl)) {
    stop("Input to add_ebb_prop_test must inherit from data frame")
  }
  if (!all(c(".alpha1", ".beta1") %in% colnames(tbl))) {
    stop("Input to add_ebb_prop_test must have .alpha1 and .beta1 columns, ",
         "such as the outpuf of add_ebb_estimate or augment on an ebb_prior ",
         "object.")
  }

  if (length(threshold) == 1) {
    tbl$.pep <- stats::pbeta(threshold, tbl$.alpha1, tbl$.beta1)
  } else if (length(threshold) == 2) {
    # alpha and beta parameter of another beta distribution
    tbl$.pep <- h(threshold[1], threshold[2], tbl$.alpha1, tbl$.beta1, approx = approx)
  } else {
    stop("Threshold should be a vector of length 1 or 2")
  }

  if (alternative == "less") {
    tbl$.pep <- 1 - tbl$.pep
  }

  tbl$.qvalue <- dplyr::cummean(sort(tbl$.pep))[rank(tbl$.pep)]

  if (sort) {
    tbl <- tbl[order(tbl$.pep), ]
  }
  tbl
}
