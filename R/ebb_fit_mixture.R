#' Fit a mixture of beta-binomial distributions
#'
#' @param tbl A table.
#' @param x An expression for the number of successes, evaluated within the table.
#' @param n An expression for the total number of trials, evaluated within the table.
#' @param clusters Number of clusters, default 2
#' @param iter_max Maximum number of iterations to perform
#' @param nstart Number of random restarts
#' @param vary_size Allow each cluster to have a prior probability. Use caution as
#' this may lead to clusters being lost entirely. If setting this to true, it may
#' help to use a high number of random restarts.
#' @param method Method passed on to \code{\link{ebb_fit_prior}}.
#' @param ... Extra arguments passed on to \code{\link{ebb_fit_prior}}.
#'
#' @examples
#'
#' library(dplyr)
#' library(tidyr)
#' library(purrr)
#'
#' # simulate some data
#' set.seed(2017)
#' sim_data <- tibble(cluster = 1:2,
#'                        alpha = c(30, 35),
#'                        beta = c(70, 15),
#'                        size = c(300, 700)) %>%
#'   by_row(~ rbeta(.$size, .$alpha, .$beta)) %>%
#'   unnest(p = .out) %>%
#'   mutate(total = round(rlnorm(n(), 5, 2) + 1),
#'          x = rbinom(n(), total, p))
#'
#' mm <- ebb_fit_mixture(sim_data, x, total)
#' mm
#'
#' # assignments of points to clusters
#' mm$assignments
#'
#' # how accurate was it?
#' mm$assignments %>%
#'   count(cluster, .cluster)
#'
#' library(ggplot2)
#' ggplot(mm$assignments, aes(x / total, fill = .cluster)) +
#'   geom_histogram()
#'
#' @export
ebb_fit_mixture <- function(tbl, x, n, clusters = 2, iter_max = 10, nstart = 1L,
                            vary_size = FALSE, method = "mle", ...) {
  ebb_fit_mixture_(tbl, substitute(x), substitute(n), clusters, iter_max,
                   nstart, vary_size = vary_size, method = method, ...)
}

#' @rdname ebb_fit_mixture
#' @export
ebb_fit_mixture_ <- function(tbl, x, n, clusters = 2, iter_max = 10, nstart = 1L, vary_size = FALSE,
                             method = "mle", ...) {
  if (method == "gamlss") {
    stop("Fitting a mixture of gamlss models not yet implemented")
  }

  if (nstart > 1L) {
    mixtures <- purrr::map(seq_len(nstart), ~ ebb_fit_mixture_(tbl, x, n, clusters, iter_max, ...))
    log_liks <- purrr::map_dbl(mixtures, stats::logLik)
    return(mixtures[[which.max(log_liks)]])
  }

  est <- function(.) {
    eb <- ebb_fit_prior(., x, n, method = method, ...)

    dplyr::tibble(alpha = eb$parameters$alpha,
                      beta = eb$parameters$beta,
                      mean = alpha / (alpha + beta),
                      number = nrow(.))
  }

  iterate <- function(state, iteration) {
    if (is.null(state)) {
      # already converged
      return(NULL)
    }

    fits <- state$assignments %>%
      tidyr::nest(-.cluster) %>%
      tidyr::unnest(purrr::map(data, est), .drop = TRUE)

    if (vary_size) {
      fits$probability <- fits$number / nrow(fits)
    } else {
      fits$probability <- 1 / nrow(fits)
    }

    assignments <- state$assignments %>%
      dplyr::select(id, x, n) %>%
      tidyr::crossing(fits) %>%
      dplyr::mutate(.likelihood = probability * VGAM::dbetabinom.ab(x, n, alpha, beta)) %>%
      dplyr::arrange(-.likelihood) %>%
      dplyr::distinct(id, .keep_all = TRUE) %>%
      dplyr::select(id, x, n, .cluster, .likelihood) %>%
      dplyr::arrange(id)

    if (all(state$assignments$.cluster == assignments$.cluster)) {
      # converged
      return(NULL)
    }
    list(assignments = assignments, fits = fits)
  }

  # initialization
  d <- dplyr::tibble(id = seq_len(nrow(tbl)),
                         x = eval(x, tbl),
                         n = eval(n, tbl),
                         .cluster = as.character(sample(clusters, nrow(tbl), replace = TRUE)))
  state <- list(assignments = d, fits = NULL)

  iterations_raw <- purrr::accumulate(seq_len(iter_max), iterate, .init = state)

  if (!is.null(dplyr::last(iterations_raw))) {
    warning("Expectation-maximization algorithm did not converge")
  }

  assignments_fits <- iterations_raw %>%
    purrr::discard(is.null) %>%
    purrr::transpose() %>%
    purrr::map(purrr::map_df, identity, .id = "iteration") %>%
    purrr::map(~ mutate(., iteration = as.integer(iteration)))

  a <- assignments_fits$assignments
  assignments <- dplyr::bind_cols(dplyr::tibble(iteration = a$iteration),
                                  tbl[a$id, , drop = FALSE],
                                  a[c(".cluster", ".likelihood")])

  fits <- assignments_fits$fits %>%
    dplyr::rename(cluster = .cluster)

  iterations <- list(fits = fits, assignments = assignments)

  num_iter <- max(fits$iteration)
  final_fits <- fits[fits$iteration == num_iter, ]
  final_assignments <- assignments[assignments$iteration == num_iter, ]
  final_fits$iteration <- NULL
  final_assignments$iteration <- NULL

  terms <- list(x = x, n = n)

  ret <- list(clusters = final_fits, assignments = final_assignments,
              iterations = iterations, num_iter = num_iter, terms = terms,
              vary_size = vary_size)
  class(ret) <- "ebb_mixture"
  ret
}


crossing_clusters_ <- function(tbl, clusters, x, n) {
  x_value <- eval(x, tbl)
  n_value <- eval(n, tbl)

  cr <- tidyr::crossing(tbl, .cluster = unique(clusters$cluster))
  a <- clusters$alpha[match(cr$.cluster, clusters$cluster)]
  b <- clusters$beta[match(cr$.cluster, clusters$cluster)]
  probability <- clusters$probability[match(cr$.cluster, clusters$cluster)]

  cr$.likelihood <- probability * VGAM::dbetabinom.ab(x_value, n_value, a, b)
  cr
}
