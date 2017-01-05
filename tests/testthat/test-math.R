context("Mathematical functions")

## helper functions
set.seed(2017)

# higher n is slower but can detect smaller differences
n <- 1e5

# test that h and expected_loss match Monte Carlo simulations of the Beta.
# there is some uncertainty involved, so we go with the approach of testing
# whether the simulation is significantly different from the calculated value,
# and checking that the p-value is *greater* than a particular value.

test_h <- function(a, b, c, d, threshold = 1e-4, ...) {
  hval <- h(a, b, c, d, ...)
  hval_sim <- sum(rbeta(n, a, b) > rbeta(n, c, d))

  # if we're off by even a little, binom.test should detect it, as with this n
  # the binomial test is very powerful.
  pval <- binom.test(hval_sim, n, p = hval)$p.value
  # with default will give false positive 1/10000 tests. Nobody's perfect.
  expect_lt(5e-4, pval)
}

test_random <- function(func, m) {
  # test a function m times with random parameters
  replicate(m, do.call(func, as.list(rgeom(4, .005))))
}

## tests

test_that("h computes the probability Beta(a, b) > Beta(c, d)", {
  test_h(2, 2, 2, 2)
  test_h(9, 15, 15, 20)
  test_h(5, 100, 8, 120)
  test_h(100, 1000, 150, 1010)

  test_h(100, 1000, 150, 1010, approx = TRUE)

  # some random parameters
  test_random(test_h, 5)
})
