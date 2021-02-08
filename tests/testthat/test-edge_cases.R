context("edge case")

set.seed(2017)

obs <- 500
dat <- data.frame(k=rep(0, obs), n=rep(10, obs))


test_that("add_ebb_estimate does not error if all counts are 0", {
  testthat::expect_error(ebbr::add_ebb_estimate(dat, k, n), NA) # expect NO error
})
