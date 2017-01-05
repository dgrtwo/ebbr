context("Proportion test")

set.seed(2017)

# simulate beta-binomial data
obs <- 500
dat <- data_frame(prob = rbeta(obs, 300, 700),
                  total = round(rlnorm(obs, 6, 2)) + 1,
                  x = rbinom(obs, total, prob))

test_that("add_ebb_prop_test works relative to a proportion", {
  expect_error(add_ebb_prop_test(dat, .4), "Input to add_ebb_prop_test")

  eb_estimates <- add_ebb_estimate(dat, x, total)

  pt_3 <- add_ebb_prop_test(eb_estimates, .3)
  expect_equal(pt_3$x, dat$x)
  expect_equal(pt_3$total, dat$total)

  s <- add_ebb_prop_test(eb_estimates, .3, sort = TRUE)
  expect_lt(.3, s$prob[1])
  expect_lt(.3, s$prob[2])
  # there may be small differences because of ties
  expect_lt(max(abs(cummean(s$.pep) - s$.qvalue)), .002)
})

test_that("add_ebb_prop_test works relative to another beta", {
  eb_estimates <- add_ebb_estimate(dat, x, total)

  pt_30_70 <- add_ebb_prop_test(eb_estimates, c(30, 70))
  expect_equal(pt_30_70$x, dat$x)
  expect_equal(pt_30_70$total, dat$total)

  s <- add_ebb_prop_test(eb_estimates, c(30, 70), sort = TRUE)
  expect_equal(s$.pep, h(30, 70, s$.alpha1, s$.beta1))
  # there may be small differences because of ties
  expect_lt(max(abs(cummean(s$.pep) - s$.qvalue)), .002)
})
