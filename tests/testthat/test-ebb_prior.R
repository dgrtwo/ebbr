context("ebb_prior")

set.seed(2017)

# simulate beta-binomial data
obs <- 500
dat <- data_frame(prob = rbeta(obs, 300, 700),
                  total = round(rlnorm(obs, 6, 2)) + 1,
                  x = rbinom(obs, total, prob))

test_that("Can fit a beta-binomial prior with maximum likelihood and method of moments", {
  for (method in c("mle", "mm")) {
    ebb_fit <- ebb_fit_prior(dat, x, total, method = method)

    expect_is(ebb_fit, "ebb_prior")
    td <- tidy(ebb_fit)
    expect_is(td, "tbl_df")
    expect_lt(abs(td$mean - .3), .05)

    if (method == "mle") {
      expect_lt(abs(td$alpha + td$beta - 1000), 200)
      expect_is(ebb_fit$fit, "mle")
    }

    expect_true(all(c("alpha", "beta", "logLik", "AIC", "BIC") %in% colnames(glance(ebb_fit))))
  }
})

test_that("Can augment with posterior parameters", {
  # use method of moments for fast fit (even if not accurate)
  ebb_fit <- ebb_fit_prior(dat, x, total, method = "mm")

  td <- tidy(ebb_fit)
  au <- augment(ebb_fit)
  expect_is(au, "tbl_df")

  expect_equal(au$x, dat$x)
  expect_equal(au$total, dat$total)
  expect_equal(au$.alpha1, au$x + td$alpha)
  expect_equal(au$.beta1, au$total - au$x + td$beta)
  expect_equal(au$.fitted, au$.alpha1 / (au$.alpha1 + au$.beta1))
  expect_equal(au$.raw, dat$x / dat$total)
  expect_equal(au$.low, qbeta(.025, au$.alpha1, au$.beta1))
  expect_equal(au$.high, qbeta(.975, au$.alpha1, au$.beta1))

  # with different credible interval
  au2 <- augment(ebb_fit, cred_level = .9)
  expect_equal(au2$.low, qbeta(.05, au$.alpha1, au$.beta1))
  expect_equal(au2$.high, qbeta(.95, au$.alpha1, au$.beta1))
})


test_that("Can augment new data with posterior parameters", {
  # use method of moments for fast fit (even if not accurate)
  ebb_fit <- ebb_fit_prior(dat, x, total, method = "mm")

  d2 <- dat[101:110, ]
  td <- tidy(ebb_fit)
  au <- augment(ebb_fit, newdata = d2)

  expect_is(au, "tbl_df")
  expect_equal(nrow(au), 10)
  expect_equal(d2$x, au$x)
  expect_equal(d2$total, au$total)
  expect_equal(au$.alpha1, d2$x + td$alpha)
  expect_equal(au$.beta1, d2$total - d2$x + td$beta)
})


test_that("Can fit a beta-binomial prior with beta-binomial regression", {
  # simulate beta-binomial data with some predictor
  set.seed(2017)
  obs <- 500
  dat2 <- data_frame(predictor = rnorm(obs),
                     mu = plogis(predictor),
                     prob = rbeta(obs, 100 * mu, 100 * (1 - mu)),
                     total = round(rlnorm(obs, 6, 2)) + 1,
                     x = rbinom(obs, total, prob))

  ebb_bb_reg <- ebb_fit_prior(dat2, x, total, method = "gamlss",
                              mu_predictors = ~ predictor)

  expect_is(ebb_bb_reg, "ebb_prior")
  expect_equal(ebb_bb_reg$method, "gamlss")

  td <- tidy(ebb_bb_reg)
  expect_is(td, "tbl_df")

  expect_equal(nrow(td), 3)
  expect_equal(as.character(td$parameter), c("mu", "mu", "sigma"))
  expect_equal(td$term[2], "predictor")
  expect_lt(abs(td$estimate[2] - 1), .1)

  # augment
  au <- augment(ebb_bb_reg)

  expect_equal(au$x, dat2$x)
  expect_equal(au$total, dat2$total)
  expect_equal(au$.alpha0, au$.mu / au$.sigma)
  expect_equal(au$.alpha1, au$x + au$.alpha0)
  expect_equal(au$.beta1, au$total - au$x + au$.beta0)
  expect_equal(au$.fitted, au$.alpha1 / (au$.alpha1 + au$.beta1))
  expect_equal(au$.raw, dat2$x / dat2$total)

  au2 <- augment(ebb_bb_reg, dat2)

  # predictions should be roughly right
  expect_equal(dat2$mu, au2$mu)
  expect_lt(max(abs(au2$.mu - au2$mu)), .02)

  g <- glance(ebb_bb_reg)
  expect_equal(colnames(g), c("df", "logLik", "AIC", "BIC"))
  expect_equal(attr(logLik(ebb_bb_reg), "df"), 3)
})


test_that("Can add posterior parameters with add_ebb_estimate", {
  eb_added <- dat %>%
    add_ebb_estimate(x, total)

  expect_equal(eb_added$x, dat$x)
  expect_equal(eb_added$total, dat$total)
})
