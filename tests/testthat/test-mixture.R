context("mixture")

set.seed(2017)

sim_data <- tibble(cluster = 1:2,
                       alpha = c(30, 35),
                       beta = c(70, 15),
                       size = c(300, 700)) %>%
  group_by(across()) %>%
  summarize(p = rbeta(size, alpha, beta), .groups = "drop") %>%
  mutate(total = round(rlnorm(n(), 5, 2) + 1),
         x = rbinom(n(), total, p))

test_that("Can fit a mixture model", {
  set.seed(2017)
  mm <- sim_data %>%
    ebb_fit_mixture(x, total)

  td <- tidy(mm)
  means <- sort(td$mean)
  expect_lt(max(abs(means - c(.3, .7))), .05)

  j <- mm$assignments %>%
    count(.cluster) %>%
    inner_join(mm$clusters, by = c(".cluster" = "cluster"))

  expect_true(all(j$n == j$number))
})

test_that("Can fit a mixture model with multiple restarts", {
  set.seed(2016)
  mm <- sim_data %>%
    ebb_fit_mixture(x, total, nstart = 2)

  td <- tidy(mm)
  means <- sort(td$mean)
  expect_lt(max(abs(means - c(.3, .7))), .05)
})
