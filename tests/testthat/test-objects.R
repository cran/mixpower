testthat::test_that("mp_design validates inputs", {
  d <- mp_design(clusters = list(subject = 10), trials_per_cell = 2)
  expect_true(inherits(d, "mp_design"))

  expect_error(mp_design(list(subject = -1), 1), "must be > 0")
  expect_error(mp_design(list(), 1), "named")
  expect_error(mp_design(list(subject = 10), 0), "must be > 0")
})

testthat::test_that("mp_assumptions validates inputs", {
  a <- mp_assumptions(fixed_effects = list(condition = 0.4), residual_sd = 1)
  expect_true(inherits(a, "mp_assumptions"))

  expect_error(mp_assumptions(list(condition = NA)), "without NA")
  expect_error(mp_assumptions(list(condition = 0.2), icc = list(subject = 1)), "in \\[0, 1\\)")
  expect_error(mp_assumptions(list(condition = 0.2), residual_sd = -1), "non-negative")
})

testthat::test_that("mp_scenario validates inputs", {
  d <- mp_design(list(subject = 10), 1)
  a <- mp_assumptions(list(condition = 0.2), residual_sd = 1)

  s <- mp_scenario(y ~ condition, d, a, test = "wald")
  expect_true(inherits(s, "mp_scenario"))

  expect_error(mp_scenario("y ~ x", d, a), "formula")
  expect_error(mp_scenario(y ~ condition, list(), a), "mp_design")
  expect_error(mp_scenario(y ~ condition, d, list()), "mp_assumptions")
})
