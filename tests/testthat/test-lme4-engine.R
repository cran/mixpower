testthat::test_that("lme4 backend produces runnable scenario and mp_power executes", {
  skip_if_not_installed("lme4")

  d <- mp_design(clusters = list(subject = 30), trials_per_cell = 5)
  a <- mp_assumptions(fixed_effects = list(condition = 0.5), residual_sd = 1)

  s <- mp_scenario_lme4(
    y ~ condition + (1 | subject),
    design = d,
    assumptions = a,
    predictor = "condition",
    subject = "subject",
    outcome = "y"
  )

  res <- mp_power(s, nsim = 20, seed = 123)  # small for CRAN
  expect_true(inherits(res, "mp_power"))
  expect_true(is.finite(res$power) || is.na(res$power))
  expect_true(nrow(res$sims) == 20)
})

testthat::test_that("power is higher with larger effect (very small nsim sanity check)", {
  skip_if_not_installed("lme4")

  d <- mp_design(clusters = list(subject = 25), trials_per_cell = 6)

  a_lo <- mp_assumptions(fixed_effects = list(condition = 0.0), residual_sd = 1)
  a_hi <- mp_assumptions(fixed_effects = list(condition = 0.8), residual_sd = 1)

  s_lo <- mp_scenario_lme4(
    y ~ condition + (1 | subject),
    design = d,
    assumptions = a_lo,
    predictor = "condition",
    subject = "subject",
    outcome = "y"
  )

  s_hi <- mp_scenario_lme4(
    y ~ condition + (1 | subject),
    design = d,
    assumptions = a_hi,
    predictor = "condition",
    subject = "subject",
    outcome = "y"
  )

  r_lo <- mp_power(s_lo, nsim = 25, seed = 1)
  r_hi <- mp_power(s_hi, nsim = 25, seed = 1)

  # With tiny nsim this is noisy; we only check directional tendency with slack.
  expect_true(r_hi$power >= r_lo$power - 0.1)
})
