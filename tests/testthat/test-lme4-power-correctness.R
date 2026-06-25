# Regression tests for a defect where the lme4-family Wald test errored on
# every replicate (`base::diag()` on the S4 dpoMatrix from `vcov(merMod)`),
# making `mp_power()` silently return 0 for every lme4 design. The previous
# tests only checked that power was finite, so they never caught it.

test_that("lme4 Gaussian power is near 1 at a strongly-powered design", {
  skip_if_not_installed("lme4")
  skip_on_cran()

  d <- mp_design(clusters = list(subject = 40), trials_per_cell = 8)
  a <- mp_assumptions(
    fixed_effects = list(`(Intercept)` = 0, condition = 0.8),
    residual_sd = 1
  )
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
  res <- mp_power(scn, nsim = 40, seed = 100)

  expect_gt(res$power, 0.9)
  expect_equal(res$diagnostics$fail_rate, 0)
})

test_that("lme4 Gaussian Type I error is near alpha under the null", {
  skip_if_not_installed("lme4")
  skip_on_cran()

  d <- mp_design(clusters = list(subject = 40), trials_per_cell = 8)
  a <- mp_assumptions(
    fixed_effects = list(`(Intercept)` = 0, condition = 0.0),
    residual_sd = 1
  )
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
  res <- mp_power(scn, nsim = 200, seed = 7, alpha = 0.05)

  # Wald-z is mildly anti-conservative in small samples; allow generous slack.
  expect_lt(res$power, 0.15)
})

test_that("lme4 binomial power is high at a strong effect", {
  skip_if_not_installed("lme4")
  skip_on_cran()

  d <- mp_design(clusters = list(subject = 40), trials_per_cell = 8)
  a <- mp_assumptions(
    fixed_effects = list(`(Intercept)` = 0, condition = 1.5),
    random_effects = list(subject = list(intercept_sd = 0.3))
  )
  scn <- mp_scenario_lme4_binomial(
    y ~ condition + (1 | subject),
    design = d, assumptions = a
  )
  res <- mp_power(scn, nsim = 40, seed = 11)
  expect_gt(res$power, 0.8)
})
