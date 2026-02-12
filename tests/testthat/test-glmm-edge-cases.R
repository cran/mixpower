test_that("binomial backend handles extreme effects without crashing", {
  skip_if_not_installed("lme4")

  d <- mp_design(clusters = list(subject = 15), trials_per_cell = 6)
  a <- mp_assumptions(
    fixed_effects = list(`(Intercept)` = 0, condition = 5),
    residual_sd = 1,
    icc = list(subject = 0.1)
  )

  scn <- mp_scenario_lme4_binomial(
    y ~ condition + (1 | subject),
    design = d,
    assumptions = a,
    test_method = "wald"
  )

  out <- mp_power(scn, nsim = 6, seed = 123)
  expect_s3_class(out, "mp_power")
  expect_true(out$diagnostics$fail_rate <= 1)
})

test_that("binomial LRT returns NA rather than crashing on irregular LRT output", {
  skip_if_not_installed("lme4")

  d <- mp_design(clusters = list(subject = 10), trials_per_cell = 3)
  a <- mp_assumptions(
    fixed_effects = list(`(Intercept)` = 0, condition = 4),
    residual_sd = 1,
    icc = list(subject = 0.1)
  )

  scn <- mp_scenario_lme4_binomial(
    y ~ condition + (1 | subject),
    design = d,
    assumptions = a,
    test_method = "lrt",
    null_formula = y ~ 1 + (1 | subject)
  )

  out <- mp_power(scn, nsim = 4, seed = 1)
  expect_s3_class(out, "mp_power")
  expect_true(out$diagnostics$fail_rate <= 1)
})
