test_that("lme4 backend supports Wald and LRT methods", {
  skip_if_not_installed("lme4")

  d <- mp_design(clusters = list(subject = 20), trials_per_cell = 4)
  a <- mp_assumptions(
    fixed_effects = list(`(Intercept)` = 0, condition = 0.4),
    residual_sd = 1,
    icc = list(subject = 0.1)
  )

  scn_wald <- mp_scenario_lme4(
    y ~ condition + (1 | subject),
    design = d,
    assumptions = a,
    predictor = "condition",
    subject = "subject",
    outcome = "y",
    test_method = "wald"
  )

  scn_lrt <- mp_scenario_lme4(
    y ~ condition + (1 | subject),
    design = d,
    assumptions = a,
    predictor = "condition",
    subject = "subject",
    outcome = "y",
    test_method = "lrt",
    null_formula = y ~ 1 + (1 | subject)
  )

  out_wald <- mp_power(scn_wald, nsim = 6, seed = 101)
  out_lrt  <- mp_power(scn_lrt, nsim = 6, seed = 101)

  expect_s3_class(out_wald, "mp_power")
  expect_s3_class(out_lrt, "mp_power")
  expect_true(is.finite(out_wald$power) || is.na(out_wald$power))
  expect_true(is.finite(out_lrt$power) || is.na(out_lrt$power))
})

test_that("LRT requires explicit null_formula", {
  skip_if_not_installed("lme4")

  d <- mp_design(clusters = list(subject = 10), trials_per_cell = 3)
  a <- mp_assumptions(
    fixed_effects = list(`(Intercept)` = 0, condition = 0.3),
    residual_sd = 1
  )

  expect_error(
    mp_scenario_lme4(
      y ~ condition + (1 | subject),
      design = d,
      assumptions = a,
      test_method = "lrt"
    ),
    "null_formula"
  )
})
