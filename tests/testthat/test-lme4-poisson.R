test_that("poisson lme4 backend runs end-to-end", {
  skip_if_not_installed("lme4")

  d <- mp_design(clusters = list(subject = 20), trials_per_cell = 4)
  a <- mp_assumptions(
    fixed_effects = list(`(Intercept)` = 0, condition = 0.5),
    residual_sd = 1,
    icc = list(subject = 0.3)
  )

  scn <- mp_scenario_lme4_poisson(
    y ~ condition + (1 | subject),
    design = d,
    assumptions = a,
    test_method = "wald"
  )

  out <- mp_power(scn, nsim = 6, seed = 10)
  expect_s3_class(out, "mp_power")
  expect_true(is.numeric(out$power))
  expect_true(out$diagnostics$fail_rate <= 1)
})

test_that("poisson LRT requires explicit null_formula", {
  skip_if_not_installed("lme4")

  d <- mp_design(clusters = list(subject = 10), trials_per_cell = 3)
  a <- mp_assumptions(
    fixed_effects = list(`(Intercept)` = 0, condition = 0.3),
    residual_sd = 1
  )

  expect_error(
    mp_scenario_lme4_poisson(
      y ~ condition + (1 | subject),
      design = d,
      assumptions = a,
      test_method = "lrt"
    ),
    "null_formula"
  )
})
