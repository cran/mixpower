test_that("mp_sensitivity works for Poisson scenarios", {
  skip_if_not_installed("lme4")

  d <- mp_design(clusters = list(subject = 20), trials_per_cell = 4)
  a <- mp_assumptions(
    fixed_effects = list(`(Intercept)` = 0, condition = 0.4),
    residual_sd = 1,
    icc = list(subject = 0.3)
  )

  scn <- mp_scenario_lme4_poisson(
    y ~ condition + (1 | subject),
    design = d,
    assumptions = a,
    test_method = "wald"
  )

  out <- mp_sensitivity(
    scn,
    vary = list(`fixed_effects.condition` = c(0.2, 0.4)),
    nsim = 6,
    seed = 42
  )

  expect_s3_class(out, "mp_sensitivity")
  expect_true(all(c("estimate", "failure_rate") %in% names(out$results)))
})

test_that("mp_sensitivity works for NB scenarios", {
  skip_if_not_installed("lme4")

  d <- mp_design(clusters = list(subject = 20), trials_per_cell = 4)
  a <- mp_assumptions(
    fixed_effects = list(`(Intercept)` = 0, condition = 0.4),
    residual_sd = 1,
    icc = list(subject = 0.3)
  )
  a$theta <- 1.5

  scn <- mp_scenario_lme4_nb(
    y ~ condition + (1 | subject),
    design = d,
    assumptions = a,
    test_method = "wald"
  )

  out <- mp_sensitivity(
    scn,
    vary = list(`fixed_effects.condition` = c(0.2, 0.4)),
    nsim = 6,
    seed = 42
  )

  expect_s3_class(out, "mp_sensitivity")
  expect_true(all(c("estimate", "failure_rate") %in% names(out$results)))
})
