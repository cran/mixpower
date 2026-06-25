test_that("mp_scenario_glmmtmb_lmm runs mp_power without error", {
  skip_on_cran()
  skip_if_not_installed("glmmTMB")

  d <- mp_design(clusters = list(subject = 20), trials_per_cell = 3)
  a <- mp_assumptions(
    fixed_effects = list(`(Intercept)` = 0, condition = 0.3),
    residual_sd = 1
  )
  scn <- mp_scenario_glmmtmb_lmm(
    y ~ condition + (1 | subject),
    design = d,
    assumptions = a
  )
  res <- mp_power(scn, nsim = 15, seed = 7)
  expect_true(is.numeric(res$power))
  expect_true(res$power >= 0 && res$power <= 1 || is.na(res$power))
})

test_that("glmmTMB and lme4 Gaussian agree at a well-powered design", {
  skip_on_cran()
  skip_if_not_installed("glmmTMB")
  skip_if_not(glmmtmb_tmb_ok(), "glmmTMB built against a different TMB ABI; fits unreliable")

  # Use a strongly-powered design so both engines sit near the power ceiling;
  # this is stable to Monte Carlo noise and to minor cross-engine differences.
  d <- mp_design(clusters = list(subject = 40), trials_per_cell = 8)
  a <- mp_assumptions(
    fixed_effects = list(`(Intercept)` = 0, condition = 0.8),
    residual_sd = 1
  )
  f <- y ~ condition + (1 | subject)
  pl <- mp_power(mp_scenario_lme4(f, design = d, assumptions = a), nsim = 40, seed = 100)
  pt <- mp_power(mp_scenario_glmmtmb_lmm(f, design = d, assumptions = a), nsim = 40, seed = 100)
  expect_gt(pl$power, 0.7)
  expect_gt(pt$power, 0.7)
  expect_lt(abs(pl$power - pt$power), 0.2)
})
