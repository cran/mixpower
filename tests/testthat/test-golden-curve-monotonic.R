test_that("power curve estimate increases with effect size (monotonic trend)", {
  skip_on_cran()
  d <- mp_design(clusters = list(subject = 30), trials_per_cell = 4)
  a_small <- mp_assumptions(
    fixed_effects = list(`(Intercept)` = 0, condition = 0.05),
    residual_sd = 1
  )
  a_large <- mp_assumptions(
    fixed_effects = list(`(Intercept)` = 0, condition = 0.6),
    residual_sd = 1
  )
  f <- y ~ condition + (1 | subject)
  sc_small <- mp_scenario_lme4(f, design = d, assumptions = a_small)
  sc_large <- mp_scenario_lme4(f, design = d, assumptions = a_large)

  p_small <- mp_power(sc_small, nsim = 25, seed = 800)$power
  p_large <- mp_power(sc_large, nsim = 25, seed = 800)$power

  expect_true(all(c(p_small, p_large) >= 0, na.rm = TRUE))
  expect_true(all(c(p_small, p_large) <= 1, na.rm = TRUE))
  expect_true(p_large >= p_small)
})
