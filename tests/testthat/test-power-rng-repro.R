test_that("mp_power is reproducible with fixed seed", {
  d <- mp_design(clusters = list(subject = 16), trials_per_cell = 2)
  a <- mp_assumptions(fixed_effects = list(`(Intercept)` = 0, condition = 0.3), residual_sd = 1)
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)

  r1 <- mp_power(scn, nsim = 12, seed = 555)
  r2 <- mp_power(scn, nsim = 12, seed = 555)
  expect_equal(r1$sims$p_value, r2$sims$p_value)
  expect_equal(r1$power, r2$power)
})
