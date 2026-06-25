test_that("streaming aggregate matches full for power estimate", {
  skip_on_cran()
  d <- mp_design(clusters = list(subject = 15), trials_per_cell = 2)
  a <- mp_assumptions(
    fixed_effects = list(`(Intercept)` = 0, condition = 0.35),
    residual_sd = 1
  )
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)

  full <- mp_power(scn, nsim = 40, seed = 42, aggregate = "full")
  stream <- mp_power(scn, nsim = 40, seed = 42, aggregate = "streaming", keep = "minimal")

  expect_equal(full$power, stream$power)
  expect_equal(full$mcse, stream$mcse, tolerance = 1e-10)
  expect_equal(full$diagnostics$fail_rate, stream$diagnostics$fail_rate)
  expect_equal(nrow(stream$sims), 0L)
  expect_equal(attr(stream$sims, "aggregate"), "streaming")
})

test_that("streaming rejects keep other than minimal", {
  d <- mp_design(clusters = list(subject = 10), trials_per_cell = 1)
  a <- mp_assumptions(fixed_effects = list(condition = 0.2), residual_sd = 1)
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
  expect_error(
    mp_power(scn, nsim = 5, seed = 1, aggregate = "streaming", keep = "fits"),
    "streaming"
  )
})
