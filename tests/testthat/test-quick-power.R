test_that("mp_quick_power returns mp_power and matches full pipeline", {
  skip_on_cran()
  formula <- y ~ condition + (1 | subject)
  clusters <- list(subject = 30)
  trials_per_cell <- 4
  fixed_effects <- list(`(Intercept)` = 0, condition = 0.3)
  residual_sd <- 1
  nsim <- 10
  seed <- 1

  quick <- mp_quick_power(
    formula,
    clusters = clusters,
    trials_per_cell = trials_per_cell,
    fixed_effects = fixed_effects,
    residual_sd = residual_sd,
    nsim = nsim,
    seed = seed
  )

  expect_s3_class(quick, "mp_power")
  expect_equal(quick$nsim, nsim)
  expect_true(is.numeric(quick$power))

  d <- mp_design(clusters = clusters, trials_per_cell = trials_per_cell)
  a <- mp_assumptions(fixed_effects = fixed_effects, residual_sd = residual_sd)
  scn <- mp_scenario_lme4(formula, d, a)
  full <- mp_power(scn, nsim = nsim, seed = seed)
  expect_equal(quick$power, full$power)
})

test_that("mp_quick_power passes ... to mp_power", {
  skip_on_cran()
  res <- mp_quick_power(
    y ~ condition + (1 | subject),
    clusters = list(subject = 25),
    trials_per_cell = 2,
    fixed_effects = list(condition = 0.2),
    residual_sd = 1,
    nsim = 5,
    seed = 42,
    failure_policy = "exclude",
    conf_level = 0.9
  )
  expect_equal(res$failure_policy, "exclude")
  expect_equal(res$conf_level, 0.9)
})
