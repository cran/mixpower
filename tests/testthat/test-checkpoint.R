test_that("checkpointed power equals a single mp_power run", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  d <- mp_design(list(subject = 25), trials_per_cell = 6)
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = 0.4),
    random_effects = list(subject = list(intercept_sd = 0.5)),
    residual_sd = 1
  )
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)

  f <- tempfile(fileext = ".rds")
  on.exit(unlink(f), add = TRUE)

  ck <- suppressMessages(mp_power_checkpoint(scn, nsim = 30, file = f,
                                             batch_size = 10, seed = 1))
  ref <- mp_power(scn, nsim = 30, seed = 1)

  expect_s3_class(ck, "mp_power")
  expect_equal(ck$power, ref$power)
  expect_equal(ck$sims$p_value, ref$sims$p_value)
  expect_true(file.exists(f))
})

test_that("checkpoint resumes and can grow nsim", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  d <- mp_design(list(subject = 25), trials_per_cell = 6)
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = 0.4),
    random_effects = list(subject = list(intercept_sd = 0.5)),
    residual_sd = 1
  )
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)

  f <- tempfile(fileext = ".rds")
  on.exit(unlink(f), add = TRUE)

  # Run 10, then resume to 30 using the same file.
  suppressMessages(mp_power_checkpoint(scn, nsim = 10, file = f, batch_size = 10, seed = 7))
  resumed <- suppressMessages(mp_power_checkpoint(scn, nsim = 30, file = f,
                                                  batch_size = 10, seed = 7))
  ref <- mp_power(scn, nsim = 30, seed = 7)
  expect_equal(resumed$sims$p_value, ref$sims$p_value)
})

test_that("mp_power_checkpoint validates inputs", {
  d <- mp_design(list(subject = 10), trials_per_cell = 4)
  a <- mp_assumptions(list("(Intercept)" = 0, condition = 0.3),
                      random_effects = list(subject = list(intercept_sd = 0.4)),
                      residual_sd = 1)
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
  expect_error(mp_power_checkpoint(scn, nsim = 10, file = tempfile(), seed = NULL),
               "non-NULL `seed`")
})
