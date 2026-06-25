test_that("converters round-trip", {
  expect_equal(mp_beta_to_d(mp_d_to_beta(0.5, sd = 1.3), sd = 1.3), 0.5)
  expect_equal(mp_beta_to_r2(mp_r2_to_beta(0.06, sd_resid = 1.2), sd_resid = 1.2), 0.06)
  expect_equal(mp_sd_to_icc(mp_icc_to_sd(0.15, sd_resid = 1), sd_resid = 1), 0.15)
  expect_equal(mp_logodds_to_or(mp_or_to_logodds(1.8)), 1.8)
})

test_that("converters compute the documented quantities", {
  expect_equal(mp_d_to_beta(0.4, sd = 2), 0.8)
  expect_equal(mp_icc_to_sd(0.5, sd_resid = 1), 1) # tau = sigma when icc = .5
  expect_equal(mp_or_to_logodds(exp(0.7)), 0.7)
  expect_equal(mp_t_to_beta(2, 0.1), 0.2)
  expect_equal(mp_f_to_beta(4, 0.1), 0.2) # sqrt(F) = |t|
})

test_that("converters validate inputs", {
  expect_error(mp_r2_to_beta(1), "out of range")
  expect_error(mp_icc_to_sd(-0.1), "out of range")
  expect_error(mp_or_to_logodds(0), "> 0")
  expect_error(mp_d_to_beta(0.5, sd = -1), "out of range")
})

test_that("mp_d_to_beta reproduces the target d in a large-sample simulation", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  sd_total <- sqrt(0.5^2 + 1^2)
  beta <- mp_d_to_beta(0.5, sd = sd_total)

  d <- mp_design(list(subject = 400), trials_per_cell = 2,
                 predictors = list(group = list(type = "binary", level = "between")))
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, group = beta),
    random_effects = list(subject = list(intercept_sd = 0.5)),
    residual_sd = 1
  )
  scn <- mp_scenario_lme4(y ~ group + (1 | subject), design = d,
                          assumptions = a, predictor = "group")

  set.seed(123)
  dat <- scn$engine$simulate_fun(scn)
  emp_d <- (mean(dat$y[dat$group == 1]) - mean(dat$y[dat$group == 0])) / stats::sd(dat$y)
  expect_equal(emp_d, 0.5, tolerance = 0.12)
})
