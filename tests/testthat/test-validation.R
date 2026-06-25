# Validation suite: Type I calibration, asymptotics, inference plumbing, and
# method guidance. These are the credibility checks that justify trusting a
# mixpower power number.

test_that("mp_calibrate recovers ~alpha for a correctly specified model", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  d <- mp_design(clusters = list(subject = 35), trials_per_cell = 8)
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = 0.4),
    random_effects = list(subject = list(intercept_sd = 0.5)),
    residual_sd = 1
  )
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)

  cal <- mp_calibrate(scn, nsim = 150, seed = 11)
  expect_s3_class(cal, "mp_calibration")
  expect_lt(cal$type1, 0.12)
  expect_output(print(cal), "Type I")

  tab <- mp_report_table(cal)
  expect_equal(tab$verdict, cal$verdict)
  expect_true(tab$type1 >= 0 && tab$type1 <= 1)
})

test_that("mp_calibrate flags an omitted random slope as anti-conservative", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  d <- mp_design(clusters = list(subject = 35), trials_per_cell = 8)

  # DGP has a strong by-subject slope on condition...
  a_slope <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = 0.4),
    random_effects = list(subject = list(intercept_sd = 0.5, slopes = list(condition = 0.8))),
    residual_sd = 1
  )
  # ...but the analysis model omits it (random intercept only) -> inflated Type I.
  scn_mis <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a_slope)

  # Matched (intercept-only DGP + intercept-only model) controls Type I.
  a_int <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = 0.4),
    random_effects = list(subject = list(intercept_sd = 0.5)),
    residual_sd = 1
  )
  scn_ok <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a_int)

  cal_mis <- mp_calibrate(scn_mis, nsim = 150, seed = 7)
  cal_ok <- mp_calibrate(scn_ok, nsim = 150, seed = 7)

  expect_gt(cal_mis$type1, cal_ok$type1)
  expect_gt(cal_mis$type1, 0.12)
  expect_equal(cal_mis$verdict, "anti-conservative")
})

test_that("power increases with sample size and reaches high power (asymptotics)", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = 0.8),
    random_effects = list(subject = list(intercept_sd = 0.5)),
    residual_sd = 1
  )
  mk <- function(n) mp_scenario_lme4(
    y ~ condition + (1 | subject),
    design = mp_design(list(subject = n), trials_per_cell = 6),
    assumptions = a
  )
  p_small <- mp_power(mk(10), nsim = 80, seed = 3)$power
  p_large <- mp_power(mk(60), nsim = 80, seed = 3)$power
  expect_gt(p_large, p_small)
  expect_gt(p_large, 0.9)
})

test_that("Wald p-value plumbing matches a direct lmerTest computation", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  d <- mp_design(clusters = list(subject = 30), trials_per_cell = 6)
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = 0.5),
    random_effects = list(subject = list(intercept_sd = 0.5)),
    residual_sd = 1
  )
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)

  dat <- scn$engine$simulate_fun(scn)
  fit <- scn$engine$fit_fun(dat, scn)
  p_engine <- scn$engine$test_fun(fit, scn)$p_value

  cf <- stats::coef(summary(fit))
  z <- cf["condition", "Estimate"] / cf["condition", "Std. Error"]
  p_direct <- 2 * stats::pnorm(abs(z), lower.tail = FALSE)
  expect_equal(p_engine, as.numeric(p_direct), tolerance = 1e-8)
})

test_that("mp_recommend_method cautions on few clusters and clears on many", {
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = 0.4),
    random_effects = list(subject = list(intercept_sd = 0.5)),
    residual_sd = 1
  )
  few <- mp_scenario_lme4(
    y ~ condition + (1 | subject),
    design = mp_design(list(subject = 10), trials_per_cell = 8), assumptions = a
  )
  many <- mp_scenario_lme4(
    y ~ condition + (1 | subject),
    design = mp_design(list(subject = 200), trials_per_cell = 8), assumptions = a
  )

  rf <- mp_recommend_method(few)
  expect_true(rf$caution)
  expect_true(any(c("kenward-roger", "satterthwaite") %in% rf$recommended))
  expect_output(print(rf), "recommended")

  rm <- mp_recommend_method(many)
  expect_false(rm$caution)
})
