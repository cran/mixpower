test_that("Gaussian random-intercept SD flows from assumptions (regression)", {
  d <- mp_design(clusters = list(subject = 60), trials_per_cell = 4)

  make_scn <- function(sd) {
    mp_scenario_lme4(
      y ~ condition + (1 | subject),
      design = d,
      assumptions = mp_assumptions(
        fixed_effects = list("(Intercept)" = 0, condition = 0.3),
        random_effects = list(subject = list(intercept_sd = sd)),
        residual_sd = 1
      )
    )
  }

  scn_lo <- make_scn(0.01)
  scn_hi <- make_scn(3.0)

  set.seed(1)
  dat_lo <- scn_lo$engine$simulate_fun(scn_lo, seed = 1)
  set.seed(1)
  dat_hi <- scn_hi$engine$simulate_fun(scn_hi, seed = 1)

  # Between-subject variance of outcome means must track the specified SD.
  # Before the fix the SD was hardcoded to 1 and this ratio was ~1.
  v_lo <- stats::var(tapply(dat_lo$y, dat_lo$subject, mean))
  v_hi <- stats::var(tapply(dat_hi$y, dat_hi$subject, mean))
  expect_gt(v_hi, v_lo * 5)
})

test_that("legacy icc still works but warns and maps to intercept_sd", {
  options(mixpower.icc_deprecation_warned = FALSE)
  expect_warning(
    a <- mp_assumptions(
      fixed_effects = list("(Intercept)" = 0, condition = 0.3),
      icc = list(subject = 0.7),
      residual_sd = 1
    ),
    "deprecated"
  )
  expect_equal(a$random_effects$subject$intercept_sd, 0.7)
  expect_equal(.mp_re_intercept_sd(a, "subject"), 0.7)
})

test_that("explicit random_effects takes precedence over legacy icc", {
  options(mixpower.icc_deprecation_warned = TRUE) # silence for this check
  a <- mp_assumptions(
    fixed_effects = list(condition = 0.3),
    random_effects = list(subject = list(intercept_sd = 0.5)),
    icc = list(subject = 0.9)
  )
  expect_equal(.mp_re_intercept_sd(a, "subject"), 0.5)
})

test_that("sensitivity can vary random_effects.<group>.intercept_sd", {
  skip_if_not_installed("lme4")
  skip_on_cran()

  d <- mp_design(clusters = list(subject = 25), trials_per_cell = 4)
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = 0.3),
    random_effects = list(subject = list(intercept_sd = 0.5)),
    residual_sd = 1
  )
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)

  out <- mp_sensitivity(
    scn,
    vary = list(`random_effects.subject.intercept_sd` = c(0.2, 1.0)),
    nsim = 4,
    seed = 1
  )
  expect_s3_class(out, "mp_sensitivity")
  expect_equal(out$results$`random_effects.subject.intercept_sd`, c(0.2, 1.0))

  expect_error(
    mp_sensitivity(scn, vary = list(`random_effects.subject` = c(0.2, 1.0)), nsim = 2),
    "intercept_sd"
  )
})
