test_that("df-corrected tests are no more anti-conservative than Wald (same data)", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  skip_if_not_installed("lmerTest")

  # Small sample under the null: Wald-z is anti-conservative; df corrections
  # use the t distribution, so on identical simulated data their p-values are
  # always >= Wald's, hence reject no more often.
  d <- mp_design(clusters = list(subject = 8), trials_per_cell = 4)
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = 0),
    residual_sd = 1
  )
  f <- y ~ condition + (1 | subject)

  ti_wald <- mp_power(
    mp_scenario_lme4(f, design = d, assumptions = a, test_method = "wald"),
    nsim = 150, seed = 99
  )$power
  ti_satt <- mp_power(
    mp_scenario_lme4(f, design = d, assumptions = a, test_method = "satterthwaite"),
    nsim = 150, seed = 99
  )$power

  expect_true(is.finite(ti_satt))
  expect_lte(ti_satt, ti_wald)
})

test_that("Kenward-Roger runs and detects a strong effect", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  skip_if_not_installed("lmerTest")
  skip_if_not_installed("pbkrtest")

  d <- mp_design(clusters = list(subject = 25), trials_per_cell = 6)
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = 0.8),
    residual_sd = 1
  )
  scn <- mp_scenario_lme4(
    y ~ condition + (1 | subject),
    design = d, assumptions = a, test_method = "kenward-roger"
  )
  res <- mp_power(scn, nsim = 20, seed = 7)
  expect_true(res$power > 0.7)
  expect_equal(res$diagnostics$fail_rate, 0)
})

test_that("Satterthwaite/KR are rejected for non-LMM fits", {
  skip_if_not_installed("lmerTest")
  fit <- stats::lm(mpg ~ wt, data = mtcars)
  expect_error(.mp_p_value(fit, "wt", "satterthwaite"), "linear mixed models")
  skip_if_not_installed("pbkrtest")
  expect_error(.mp_p_value(fit, "wt", "kenward-roger"), "linear mixed models")
})

test_that("parametric bootstrap LRT runs end-to-end and needs a null formula", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  skip_if_not_installed("pbkrtest")

  d <- mp_design(clusters = list(subject = 12), trials_per_cell = 4)
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = 0.8),
    residual_sd = 1
  )

  expect_error(
    mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a,
                     test_method = "pb"),
    "null_formula"
  )

  scn <- mp_scenario_lme4(
    y ~ condition + (1 | subject),
    design = d, assumptions = a,
    test_method = "pb",
    null_formula = y ~ 1 + (1 | subject),
    pb_nsim = 25
  )
  res <- mp_power(scn, nsim = 4, seed = 3)
  expect_true(is.numeric(res$power) && res$power >= 0 && res$power <= 1)
})

test_that("GLMM constructors reject LMM-only methods", {
  d <- mp_design(clusters = list(subject = 10), trials_per_cell = 2)
  a <- mp_assumptions(fixed_effects = list("(Intercept)" = 0, condition = 0.3))
  expect_error(
    mp_scenario_lme4_binomial(y ~ condition + (1 | subject), design = d,
                              assumptions = a, test_method = "kenward-roger"),
    "should be one of"
  )
})
