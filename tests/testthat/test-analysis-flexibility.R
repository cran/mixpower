test_that("omnibus joint-Wald test of multiple terms works", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  d <- mp_design(list(subject = 30), trials_per_cell = 8)
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, x1 = 0.6, x2 = 0.5),
    random_effects = list(subject = list(intercept_sd = 0.5)),
    residual_sd = 1
  )
  scn <- mp_scenario_lme4(y ~ x1 + x2 + (1 | subject), design = d,
                          assumptions = a, predictor = "x1",
                          test_term = c("x1", "x2"))
  # The joint test of two real effects is well powered.
  expect_gt(mp_power(scn, nsim = 40, seed = 1)$power, 0.7)

  # Under the joint null, the omnibus test is calibrated (not anti-conservative).
  a0 <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, x1 = 0, x2 = 0),
    random_effects = list(subject = list(intercept_sd = 0.5)),
    residual_sd = 1
  )
  scn0 <- mp_scenario_lme4(y ~ x1 + x2 + (1 | subject), design = d,
                           assumptions = a0, predictor = "x1",
                           test_term = c("x1", "x2"))
  type1 <- mp_power(scn0, nsim = 120, seed = 2)$power
  expect_lt(type1, 0.15)
})

test_that("a linear contrast reduces to the single-coefficient Wald test", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  d <- mp_design(list(subject = 30), trials_per_cell = 6)
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = 0.5),
    random_effects = list(subject = list(intercept_sd = 0.5)),
    residual_sd = 1
  )
  base <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
  contr <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a,
                            contrast = c(condition = 1))

  dat <- base$engine$simulate_fun(base)
  fit <- base$engine$fit_fun(dat, base)
  p_wald <- base$engine$test_fun(fit, base)$p_value
  p_contr <- contr$engine$test_fun(fit, contr)$p_value
  expect_equal(p_contr, p_wald, tolerance = 1e-8)
})

test_that("mp_compare_models exposes Type I inflation from a misspecified model", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  d <- mp_design(list(subject = 30), trials_per_cell = 8)
  # Null fixed effect, but a real by-subject random slope on condition.
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = 0),
    random_effects = list(subject = list(intercept_sd = 0.5,
                                          slopes = list(condition = 0.8))),
    residual_sd = 1
  )
  maximal <- mp_scenario_lme4(y ~ condition + (1 + condition | subject), d, a)
  reduced <- mp_scenario_lme4(y ~ condition + (1 | subject), d, a)

  cmp <- suppressWarnings(mp_compare_models(
    list(maximal = maximal, reduced = reduced), nsim = 80, seed = 3
  ))
  expect_s3_class(cmp, "mp_model_comparison")
  res <- cmp$results
  rownames(res) <- res$model
  # Reduced model ignores the slope -> inflated Type I vs the maximal model.
  expect_gt(res["reduced", "power"], res["maximal", "power"])
  expect_lt(res["maximal", "power"], 0.15)
  expect_output(print(cmp), "model_comparison")
})

test_that("mp_compare_models validates inputs", {
  d <- mp_design(list(subject = 10), trials_per_cell = 4)
  a <- mp_assumptions(list("(Intercept)" = 0, condition = 0.3),
                      random_effects = list(subject = list(intercept_sd = 0.4)),
                      residual_sd = 1)
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
  expect_error(mp_compare_models(list(scn), nsim = 5), "named list")
  expect_error(mp_compare_models(scn, nsim = 5), "named list")
})
