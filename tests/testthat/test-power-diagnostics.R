test_that("Clopper-Pearson CI matches binom.test and is well-behaved at 0/1", {
  expect_equal(
    .mp_power_ci(5, 20, 0.95, "clopper-pearson"),
    as.numeric(stats::binom.test(5, 20, conf.level = 0.95)$conf.int),
    tolerance = 1e-8
  )
  lo0 <- .mp_power_ci(0, 20, 0.95, "clopper-pearson")
  expect_equal(lo0[1], 0)
  expect_gt(lo0[2], 0) # Wald would give [0, 0]
  hi1 <- .mp_power_ci(20, 20, 0.95, "clopper-pearson")
  expect_equal(hi1[2], 1)
  expect_lt(hi1[1], 1)
  expect_equal(.mp_power_ci(NA, 0, 0.95), c(NA_real_, NA_real_))
})

test_that("mp_power uses Clopper-Pearson by default and brackets the estimate", {
  d <- mp_design(clusters = list(subject = 30), trials_per_cell = 4)
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = 0.4),
    residual_sd = 1
  )
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
  res <- mp_power(scn, nsim = 30, seed = 5)
  expect_identical(res$ci_method, "clopper-pearson")
  expect_lte(res$ci[1], res$power)
  expect_gte(res$ci[2], res$power)
  expect_gte(res$ci[1], 0)
  expect_lte(res$ci[2], 1)
})

test_that("Type M shows exaggeration at low power and ~1 at high power", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  f <- y ~ condition + (1 | subject)

  # Low power: significant estimates are inflated (Gelman & Carlin 2014).
  d_lo <- mp_design(clusters = list(subject = 15), trials_per_cell = 2)
  a_lo <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = 0.12),
    residual_sd = 1
  )
  res_lo <- mp_power(mp_scenario_lme4(f, design = d_lo, assumptions = a_lo),
                     nsim = 300, seed = 11)
  expect_true(is.finite(res_lo$diagnostics$type_m))
  expect_gt(res_lo$diagnostics$type_m, 1.1)

  # High power: estimates are essentially unbiased; no sign errors.
  d_hi <- mp_design(clusters = list(subject = 40), trials_per_cell = 8)
  a_hi <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = 0.8),
    residual_sd = 1
  )
  res_hi <- mp_power(mp_scenario_lme4(f, design = d_hi, assumptions = a_hi),
                     nsim = 40, seed = 12)
  expect_lt(res_hi$diagnostics$type_m, 1.3)
  expect_gt(res_hi$diagnostics$type_m, 0.8)
  expect_equal(res_hi$diagnostics$type_s, 0)
})

test_that("Type S/M are NA when the true effect is zero or unknown", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  d <- mp_design(clusters = list(subject = 20), trials_per_cell = 4)
  a_null <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = 0),
    residual_sd = 1
  )
  res <- mp_power(mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a_null),
                  nsim = 20, seed = 1)
  expect_true(is.na(res$diagnostics$type_s))
  expect_true(is.na(res$diagnostics$type_m))
})

test_that("streaming and full agree on Type S/M and CI", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  d <- mp_design(clusters = list(subject = 20), trials_per_cell = 4)
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = 0.3),
    residual_sd = 1
  )
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
  full <- mp_power(scn, nsim = 40, seed = 42, aggregate = "full")
  stream <- mp_power(scn, nsim = 40, seed = 42, aggregate = "streaming", keep = "minimal")
  expect_equal(full$ci, stream$ci, tolerance = 1e-10)
  expect_equal(full$diagnostics$type_m, stream$diagnostics$type_m, tolerance = 1e-10)
  expect_equal(full$diagnostics$type_s, stream$diagnostics$type_s, tolerance = 1e-10)
})
