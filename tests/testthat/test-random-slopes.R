test_that("random_effects accepts slopes and cor; validation guards them", {
  ok <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = 0.3),
    random_effects = list(subject = list(
      intercept_sd = 0.5, slopes = list(condition = 0.3), cor = 0.2
    )),
    residual_sd = 1
  )
  expect_equal(.mp_re_slope_sd(ok, "subject", "condition"), 0.3)
  expect_equal(.mp_re_cor(ok, "subject"), 0.2)

  expect_error(
    mp_assumptions(list(condition = 0.3),
                   random_effects = list(subject = list(intercept_sd = 0.5, cor = 2))),
    "\\[-1, 1\\]"
  )
  expect_error(
    mp_assumptions(list(condition = 0.3),
                   random_effects = list(subject = list(
                     intercept_sd = 0.5, slopes = list(condition = -1)
                   ))),
    "non-negative"
  )
})

test_that("random_effects accepts multiple correlated slopes (scalar or matrix cor)", {
  multi <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, x1 = 0.3, x2 = 0.2),
    random_effects = list(subject = list(
      intercept_sd = 0.5,
      slopes = list(x1 = 0.3, x2 = 0.25),
      cor = 0.1
    )),
    residual_sd = 1
  )
  expect_equal(.mp_re_slope_sd(multi, "subject", "x1"), 0.3)
  expect_equal(.mp_re_slope_sd(multi, "subject", "x2"), 0.25)

  R <- matrix(c(1, 0.2, -0.1, 0.2, 1, 0.0, -0.1, 0.0, 1), nrow = 3)
  ok_mat <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, x1 = 0.3, x2 = 0.2),
    random_effects = list(subject = list(
      intercept_sd = 0.5, slopes = list(x1 = 0.3, x2 = 0.25), cor = R
    )),
    residual_sd = 1
  )
  expect_true(is.matrix(ok_mat$random_effects$subject$cor))

  # Wrong-dimension correlation matrix is rejected.
  expect_error(
    mp_assumptions(
      fixed_effects = list("(Intercept)" = 0, x1 = 0.3, x2 = 0.2),
      random_effects = list(subject = list(
        intercept_sd = 0.5, slopes = list(x1 = 0.3, x2 = 0.25),
        cor = matrix(c(1, 0.2, 0.2, 1), nrow = 2)
      ))
    ),
    "must be 3 x 3"
  )
  # Non-positive-definite scalar correlation across 3 terms is rejected.
  expect_error(
    mp_assumptions(
      fixed_effects = list("(Intercept)" = 0, x1 = 0.3, x2 = 0.2),
      random_effects = list(subject = list(
        intercept_sd = 0.5, slopes = list(x1 = 0.3, x2 = 0.25), cor = -0.9
      ))
    ),
    "positive-definite"
  )
})

test_that(".mp_draw_re_block reproduces the requested covariance structure", {
  set.seed(99)
  sds <- c("(Intercept)" = 1.0, x1 = 0.8, x2 = 0.5)
  R <- matrix(c(1.0, 0.4, -0.2,
                0.4, 1.0, 0.1,
                -0.2, 0.1, 1.0), nrow = 3, byrow = TRUE)
  dimnames(R) <- list(names(sds), names(sds))

  B <- .mp_draw_re_block(50000L, sds, R)
  expect_equal(colnames(B), names(sds))
  expect_equal(unname(apply(B, 2, stats::sd)), unname(sds), tolerance = 0.05)

  emp <- stats::cor(B)
  expect_equal(emp[1, 2], R[1, 2], tolerance = 0.05)
  expect_equal(emp[1, 3], R[1, 3], tolerance = 0.05)
  expect_equal(emp[2, 3], R[2, 3], tolerance = 0.05)

  # Inactive terms (zero SD) are dropped from the block.
  z <- .mp_draw_re_block(10L, c("(Intercept)" = 0, x1 = 0.5), diag(2))
  expect_equal(colnames(z), "x1")
  expect_null(.mp_draw_re_block(10L, c("(Intercept)" = 0), matrix(1)))
})

test_that("multiple fixed effects generate balanced, orthogonal predictors", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  d <- mp_design(clusters = list(subject = 10), trials_per_cell = 8)
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, x1 = 0.4, x2 = 0.3),
    random_effects = list(subject = list(intercept_sd = 0.5)),
    residual_sd = 1
  )
  scn <- mp_scenario_lme4(
    y ~ x1 + x2 + (1 | subject), design = d, assumptions = a, predictor = "x1"
  )
  dat <- scn$engine$simulate_fun(scn)
  expect_true(all(c("x1", "x2") %in% names(dat)))
  expect_equal(mean(dat$x1), 0.5)
  expect_equal(mean(dat$x2), 0.5)
  # A 2^2 factorial (trials_per_cell a multiple of 4) is orthogonal.
  expect_equal(unname(stats::cor(dat$x1, dat$x2)), 0)
})

test_that("simulation with random slopes is reproducible", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  d <- mp_design(clusters = list(subject = 25), trials_per_cell = 6)
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = 0.3),
    random_effects = list(subject = list(
      intercept_sd = 0.5, slopes = list(condition = 0.4), cor = 0.1
    )),
    residual_sd = 1
  )
  scn <- mp_scenario_lme4(y ~ condition + (1 + condition | subject), design = d, assumptions = a)
  r1 <- mp_power(scn, nsim = 12, seed = 321)
  r2 <- mp_power(scn, nsim = 12, seed = 321)
  expect_equal(r1$sims$p_value, r2$sims$p_value)
})

test_that("two correlated random slopes run end-to-end and detect the focal effect", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  d <- mp_design(clusters = list(subject = 24), trials_per_cell = 8)
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, x1 = 0.9, x2 = 0.3),
    random_effects = list(subject = list(
      intercept_sd = 0.4,
      slopes = list(x1 = 0.3, x2 = 0.3),
      cor = 0.0
    )),
    residual_sd = 1
  )
  scn <- mp_scenario_lme4(
    y ~ x1 + x2 + (1 + x1 + x2 | subject),
    design = d, assumptions = a, predictor = "x1"
  )
  r1 <- mp_power(scn, nsim = 30, seed = 7)
  r2 <- mp_power(scn, nsim = 30, seed = 7)
  expect_equal(r1$sims$p_value, r2$sims$p_value)
  expect_gt(r1$power, 0.6)
})

test_that("omitting a present random slope inflates Type I error", {
  skip_if_not_installed("lme4")
  skip_on_cran()

  # Null fixed effect, but a large between-subject random slope on condition.
  d <- mp_design(clusters = list(subject = 30), trials_per_cell = 6)
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = 0),
    random_effects = list(subject = list(
      intercept_sd = 0.5, slopes = list(condition = 0.8)
    )),
    residual_sd = 1
  )

  scn_max <- mp_scenario_lme4(
    y ~ condition + (1 + condition | subject), design = d, assumptions = a
  )
  scn_int <- mp_scenario_lme4(
    y ~ condition + (1 | subject), design = d, assumptions = a
  )

  ti_max <- mp_power(scn_max, nsim = 80, seed = 2024)$power
  ti_int <- mp_power(scn_int, nsim = 80, seed = 2024)$power

  # The intercept-only model ignores slope variability -> anti-conservative.
  expect_gt(ti_int, ti_max)
  # The maximal model keeps Type I error near the nominal level.
  expect_lt(ti_max, 0.15)
})
