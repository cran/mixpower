mk_long <- function(effect = 0.4, n = 30, t = 6) {
  d <- mp_design(list(subject = n), trials_per_cell = t,
                 predictors = list(time = "continuous"))
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, time = effect),
    random_effects = list(subject = list(intercept_sd = 0.5)),
    residual_sd = 1
  )
  mp_scenario_lme4(y ~ time + (1 | subject), design = d, assumptions = a, predictor = "time")
}

test_that("MCAR deletes about the requested fraction and is reproducible", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  scn <- mk_long()
  miss <- mp_missing(scn, "mcar", prob = 0.3)

  set.seed(10)
  d1 <- miss$engine$simulate_fun(miss)
  full_n <- 30 * 6
  expect_lt(nrow(d1), full_n)
  expect_gt(nrow(d1), full_n * 0.5) # ~70% retained

  r1 <- suppressWarnings(mp_power(miss, nsim = 12, seed = 4))
  r2 <- suppressWarnings(mp_power(miss, nsim = 12, seed = 4))
  expect_equal(r1$sims$p_value, r2$sims$p_value)
})

test_that("monotone dropout is a within-subject prefix", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  scn <- mk_long(n = 40, t = 6)
  miss <- mp_missing(scn, "dropout", time = "time",
                     dropout = c(0, 0.1, 0.2, 0.35, 0.5, 0.65))
  set.seed(2)
  dat <- miss$engine$simulate_fun(miss)

  # Counts per timepoint should be non-increasing (monotone dropout).
  per_t <- as.numeric(table(factor(dat$time, levels = 0:5)))
  expect_true(all(diff(per_t) <= 0))

  # Each subject's observed timepoints form a prefix 0,1,...,k.
  ok <- tapply(dat$time, dat$subject, function(z) {
    s <- sort(unique(z))
    identical(as.numeric(s), as.numeric(seq_along(s) - 1))
  })
  expect_true(all(ok))
})

test_that("Weibull dropout removes later observations", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  scn <- mk_long(n = 30, t = 8)
  miss <- mp_missing(scn, "dropout", time = "time",
                     dropout = list(shape = 1.5, scale = 4))
  set.seed(3)
  dat <- miss$engine$simulate_fun(miss)
  per_t <- as.numeric(table(factor(dat$time, levels = 0:7)))
  expect_true(per_t[1] >= per_t[8])
  expect_lt(nrow(dat), 30 * 8)
})

test_that("MAR deletion depends on the named covariate", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  scn <- mk_long(n = 60, t = 6)
  # Strong positive dependence on time -> later timepoints deleted far more.
  miss <- mp_missing(scn, "mar", prob = 0.2, on = "time", slope = 1.5)
  set.seed(5)
  dat <- miss$engine$simulate_fun(miss)
  retain_early <- mean(dat$time <= 1)
  retain_late <- mean(dat$time >= 4)
  expect_gt(retain_early, retain_late)
})

test_that("missingness lowers power relative to complete data", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  # A modest binary-within effect leaves headroom so deletion visibly hurts.
  d <- mp_design(list(subject = 18), trials_per_cell = 6)
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = 0.3),
    random_effects = list(subject = list(intercept_sd = 0.5)),
    residual_sd = 1
  )
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
  full <- suppressWarnings(mp_power(scn, nsim = 60, seed = 8)$power)
  miss <- suppressWarnings(mp_power(mp_missing(scn, "mcar", prob = 0.6), nsim = 60, seed = 8)$power)
  expect_gt(full, miss)
})

test_that("mp_missing validates its specification", {
  scn <- mk_long()
  expect_error(mp_missing(scn, "mcar", prob = 1.5), "\\[0, 1\\)")
  expect_error(mp_missing(scn, "mar", prob = 0.2), "requires `on`")
  expect_error(mp_missing(scn, "dropout", time = "time"), "requires `dropout`")
  expect_error(mp_missing(scn, "dropout", dropout = c(0, 0.2)), "requires `time`")
  expect_error(mp_missing(scn, "dropout", time = "time", dropout = c(0.5, 0.2)),
               "non-decreasing")
})
