test_that(".mp_extend_frame clones grouping levels with fresh ids", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  mf <- lme4::sleepstudy # 18 subjects x 10 rows
  ext <- .mp_extend_frame(mf, "Subject", 40L)
  expect_equal(length(unique(ext$Subject)), 40L)
  expect_equal(nrow(ext), 40L * 10L)
  # Within-subject covariate structure (Days) is preserved per cloned subject.
  per <- tapply(ext$Days, ext$Subject, function(z) paste(sort(z), collapse = ","))
  expect_equal(length(unique(per)), 1L)

  smaller <- .mp_extend_frame(mf, "Subject", 5L)
  expect_equal(length(unique(smaller$Subject)), 5L)
})

test_that("mp_extend validates the scenario and group names", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  m <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  scn <- mp_from_fit(m, test_term = "Days")

  expect_equal(mp_extend(scn, Subject = 40)$extend$Subject, 40L)
  expect_error(mp_extend(scn, Nope = 10), "not a grouping factor")
  expect_error(mp_extend(scn, Subject = -1), "> 0")

  # Synthetic (non-from-fit) scenarios do not support extend.
  d <- mp_design(list(subject = 10), trials_per_cell = 4)
  a <- mp_assumptions(list("(Intercept)" = 0, condition = 0.3),
                      random_effects = list(subject = list(intercept_sd = 0.4)),
                      residual_sd = 1)
  syn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
  expect_error(mp_extend(syn, subject = 20), "mp_from_fit")
})

test_that("extending a pilot scales N and is reproducible", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  m <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  scn <- mp_from_fit(m, test_term = "Days")

  big <- mp_extend(scn, Subject = 36)
  dat <- big$engine$simulate_fun(big)
  expect_equal(length(unique(dat$Subject)), 36L)

  r1 <- suppressWarnings(mp_power(big, nsim = 12, seed = 5))
  r2 <- suppressWarnings(mp_power(big, nsim = 12, seed = 5))
  expect_equal(r1$sims$p_value, r2$sims$p_value)
})

test_that("power increases with extended sample size from a pilot", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  m <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  scn <- mp_from_fit(m, test_term = "Days")
  # Shrink the (large) Days effect so power is in the informative range.
  scn <- mp_sesoi(scn, effect = 1.5)

  p_small <- suppressWarnings(mp_power(mp_extend(scn, Subject = 8), nsim = 40, seed = 2)$power)
  p_large <- suppressWarnings(mp_power(mp_extend(scn, Subject = 48), nsim = 40, seed = 2)$power)
  expect_gt(p_large, p_small)
})

test_that("extend.<group> drives a power curve over N", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  m <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  scn <- mp_sesoi(mp_from_fit(m, test_term = "Days"), effect = 1.5)

  curve <- suppressWarnings(mp_power_curve(
    scn, vary = list(`extend.Subject` = c(8, 24, 48)), nsim = 30, seed = 9
  ))
  res <- curve$results[order(curve$results[["extend.Subject"]]), ]
  expect_equal(nrow(res), 3L)
  expect_gte(res$estimate[3], res$estimate[1])
})
