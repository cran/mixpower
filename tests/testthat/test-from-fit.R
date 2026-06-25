test_that("mp_from_fit builds a scenario reflecting the fitted model", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  m <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  scn <- mp_from_fit(m, test_term = "Days")

  expect_s3_class(scn, "mp_scenario")
  expect_equal(scn$test$term, "Days")
  expect_equal(scn$assumptions$fixed_effects$Days,
               unname(lme4::fixef(m)["Days"]), tolerance = 1e-6)
  expect_true(!is.null(scn$assumptions$random_effects$Subject$intercept_sd))
  expect_true(!is.null(scn$assumptions$random_effects$Subject$slopes$Days))
  expect_true(is.numeric(scn$assumptions$residual_sd))
})

test_that("mp_from_fit data-based power is high for a strong pilot effect", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  m <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  scn <- mp_from_fit(m, test_term = "Days")
  res <- mp_power(scn, nsim = 20, seed = 1)
  expect_gt(res$power, 0.9)
})

test_that("mp_from_fit supports effect-size sensitivity (data-based vs SESOI)", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  m <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  scn <- mp_from_fit(m, test_term = "Days")
  out <- mp_sensitivity(scn, vary = list(`fixed_effects.Days` = c(1, 10)),
                        nsim = 25, seed = 2)
  expect_equal(out$results$`fixed_effects.Days`, c(1, 10))
  expect_lt(out$results$estimate[1], out$results$estimate[2])
})

test_that("mp_from_fit default test_term is the first non-intercept effect", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  m <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  expect_equal(mp_from_fit(m)$test$term, "Days")
})

test_that("mp_from_fit rejects unsupported objects and bad terms", {
  expect_error(mp_from_fit(stats::lm(mpg ~ wt, data = mtcars)), "lme4")
  skip_if_not_installed("lme4")
  skip_on_cran()
  m <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  expect_error(mp_from_fit(m, test_term = "nope"), "not a fixed effect")
})

test_that("mp_from_fit works for a glmer (binomial) fit and gates LMM-only methods", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  set.seed(1)
  ns <- 20L
  nt <- 8L
  n <- ns * nt
  subj <- rep(seq_len(ns), each = nt)
  x <- rep(c(0, 1), length.out = n)
  b0 <- stats::rnorm(ns, 0, 0.4)
  y <- stats::rbinom(n, 1, stats::plogis(1.2 * x + b0[subj]))
  dd <- data.frame(y = y, condition = x, subject = factor(subj))
  m <- lme4::glmer(y ~ condition + (1 | subject), data = dd, family = stats::binomial())

  scn <- mp_from_fit(m, test_term = "condition")
  res <- mp_power(scn, nsim = 15, seed = 3)
  expect_true(is.numeric(res$power) && res$power >= 0 && res$power <= 1)

  # Satterthwaite is linear-mixed-model-only and not offered for glmer fits.
  expect_error(mp_from_fit(m, test_method = "satterthwaite"), "should be one of")
})
