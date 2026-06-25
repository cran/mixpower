make_scn <- function(effect = 0.8) {
  d <- mp_design(clusters = list(subject = 30), trials_per_cell = 8)
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = effect),
    random_effects = list(subject = list(intercept_sd = 0.5)),
    residual_sd = 1
  )
  mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
}

test_that("mp_sesoi scales or sets the focal fixed effect", {
  scn <- make_scn(0.8)

  s1 <- mp_sesoi(scn, multiplier = 0.5)
  expect_equal(s1$assumptions$fixed_effects$condition, 0.4)

  s2 <- mp_sesoi(scn, effect = 0.2)
  expect_equal(s2$assumptions$fixed_effects$condition, 0.2)

  # Default multiplier is a 15% reduction.
  s3 <- mp_sesoi(scn)
  expect_equal(s3$assumptions$fixed_effects$condition, 0.8 * 0.85)

  # Original scenario is untouched.
  expect_equal(scn$assumptions$fixed_effects$condition, 0.8)
})

test_that("mp_sesoi resolves the term and validates it", {
  d <- mp_design(clusters = list(subject = 10), trials_per_cell = 8)
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, x1 = 0.4, x2 = 0.3),
    random_effects = list(subject = list(intercept_sd = 0.5)),
    residual_sd = 1
  )
  scn <- mp_scenario_lme4(y ~ x1 + x2 + (1 | subject), design = d,
                          assumptions = a, predictor = "x1")

  # Default term = scenario test term (x1).
  expect_equal(mp_sesoi(scn, multiplier = 0.5)$assumptions$fixed_effects$x1, 0.2)
  # Explicit term override.
  expect_equal(mp_sesoi(scn, multiplier = 0.5, term = "x2")$assumptions$fixed_effects$x2, 0.15)

  expect_error(mp_sesoi(scn, term = "nope"), "not present")
})

test_that("mp_sesoi lowers power relative to the full effect", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  scn <- make_scn(0.9)
  full <- mp_power(scn, nsim = 60, seed = 11)$power
  small <- mp_power(mp_sesoi(scn, multiplier = 0.3), nsim = 60, seed = 11)$power
  expect_gt(full, small)
})

test_that("mp_safeguard_effect returns the CI bound nearest zero", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  m <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  sg <- mp_safeguard_effect(m, term = "Days", conf_level = 0.90)

  expect_s3_class(sg, "mp_safeguard")
  expect_equal(sg$term, "Days")
  # Days effect is strongly positive -> safeguard is the (smaller) lower bound.
  expect_lt(sg$safeguard, sg$estimate)
  expect_equal(sg$safeguard, sg$lower)

  z <- stats::qnorm(0.95)
  expect_equal(sg$safeguard, sg$estimate - z * sg$se, tolerance = 1e-6)
  expect_false(sg$crosses_zero)

  expect_output(print(sg), "safeguard")
})

test_that("mp_safeguard_effect feeds mp_sesoi", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  m <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  sg <- mp_safeguard_effect(m, term = "Days", conf_level = 0.90)

  scn <- mp_from_fit(m, test_term = "Days")
  scn_sg <- mp_sesoi(scn, effect = sg)
  expect_equal(scn_sg$assumptions$fixed_effects$Days, sg$safeguard)
})

test_that("mp_safeguard_effect validates its inputs", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  m <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  expect_error(mp_safeguard_effect(m, conf_level = 1.5), "in \\(0, 1\\)")
  expect_error(mp_safeguard_effect(m, term = "nope"), "not a fixed effect")
  expect_error(mp_safeguard_effect(list()), "lme4 fits")
})
