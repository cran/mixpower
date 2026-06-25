simulate_one <- function(scn) scn$engine$simulate_fun(scn)

test_that("default binary/within predictor is unchanged", {
  d <- mp_design(list(subject = 6), trials_per_cell = 4)
  a <- mp_assumptions(list("(Intercept)" = 0, condition = 0.3),
                      random_effects = list(subject = list(intercept_sd = 0.4)),
                      residual_sd = 1)
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
  dat <- simulate_one(scn)
  # Balanced 0,1,0,1 within each subject.
  expect_equal(sort(unique(dat$condition)), c(0, 1))
  expect_equal(as.numeric(tapply(dat$condition, dat$subject, mean)), rep(0.5, 6))
})

test_that("continuous within predictor is a time-like 0..t-1 sequence", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  d <- mp_design(list(subject = 20), trials_per_cell = 6,
                 predictors = list(time = "continuous"))
  a <- mp_assumptions(list("(Intercept)" = 0, time = 0.5),
                      random_effects = list(subject = list(intercept_sd = 0.5)),
                      residual_sd = 1)
  scn <- mp_scenario_lme4(y ~ time + (1 | subject), design = d,
                          assumptions = a, predictor = "time")
  dat <- simulate_one(scn)
  expect_equal(sort(unique(dat$time)), 0:5)
  # A clear continuous slope is well powered.
  expect_gt(mp_power(scn, nsim = 40, seed = 4)$power, 0.7)
})

test_that("between predictors are constant within subject and balanced", {
  bin <- mp_design(list(subject = 20), trials_per_cell = 4,
                   predictors = list(group = list(type = "binary", level = "between")))
  a <- mp_assumptions(list("(Intercept)" = 0, group = 0.5),
                      random_effects = list(subject = list(intercept_sd = 0.5)),
                      residual_sd = 1)
  scn <- mp_scenario_lme4(y ~ group + (1 | subject), design = bin,
                          assumptions = a, predictor = "group")
  dat <- simulate_one(scn)
  per_subject_unique <- tapply(dat$group, dat$subject, function(z) length(unique(z)))
  expect_true(all(per_subject_unique == 1L))
  expect_equal(mean(tapply(dat$group, dat$subject, function(z) z[1])), 0.5)

  cont <- mp_design(list(subject = 20), trials_per_cell = 4,
                    predictors = list(x = list(type = "continuous", level = "between")))
  a2 <- mp_assumptions(list("(Intercept)" = 0, x = 0.5),
                       random_effects = list(subject = list(intercept_sd = 0.5)),
                       residual_sd = 1)
  scn2 <- mp_scenario_lme4(y ~ x + (1 | subject), design = cont,
                           assumptions = a2, predictor = "x")
  dat2 <- simulate_one(scn2)
  per <- tapply(dat2$x, dat2$subject, function(z) length(unique(z)))
  expect_true(all(per == 1L))
  expect_equal(round(mean(tapply(dat2$x, dat2$subject, function(z) z[1])), 6), 0)
})

test_that("three-level nested design simulates and controls Type I", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  d <- mp_design(list(site = 6, subject = 5), trials_per_cell = 4,
                 nesting = c(subject = "site"))
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = 0.5),
    random_effects = list(site = list(intercept_sd = 0.4),
                          subject = list(intercept_sd = 0.5)),
    residual_sd = 1
  )
  scn <- mp_scenario_lme4(y ~ condition + (1 | site) + (1 | subject),
                          design = d, assumptions = a)
  dat <- simulate_one(scn)
  expect_true(all(c("site", "subject") %in% names(dat)))
  expect_equal(length(unique(dat$site)), 6L)
  expect_equal(length(unique(dat$subject)), 30L) # 6 sites x 5 subjects
  expect_equal(nrow(dat), 6L * 5L * 4L)

  r1 <- suppressWarnings(mp_power(scn, nsim = 20, seed = 1))
  r2 <- suppressWarnings(mp_power(scn, nsim = 20, seed = 1))
  expect_equal(r1$sims$p_value, r2$sims$p_value)
})

test_that("unbalanced trials_per_cell gives unequal within-subject sizes", {
  d <- mp_design(list(subject = 10), trials_per_cell = c(3, 5))
  a <- mp_assumptions(list("(Intercept)" = 0, condition = 0.3),
                      random_effects = list(subject = list(intercept_sd = 0.4)),
                      residual_sd = 1)
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
  dat <- simulate_one(scn)
  counts <- as.numeric(table(dat$subject))
  expect_equal(sort(unique(counts)), c(3, 5))
  expect_equal(nrow(dat), 5L * (3L + 5L))
})
