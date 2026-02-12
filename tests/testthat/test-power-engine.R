testthat::test_that("mp_power runs with a toy engine and is reproducible", {
  d <- mp_design(list(subject = 50), trials_per_cell = 1)
  a <- mp_assumptions(list(condition = 0.4), residual_sd = 1)

  sim_fun <- function(scn, seed) {
    n <- scn$design$clusters$subject
    x <- rbinom(n, 1, 0.5)
    y <- scn$assumptions$fixed_effects$condition * x + rnorm(n, sd = scn$assumptions$residual_sd)
    data.frame(y = y, condition = x)
  }
  fit_fun <- function(dat, scn) stats::lm(scn$formula, data = dat)
  test_fun <- function(fit, scn) {
    sm <- summary(fit)
    p <- sm$coefficients["condition", "Pr(>|t|)"]
    list(p_value = as.numeric(p))
  }

  s <- mp_scenario(y ~ condition, d, a, test = "custom",
                   simulate_fun = sim_fun, fit_fun = fit_fun, test_fun = test_fun)

  r1 <- mp_power(s, nsim = 50, seed = 123)
  r2 <- mp_power(s, nsim = 50, seed = 123)

  expect_true(inherits(r1, "mp_power"))
  expect_equal(r1$power, r2$power)
  expect_equal(r1$diagnostics$fail_rate, 0)
})

testthat::test_that("mp_power errors when engine incomplete", {
  d <- mp_design(list(subject = 10), 1)
  a <- mp_assumptions(list(condition = 0.2), residual_sd = 1)
  s <- mp_scenario(y ~ condition, d, a)

  expect_error(mp_power(s, nsim = 10), "engine is incomplete")
})
