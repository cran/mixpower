## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----backend-toy--------------------------------------------------------------
library(mixpower)

sim_fun <- function(scenario, seed = NULL) {
  n <- scenario$design$clusters$subject
  x <- stats::rbinom(n, 1, 0.5)
  y <- scenario$assumptions$fixed_effects$condition * x +
    stats::rnorm(n, sd = scenario$assumptions$residual_sd)
  data.frame(y = y, condition = x)
}
fit_fun <- function(data, scenario) stats::lm(scenario$formula, data = data)
test_fun <- function(fit, scenario) {
  sm <- summary(fit)
  p <- sm$coefficients["condition", "Pr(>|t|)"]
  list(p_value = as.numeric(p))
}

eng <- mp_backend(sim_fun, fit_fun, test_fun, name = "toy_lm")
eng

## ----run----------------------------------------------------------------------
d <- mp_design(list(subject = 25), trials_per_cell = 1)
a <- mp_assumptions(list(`(Intercept)` = 0, condition = 0.2), residual_sd = 1)
scn <- mp_scenario(
  y ~ condition, d, a,
  test = "custom",
  simulate_fun = eng$simulate_fun,
  fit_fun = eng$fit_fun,
  test_fun = eng$test_fun
)
mp_power(scn, nsim = 12, seed = 1)

