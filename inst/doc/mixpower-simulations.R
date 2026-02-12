## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(mixpower)

## -----------------------------------------------------------------------------
d <- mp_design(clusters = list(subject = 20), trials_per_cell = 4)
a <- mp_assumptions(
  fixed_effects = list(`(Intercept)` = 0, condition = 0.4),
  residual_sd = 1,
  icc = list(subject = 0.1)
)

scn <- mp_scenario_lme4(
  y ~ condition + (1 | subject),
  design = d,
  assumptions = a,
  test_method = "wald"
)

res <- mp_power(scn, nsim = 10, seed = 42)
summary(res)

