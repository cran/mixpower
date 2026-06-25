## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
set.seed(1)

## -----------------------------------------------------------------------------
library(mixpower)

## -----------------------------------------------------------------------------
d <- mp_design(clusters = list(subject = 24), trials_per_cell = 6)
a <- mp_assumptions(
  fixed_effects = list(`(Intercept)` = 0, condition = 0.4),
  random_effects = list(subject = list(intercept_sd = 0.5)),
  residual_sd = 1
)
scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)

mp_calibrate(scn, nsim = 60, seed = 11)

## -----------------------------------------------------------------------------
a_slope <- mp_assumptions(
  fixed_effects = list(`(Intercept)` = 0, condition = 0.4),
  random_effects = list(subject = list(
    intercept_sd = 0.5, slopes = list(condition = 0.8)
  )),
  residual_sd = 1
)
# Data have the slope; the fitted model (1 | subject) ignores it.
scn_mis <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a_slope)

mp_calibrate(scn_mis, nsim = 60, seed = 7)

## -----------------------------------------------------------------------------
scn_few <- mp_scenario_lme4(
  y ~ condition + (1 | subject),
  design = mp_design(list(subject = 12), trials_per_cell = 8),
  assumptions = a
)
mp_recommend_method(scn_few)

## ----eval = requireNamespace("pbkrtest", quietly = TRUE) && requireNamespace("lmerTest", quietly = TRUE)----
scn_kr <- mp_scenario_lme4(
  y ~ condition + (1 | subject),
  design = mp_design(list(subject = 12), trials_per_cell = 8),
  assumptions = a, test_method = "kenward-roger"
)
mp_calibrate(scn_kr, nsim = 40, seed = 3)$type1

## -----------------------------------------------------------------------------
mp_report_table(mp_calibrate(scn, nsim = 40, seed = 1))

