## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(mixpower)

## -----------------------------------------------------------------------------
design <- mp_design(
  clusters = list(subject = 40),
  trials_per_cell = 5,
  notes = "Baseline study layout"
)

assumptions <- mp_assumptions(
  fixed_effects = list(`(Intercept)` = 0, condition = 0.3),
  residual_sd = 1,
  icc = list(subject = 0.1)
)

design
assumptions

