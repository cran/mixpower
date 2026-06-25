## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
set.seed(1)

## -----------------------------------------------------------------------------
library(mixpower)

## -----------------------------------------------------------------------------
d <- mp_design(clusters = list(subject = 20), trials_per_cell = 6)

a <- mp_assumptions(
  fixed_effects = list(`(Intercept)` = 0, x1 = 0.5, x2 = 0.3),
  random_effects = list(subject = list(
    intercept_sd = 0.4,
    slopes = list(x1 = 0.3, x2 = 0.3),
    cor = 0.1
  )),
  residual_sd = 1
)
a

## -----------------------------------------------------------------------------
scn_max <- mp_scenario_lme4(
  y ~ x1 + x2 + (1 + x1 + x2 | subject),
  design = d,
  assumptions = a,
  predictor = "x1"
)

mp_power(scn_max, nsim = 15, seed = 2024)$power

## -----------------------------------------------------------------------------
m <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
scn_fit <- mp_from_fit(m, test_term = "Days")
scn_fit$assumptions

## -----------------------------------------------------------------------------
mp_power(scn_fit, nsim = 15, seed = 1)$power

## -----------------------------------------------------------------------------
scn_sesoi <- mp_sesoi(scn_fit, multiplier = 0.85)
c(
  full  = scn_fit$assumptions$fixed_effects$Days,
  sesoi = scn_sesoi$assumptions$fixed_effects$Days
)

## -----------------------------------------------------------------------------
mults <- c(1, 0.5, 0.3, 0.2)
powers <- vapply(
  mults,
  function(mult) mp_power(mp_sesoi(scn_fit, multiplier = mult), nsim = 15, seed = 1)$power,
  numeric(1)
)
data.frame(multiplier = mults, effect = mults * scn_fit$assumptions$fixed_effects$Days, power = powers)

## -----------------------------------------------------------------------------
sg <- mp_safeguard_effect(m, term = "Days", conf_level = 0.90)
sg

## -----------------------------------------------------------------------------
scn_safe <- mp_sesoi(scn_fit, effect = sg)
mp_power(scn_safe, nsim = 15, seed = 1)$power

## -----------------------------------------------------------------------------
under <- mp_power(mp_sesoi(scn_fit, multiplier = 0.25), nsim = 25, seed = 7)
summary(under)
under$diagnostics$type_m

