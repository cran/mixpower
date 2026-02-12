## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(mixpower)

## -----------------------------------------------------------------------------
d <- mp_design(clusters = list(subject = 40), trials_per_cell = 8)
a <- mp_assumptions(
  fixed_effects = list(`(Intercept)` = 0, condition = 0.4),
  residual_sd = 1,
  icc = list(subject = 0.1)
)

scn_wald <- mp_scenario_lme4(
  y ~ condition + (1 | subject),
  design = d,
  assumptions = a,
  test_method = "wald"
)

scn_lrt <- mp_scenario_lme4(
  y ~ condition + (1 | subject),
  design = d,
  assumptions = a,
  test_method = "lrt",
  null_formula = y ~ 1 + (1 | subject)
)

vary_spec <- list(`clusters.subject` = c(30, 50, 80))

sens_wald <- mp_sensitivity(
  scn_wald,
  vary = vary_spec,
  nsim = 50,
  seed = 123
)

sens_lrt <- mp_sensitivity(
  scn_lrt,
  vary = vary_spec,
  nsim = 50,
  seed = 123
)

comparison <- rbind(
  transform(sens_wald$results, method = "wald"),
  transform(sens_lrt$results, method = "lrt")
)

comparison[, c(
  "method", "clusters.subject", "estimate", "mcse",
  "conf_low", "conf_high", "failure_rate", "singular_rate"
)]

wald_dat <- comparison[comparison$method == "wald", ]
lrt_dat  <- comparison[comparison$method == "lrt", ]

plot(
  wald_dat$`clusters.subject`, wald_dat$estimate,
  type = "b", pch = 16, lty = 1,
  ylim = c(0, 1),
  xlab = "clusters.subject",
  ylab = "Power estimate",
  col = "steelblue"
)
lines(
  lrt_dat$`clusters.subject`, lrt_dat$estimate,
  type = "b", pch = 17, lty = 2,
  col = "firebrick"
)
legend(
  "bottomright",
  legend = c("Wald", "LRT"),
  col = c("steelblue", "firebrick"),
  lty = c(1, 2), pch = c(16, 17), bty = "n"
)

## ----binomial-scenario--------------------------------------------------------
d <- mp_design(clusters = list(subject = 40), trials_per_cell = 8)
a <- mp_assumptions(
  fixed_effects = list(`(Intercept)` = 0, condition = 0.5),
  residual_sd = 1,
  icc = list(subject = 0.4)
)

scn_bin <- mp_scenario_lme4_binomial(
  y ~ condition + (1 | subject),
  design = d,
  assumptions = a,
  test_method = "wald"
)

res_bin <- mp_power(scn_bin, nsim = 50, seed = 123)
summary(res_bin)

## ----binomial-sensitivity-----------------------------------------------------
sens_bin <- mp_sensitivity(
  scn_bin,
  vary = list(`fixed_effects.condition` = c(0.2, 0.4, 0.6)),
  nsim = 50,
  seed = 123
)

plot(sens_bin)
sens_bin$results

# Inspect failure_rate and singular_rate alongside power.
# Increase nsim for final study reporting.

## ----poisson-scenario---------------------------------------------------------
d <- mp_design(clusters = list(subject = 40), trials_per_cell = 8)
a <- mp_assumptions(
  fixed_effects = list(`(Intercept)` = 0, condition = 0.4),
  residual_sd = 1,
  icc = list(subject = 0.3)
)

scn_pois <- mp_scenario_lme4_poisson(
  y ~ condition + (1 | subject),
  design = d,
  assumptions = a,
  test_method = "wald"
)

res_pois <- mp_power(scn_pois, nsim = 50, seed = 123)
summary(res_pois)

a_nb <- a
a_nb$theta <- 1.5  # NB dispersion (size parameter)

scn_nb <- mp_scenario_lme4_nb(
  y ~ condition + (1 | subject),
  design = d,
  assumptions = a_nb,
  test_method = "wald"
)

res_nb <- mp_power(scn_nb, nsim = 50, seed = 123)
summary(res_nb)

## ----count-sensitivity--------------------------------------------------------
sens_pois <- mp_sensitivity(
  scn_pois,
  vary = list(`fixed_effects.condition` = c(0.2, 0.4, 0.6)),
  nsim = 50,
  seed = 123
)

sens_nb <- mp_sensitivity(
  scn_nb,
  vary = list(`fixed_effects.condition` = c(0.2, 0.4, 0.6)),
  nsim = 50,
  seed = 123
)

plot(sens_pois)
plot(sens_nb)

# Compare power estimates and failure/singularity rates across Poisson vs NB.
# Overdispersion can reduce power relative to Poisson.
# Increase nsim for final reports.

