# mixpower

mixpower provides simulation-based power analysis for Gaussian linear mixed-effects models.

## CI expectations

- `R-CMD-check`: full multi-OS package checks (release, devel, oldrel-1).
- `tests`: quick `devtools::test()` on PRs.
- `coverage`: runs tests then uploads coverage.
- `pkgdown`: builds and deploys docs from `main`.

## mixpower 0.1.0

- Initial release.
- Supports simulation-based power analysis for Gaussian linear mixed-effects models.
- Includes diagnostics for convergence and simulation uncertainty.

## Sensitivity analysis

```r
d <- mp_design(clusters = list(subject = 30), trials_per_cell = 4)
a <- mp_assumptions(
  fixed_effects = list(`(Intercept)` = 0, condition = 0.3),
  residual_sd = 1
)
scn <- mp_scenario_lme4(
  y ~ condition + (1 | subject),
  design = d,
  assumptions = a
)

sens <- mp_sensitivity(
  scn,
  vary = list(`fixed_effects.condition` = c(0.2, 0.4, 0.6)),
  nsim = 50,
  seed = 123
)
plot(sens)
```

## Power curve and sample-size solver

```r
d <- mp_design(clusters = list(subject = 30), trials_per_cell = 4)
a <- mp_assumptions(
  fixed_effects = list(`(Intercept)` = 0, condition = 0.3),
  residual_sd = 1
)
scn <- mp_scenario_lme4(
  y ~ condition + (1 | subject),
  design = d,
  assumptions = a
)

curve <- mp_power_curve(
  scn,
  vary = list(`clusters.subject` = c(20, 30, 40, 50)),
  nsim = 50,
  seed = 123
)
plot(curve)

solve <- mp_solve_sample_size(
  scn,
  parameter = "clusters.subject",
  grid = c(20, 30, 40, 50),
  target_power = 0.8,
  nsim = 50,
  seed = 123
)
solve$solution
```

## Quick Wald vs LRT compare

```r
d <- mp_design(clusters = list(subject = 40), trials_per_cell = 8)
a <- mp_assumptions(
  fixed_effects = list(`(Intercept)` = 0, condition = 0.4),
  residual_sd = 1,
  icc = list(subject = 0.1)
)

scn_wald <- mp_scenario_lme4(
  y ~ condition + (1 | subject),
  design = d, assumptions = a,
  test_method = "wald"
)

scn_lrt <- mp_scenario_lme4(
  y ~ condition + (1 | subject),
  design = d, assumptions = a,
  test_method = "lrt",
  null_formula = y ~ 1 + (1 | subject)
)

vary_spec <- list(`clusters.subject` = c(30, 50, 80))
sens_wald <- mp_sensitivity(scn_wald, vary = vary_spec, nsim = 50, seed = 123)
sens_lrt  <- mp_sensitivity(scn_lrt,  vary = vary_spec, nsim = 50, seed = 123)
```

## Wald vs LRT sensitivity comparison

```r
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
  predictor = "condition",
  subject = "subject",
  outcome = "y",
  test_method = "wald"
)

scn_lrt <- mp_scenario_lme4(
  y ~ condition + (1 | subject),
  design = d,
  assumptions = a,
  predictor = "condition",
  subject = "subject",
  outcome = "y",
  test_method = "lrt",
  null_formula = y ~ 1 + (1 | subject)
)

vary_spec <- list(`clusters.subject` = c(30, 50, 80))

sens_wald <- mp_sensitivity(scn_wald, vary = vary_spec, nsim = 50, seed = 123)
sens_lrt  <- mp_sensitivity(scn_lrt,  vary = vary_spec, nsim = 50, seed = 123)

comp <- rbind(
  transform(sens_wald$results, method = "wald"),
  transform(sens_lrt$results,  method = "lrt")
)

comp

wald_dat <- comp[comp$method == "wald", ]
lrt_dat  <- comp[comp$method == "lrt", ]

x <- "clusters.subject"

plot(
  wald_dat[[x]], wald_dat$estimate,
  type = "b", pch = 16, lty = 1,
  ylim = c(0, 1),
  xlab = x, ylab = "Power estimate",
  col = "steelblue"
)
lines(
  lrt_dat[[x]], lrt_dat$estimate,
  type = "b", pch = 17, lty = 2,
  col = "firebrick"
)
legend(
  "bottomright",
  legend = c("Wald", "LRT"),
  col = c("steelblue", "firebrick"),
  lty = c(1, 2), pch = c(16, 17), bty = "n"
)

diag_comp <- comp[, c(
  "method",
  "clusters.subject",
  "estimate", "mcse", "conf_low", "conf_high",
  "failure_rate", "singular_rate", "n_effective", "nsim"
)]

diag_comp[order(diag_comp$method, diag_comp$`clusters.subject`), ]
```

## Binomial GLMM power (binary outcome)

```r
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
```

## Count outcomes (Poisson vs Negative Binomial)

```r
d <- mp_design(clusters = list(subject = 40), trials_per_cell = 8)
a <- mp_assumptions(
  fixed_effects = list(`(Intercept)` = 0, condition = 0.4),
  residual_sd = 1,
  icc = list(subject = 0.3)
)

# Count outcome (Poisson GLMM)
scn_pois <- mp_scenario_lme4_poisson(
  y ~ condition + (1 | subject),
  design = d,
  assumptions = a,
  test_method = "wald"
)

# Over-dispersed count outcome (Negative Binomial)
a_nb <- a
a_nb$theta <- 1.5
scn_nb <- mp_scenario_lme4_nb(
  y ~ condition + (1 | subject),
  design = d,
  assumptions = a_nb,
  test_method = "wald"
)
```

## Poisson GLMM power (count outcome)

```r
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
```

## Negative Binomial GLMM power (over-dispersed counts)

```r
d <- mp_design(clusters = list(subject = 40), trials_per_cell = 8)
a <- mp_assumptions(
  fixed_effects = list(`(Intercept)` = 0, condition = 0.4),
  residual_sd = 1,
  icc = list(subject = 0.3)
)
a$theta <- 1.5

scn_nb <- mp_scenario_lme4_nb(
  y ~ condition + (1 | subject),
  design = d,
  assumptions = a,
  test_method = "wald"
)

res_nb <- mp_power(scn_nb, nsim = 50, seed = 123)
summary(res_nb)
```
