# mixpower

<!-- badges: start -->
[![R-CMD-check](https://github.com/alitovchenko/mixpower/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/alitovchenko/mixpower/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/alitovchenko/mixpower/branch/main/graph/badge.svg)](https://app.codecov.io/gh/alitovchenko/mixpower)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

mixpower provides simulation-based power and sample-size analysis for linear and
generalized linear mixed-effects models (Gaussian, binomial, Poisson, and
negative-binomial families) fitted with `lme4`. It pairs a backend-agnostic
simulation engine with publication-ready diagnostics: exact (Clopper-Pearson)
power intervals, Monte Carlo standard errors, Type S/M error rates, and
convergence/singular-fit reporting.

## CI expectations

- `R-CMD-check`: full multi-OS package checks (release, devel, oldrel-1).
- `tests`: quick `devtools::test()` on PRs.
- `coverage`: runs tests then uploads coverage.
- `pkgdown`: builds and deploys docs from `main`.

## mixpower 0.1.0

- Initial release.
- Supports simulation-based power analysis for Gaussian linear mixed-effects models.
- Includes diagnostics for convergence and simulation uncertainty.

## Power from a fitted model (pilot data)

If you already have a fitted `lmer`/`glmer` model, `mp_from_fit()` turns it into
a scenario directly — like `simr`, but with mixpower's diagnostics (Type S/M,
exact CIs), df-corrected tests, and effect-size sensitivity.

```r
library(lme4)
pilot <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)

scn <- mp_from_fit(pilot, test_term = "Days")
mp_power(scn, nsim = 200, seed = 1)            # data-based power

# Smallest-effect-of-interest: vary the effect while keeping the fitted
# variance components.
mp_sensitivity(scn, vary = list(`fixed_effects.Days` = c(2, 5, 10)),
               nsim = 200, seed = 1)

# Scale the pilot's sample size up or down (simr-style extend) and curve power:
mp_power(mp_extend(scn, Subject = 60), nsim = 200, seed = 1)
mp_power_curve(scn, vary = list(`extend.Subject` = c(20, 40, 60, 80)),
               nsim = 200, seed = 1)
```

## Smallest effect of interest & safeguard power

Plan power around a *smallest effect size of interest* instead of an optimistic
pilot estimate. `mp_sesoi()` shrinks the focal effect (default 15%), and
`mp_safeguard_effect()` derives a conservative, uncertainty-aware effect from a
fit's confidence bound (Perugini et al., 2014).

```r
# 15% reduction (a conservative SESOI heuristic)
mp_power(mp_sesoi(scn, multiplier = 0.85), nsim = 200, seed = 1)

# Safeguard: use the CI bound nearest zero from the pilot fit
sg <- mp_safeguard_effect(pilot, term = "Days", conf_level = 0.90)
mp_power(mp_sesoi(scn, effect = sg), nsim = 200, seed = 1)
```

## Maximal models: correlated random slopes

`random_effects` supports one or more random slopes per grouping factor with a
scalar or full-matrix correlation. Each fixed effect you name becomes a balanced
design predictor, so several factors are crossed automatically.

```r
a <- mp_assumptions(
  fixed_effects = list(`(Intercept)` = 0, x1 = 0.5, x2 = 0.3),
  random_effects = list(subject = list(
    intercept_sd = 0.4,
    slopes = list(x1 = 0.3, x2 = 0.3),
    cor = 0.1
  )),
  residual_sd = 1
)
scn <- mp_scenario_lme4(y ~ x1 + x2 + (1 + x1 + x2 | subject),
                        design = mp_design(list(subject = 30), trials_per_cell = 8),
                        assumptions = a, predictor = "x1")
mp_power(scn, nsim = 200, seed = 1)
```

## Realistic designs: continuous covariates, nesting, imbalance

`mp_design()` goes beyond a single balanced binary factor: declare continuous or
between-subject predictors, nest subjects in a higher grouping factor (three
levels), or set unbalanced within-subject sample sizes.

```r
# Continuous within-subject (time-like) predictor:
mp_design(list(subject = 30), trials_per_cell = 6,
          predictors = list(time = "continuous"))

# Three-level: 8 sites, 5 subjects per site, 4 trials each:
mp_design(list(site = 8, subject = 5), trials_per_cell = 4,
          nesting = c(subject = "site"))

# Unbalanced within-subject sizes (recycled across subjects):
mp_design(list(subject = 20), trials_per_cell = c(3, 5, 8))
```

## Missing data and dropout

Reflect realistic incomplete data with `mp_missing()`: missing-completely-at-random,
missing-at-random (probability depends on an observed column), or monotone
longitudinal dropout (per-timepoint proportions or a Weibull dropout time).

```r
# 20% MCAR deletion:
mp_power(mp_missing(scn, "mcar", prob = 0.2), nsim = 200, seed = 1)

# Monotone dropout along a time predictor:
mp_power(mp_missing(scn, "dropout", time = "time",
                    dropout = c(0, 0.1, 0.2, 0.35, 0.5, 0.6)), nsim = 200, seed = 1)
```

## Flexible analysis: omnibus tests, contrasts, model comparison

Test several coefficients jointly (omnibus), a custom linear contrast, or
compare competing analysis models on the *same* simulated data to expose Type I
inflation from misspecification.

```r
# Omnibus joint Wald test of two terms:
mp_scenario_lme4(y ~ x1 + x2 + (1 | subject), d, a, test_term = c("x1", "x2"))

# Custom linear contrast (e.g. emmeans-style weights):
mp_scenario_lme4(y ~ condition + (1 | subject), d, a, contrast = c(condition = 1))

# Same-data comparison of a maximal vs reduced model:
mp_compare_models(list(maximal = scn_max, reduced = scn_int), nsim = 500, seed = 1)
```

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
  random_effects = list(subject = list(intercept_sd = 0.1))
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
  random_effects = list(subject = list(intercept_sd = 0.1))
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
  random_effects = list(subject = list(intercept_sd = 0.4))
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
  random_effects = list(subject = list(intercept_sd = 0.3))
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
  random_effects = list(subject = list(intercept_sd = 0.3))
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
  random_effects = list(subject = list(intercept_sd = 0.3))
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
