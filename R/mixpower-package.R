#' Simulation-Based Power Analysis for Mixed-Effects Models
#'
#' @description
#' **mixpower** is a simulation-based toolkit for power and sample-size analysis
#' for linear and generalized linear mixed-effects models (LMMs and GLMMs). It is
#' design-first (no pilot data required), supports Gaussian, binomial, Poisson,
#' and negative binomial families via **lme4**; Wald and likelihood-ratio tests;
#' multi-parameter sensitivity grids; power curves and minimum sample-size
#' solvers; parallel evaluation with deterministic seeds; and full
#' reproducibility (manifests, result bundling, export to CSV/JSON). Every run
#' reports diagnostics (failure rate, singular-fit rate, effective N).
#'
#' @details
#' Typical workflow: (1) define a design with `mp_design()` (cluster sizes,
#' trials per cell) and effect-size assumptions with `mp_assumptions()`; (2) build
#' a scenario with a backend constructor (e.g. `mp_scenario_lme4()` for Gaussian
#' LMM); (3) run `mp_power()` for a single power estimate, `mp_sensitivity()` to
#' vary parameters, or `mp_power_curve()` / `mp_solve_sample_size()` for curves
#' and sample-size. Use a fixed `seed` for reproducibility. Failure and
#' singular-fit rates are always reported and never suppressed.
#'
#' @section Getting started:
#' After loading the package, define a design and assumptions, build an lme4
#' scenario, then call `mp_power()`. See Examples below for a minimal
#' runnable workflow.
#'
#' @section Function overview:
#' **Design and assumptions:** `mp_design()`, `mp_assumptions()`.
#'
#' **Scenarios (LMM/GLMM):** `mp_scenario()`, `mp_scenario_lme4()`,
#' `mp_scenario_lme4_binomial()`, `mp_scenario_lme4_poisson()`, `mp_scenario_lme4_nb()`.
#'
#' **Power and sensitivity:** `mp_power()` (optional `aggregate = "streaming"`),
#' `mp_sensitivity()`, `mp_sensitivity_parallel()`, `mp_power_curve()`,
#' `mp_power_curve_parallel()`, `mp_solve_sample_size()`, `mp_grid_sample_size()`,
#' `mp_quick_power()`.
#'
#' **Backends and simulators:** `mp_backend()`, `validate_mp_backend()`, `mp_backend_lme4()`,
#' `mp_backend_lme4_binomial()`, `mp_backend_lme4_poisson()`, `mp_backend_lme4_nb()`,
#' `mp_backend_glmmtmb()`, `mp_scenario_glmmtmb_lmm()`, `simulate_glmm_binomial_data()`,
#' `simulate_glmm_poisson_data()`, `simulate_glmm_nb_data()`.
#'
#' **Optional tidyverse (Suggests):** `tibble::as_tibble()` and `ggplot2::autoplot()` methods.
#'
#' **Reproducibility and reporting:** `mp_manifest()`, `mp_bundle_results()`,
#' `mp_report_table()`, `mp_write_results()`.
#'
#' **Plotting:** `plot()` for `mp_power_curve` and `mp_sensitivity` objects.
#'
#' @section Getting the most out of mixpower:
#' * Use a fixed `seed` (e.g. `seed = 123`) so runs are reproducible.
#' * Check `failure_rate` and `singular_rate` in results; investigate if high.
#' * For nested model comparison use LRT with an explicit `null_formula`.
#' * Use `mp_power_curve()` or `mp_solve_sample_size()` to choose sample size.
#' * Vignettes give step-by-step guides: `vignette("mixpower-intro", package = "mixpower")`,
#'   `vignette("mixpower-design", package = "mixpower")`,
#'   `vignette("mixpower-simulations", package = "mixpower")`,
#'   `vignette("mixpower-diagnostics", package = "mixpower")`,
#'   `vignette("mixpower-reproducibility", package = "mixpower")`,
#'   `vignette("mixpower-extending", package = "mixpower")`.
#'
#' @examples
#' # Minimal workflow: design -> assumptions -> scenario -> power
#' d <- mp_design(clusters = list(subject = 30), trials_per_cell = 4)
#' a <- mp_assumptions(
#'   fixed_effects = list(`(Intercept)` = 0, condition = 0.3),
#'   residual_sd = 1
#' )
#' scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
#' res <- mp_power(scn, nsim = 50, seed = 123)
#' summary(res)
#'
#' @seealso
#' Entry points: [mp_design()], [mp_power()], [mp_quick_power()].
#' Vignettes: `vignette(package = "mixpower")`.
#'
#' @references
#' Bates D, Maechler M, Bolker B, Walker S (2015). "Fitting Linear Mixed-Effects
#' Models Using lme4." *Journal of Statistical Software*, 67(1), 1–48.
#' \doi{10.18637/jss.v067.i01}.
#'
#' Green P, MacLeod CJ (2016). "SIMR: an R package for power analysis of
#' generalized linear mixed models by simulation." *Methods in Ecology and
#' Evolution*, 7(4), 493–498. \doi{10.1111/2041-210X.12504}.
#'
#' @name mixpower-package
#' @aliases mixpower
"_PACKAGE"

# `.data` is the rlang/ggplot2 pronoun used inside autoplot() tidy-eval; declare
# it so R CMD check does not flag it as an undefined global.
utils::globalVariables(".data")
