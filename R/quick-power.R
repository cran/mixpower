#' Quick power run for a single LMM design
#'
#' One-call wrapper that builds design, assumptions, scenario (lme4 LMM), and
#' runs [mp_power()]. Intended for the common case: one fixed effect, one
#' random intercept. All arguments are explicit; pass further options to
#' [mp_power()] via `...` (e.g. `failure_policy`, `conf_level`, `keep`).
#'
#' @param formula Model formula (e.g. `y ~ condition + (1 | subject)`).
#' @param clusters Named list of cluster sizes (e.g. `list(subject = 40)`).
#' @param trials_per_cell Number of observations per cell (default 1).
#' @param fixed_effects Named list of effect sizes (e.g. `list(condition = 0.3)`).
#'   Include intercept as `(Intercept)` if needed.
#' @param residual_sd Residual standard deviation.
#' @param nsim Number of simulations.
#' @param alpha Significance level (default 0.05).
#' @param seed Optional seed for reproducibility.
#' @param random_effects Optional named list of random-effect sizes, e.g.
#'   `list(subject = list(intercept_sd = 0.5))`. See [mp_assumptions()].
#' @param icc Deprecated; interpreted as a random-intercept SD. Use
#'   `random_effects`.
#' @param test_method `"wald"` (default) or `"lrt"`.
#' @param null_formula Required when `test_method = "lrt"` (e.g. `y ~ 1 + (1 | subject)`).
#' @param predictor Predictor column name (default `"condition"`).
#' @param subject Subject ID column name (default `"subject"`).
#' @param outcome Outcome column name (default `"y"`).
#' @param item Optional item ID column name.
#' @param ... Arguments passed to [mp_power()] (e.g. `failure_policy`, `conf_level`, `keep`).
#' @return The result of [mp_power()] (object of class `mp_power`).
#' @export
#' @examples
#' mp_quick_power(
#'   y ~ condition + (1 | subject),
#'   clusters = list(subject = 40),
#'   trials_per_cell = 8,
#'   fixed_effects = list(`(Intercept)` = 0, condition = 0.3),
#'   residual_sd = 1,
#'   nsim = 50,
#'   seed = 123
#' )
mp_quick_power <- function(formula,
                           clusters,
                           trials_per_cell = 1,
                           fixed_effects,
                           residual_sd,
                           nsim,
                           alpha = 0.05,
                           seed = NULL,
                           random_effects = NULL,
                           icc = NULL,
                           test_method = c("wald", "lrt"),
                           null_formula = NULL,
                           predictor = "condition",
                           subject = "subject",
                           outcome = "y",
                           item = NULL,
                           ...) {
  test_method <- match.arg(test_method)

  design <- mp_design(clusters = clusters, trials_per_cell = trials_per_cell)
  assumptions <- mp_assumptions(
    fixed_effects = fixed_effects,
    random_effects = random_effects,
    residual_sd = residual_sd,
    icc = icc
  )
  scenario <- mp_scenario_lme4(
    formula = formula,
    design = design,
    assumptions = assumptions,
    predictor = predictor,
    subject = subject,
    outcome = outcome,
    item = item,
    test_term = predictor,
    test_method = test_method,
    null_formula = null_formula
  )
  mp_power(
    scenario = scenario,
    nsim = nsim,
    alpha = alpha,
    seed = seed,
    ...
  )
}
