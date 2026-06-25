#' Set a smallest effect size of interest (SESOI) on a scenario
#'
#' Convenience helper for planning power around a *smallest effect size of
#' interest* rather than a single point estimate. It returns a copy of
#' `scenario` with the focal fixed effect replaced, either by an explicit
#' value or by scaling the current assumed effect (e.g. a conservative 15%
#' reduction, `multiplier = 0.85`). This is the recommended way to avoid
#' overoptimistic power based on a possibly inflated pilot/published effect
#' (Anderson, Kelley & Maxwell, 2017; Kumle, Vo & Draschkow, 2021).
#'
#' @param scenario An `mp_scenario` object.
#' @param multiplier Numeric factor applied to the current fixed effect when
#'   `effect` is not supplied (default `0.85`, a 15% reduction).
#' @param effect Optional explicit SESOI on the model's coefficient scale. When
#'   supplied it overrides `multiplier`. May be a numeric scalar or an
#'   [mp_safeguard_effect()] result.
#' @param term Fixed-effect term to modify. Defaults to the scenario's test
#'   term (or the first non-intercept fixed effect).
#' @return A modified `mp_scenario` object.
#' @seealso [mp_safeguard_effect()] for a data-driven conservative effect.
#' @export
#' @examples
#' d <- mp_design(list(subject = 30), trials_per_cell = 8)
#' a <- mp_assumptions(
#'   fixed_effects = list("(Intercept)" = 0, condition = 0.5),
#'   random_effects = list(subject = list(intercept_sd = 0.5)),
#'   residual_sd = 1
#' )
#' scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
#' # Power for an effect 15% smaller than assumed:
#' scn_sesoi <- mp_sesoi(scn, multiplier = 0.85)
#' scn_sesoi$assumptions$fixed_effects$condition
mp_sesoi <- function(scenario, multiplier = 0.85, effect = NULL, term = NULL) {
  .assert_class(scenario, "mp_scenario", "scenario")
  term <- `%||%`(term, .mp_scenario_term(scenario))
  fe <- scenario$assumptions$fixed_effects
  if (is.null(fe[[term]])) {
    .stop(sprintf("`term` '%s' is not present in the scenario's fixed_effects.", term))
  }

  if (!is.null(effect)) {
    if (inherits(effect, "mp_safeguard")) effect <- effect$safeguard
    .assert_is_num(effect, "effect")
    new_val <- as.numeric(effect)
  } else {
    .assert_is_num(multiplier, "multiplier")
    new_val <- as.numeric(fe[[term]]) * multiplier
  }

  scenario$assumptions$fixed_effects[[term]] <- new_val
  scenario
}

#' Safeguard (confidence-bound) effect size from a fitted model
#'
#' Computes a *safeguard* effect for power analysis: the bound of a confidence
#' interval for a fitted effect that lies closest to zero (Perugini, Gallucci &
#' Costantini, 2014). Planning power around this conservative, uncertainty-aware
#' value protects against the optimism of using a noisy pilot point estimate.
#'
#' The interval is the Wald (normal-approximation) interval from the fitted
#' coefficient and its standard error. Pair the result with [mp_from_fit()] and
#' [mp_sesoi()] to run a safeguard-power simulation.
#'
#' @param fit A fitted `lmer`/`glmer` model.
#' @param term Fixed-effect term. Defaults to the first non-intercept effect.
#' @param conf_level Two-sided confidence level for the interval (default
#'   `0.90`). Lower values are less conservative.
#' @return An object of class `mp_safeguard`: a list with `term`, `estimate`,
#'   `se`, `conf_level`, the interval (`lower`, `upper`), and the `safeguard`
#'   bound (the interval limit nearest zero, in the direction of the estimate).
#' @seealso [mp_sesoi()], [mp_from_fit()].
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   m <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
#'   sg <- mp_safeguard_effect(m, term = "Days", conf_level = 0.90)
#'   sg
#' }
#' }
mp_safeguard_effect <- function(fit, term = NULL, conf_level = 0.90) {
  if (!inherits(fit, c("lmerMod", "lmerModLmerTest", "glmerMod"))) {
    .stop("`mp_safeguard_effect()` currently supports lme4 fits (lmer / glmer).")
  }
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package `lme4` is required for `mp_safeguard_effect()`.", call. = FALSE)
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
      is.na(conf_level) || conf_level <= 0 || conf_level >= 1) {
    .stop("`conf_level` must be a single number in (0, 1).")
  }

  fixed <- lme4::fixef(fit)
  term <- `%||%`(term, .mp_default_test_term(fixed))
  if (!term %in% names(fixed)) {
    .stop(sprintf("`term` '%s' is not a fixed effect in the model.", term))
  }

  est <- unname(fixed[[term]])
  se <- .mp_fixef_se(fit, term)
  if (!is.finite(se) || se <= 0) {
    .stop(sprintf("Could not obtain a usable standard error for term '%s'.", term))
  }

  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  lower <- est - z * se
  upper <- est + z * se
  # Conservative bound: the interval limit nearest zero in the estimate's
  # direction (the lower limit for a positive effect, the upper for a negative).
  safeguard <- if (est >= 0) lower else upper

  out <- list(
    term = term,
    estimate = est,
    se = se,
    conf_level = conf_level,
    lower = lower,
    upper = upper,
    safeguard = safeguard,
    crosses_zero = (lower < 0 && upper > 0)
  )
  class(out) <- "mp_safeguard"
  out
}

#' @export
print.mp_safeguard <- function(x, ...) {
  cat("<mp_safeguard>\n")
  cat(sprintf("  term:        %s\n", x$term))
  cat(sprintf("  estimate:    %g (se %g)\n", x$estimate, x$se))
  cat(sprintf("  %g%% CI:     [%g, %g]\n", 100 * x$conf_level, x$lower, x$upper))
  cat(sprintf("  safeguard:   %g\n", x$safeguard))
  if (isTRUE(x$crosses_zero)) {
    cat("  note:        CI includes 0; the safeguard effect is uninformative.\n")
  }
  invisible(x)
}

# Focal term for a scenario: the explicit test term when present, otherwise the
# first non-intercept fixed effect.
.mp_scenario_term <- function(scenario) {
  if (is.list(scenario$test) && !is.null(scenario$test$term)) {
    return(scenario$test$term)
  }
  nm <- setdiff(names(scenario$assumptions$fixed_effects), "(Intercept)")
  if (length(nm) == 0L) {
    .stop("Scenario has no non-intercept fixed effect; supply `term`.")
  }
  nm[[1]]
}
