#' Check the Type I error calibration of a scenario's test
#'
#' Runs the scenario under the null hypothesis --- the focal fixed effect set to
#' zero --- and estimates the empirical Type I error rate, i.e. the proportion of
#' replicates in which the (false) effect is declared significant. A trustworthy
#' test rejects at approximately `alpha`. The estimate is compared to `alpha`
#' using an exact (Clopper-Pearson) interval, giving a verdict:
#'
#' * `"well-calibrated"`: the interval contains `alpha`.
#' * `"anti-conservative"`: the interval lies entirely above `alpha` (the test
#'   rejects too often --- e.g. a Wald test with few clusters, or a model that
#'   omits a random slope that is actually present in the data-generating
#'   process). Power computed with such a test is not trustworthy.
#' * `"conservative"`: the interval lies entirely below `alpha`.
#'
#' This is the recommended sanity check to run before trusting a power number:
#' if a design/analysis combination does not control Type I error, its power is
#' meaningless.
#'
#' @param scenario An `mp_scenario`.
#' @param term Focal fixed-effect term to null out. Defaults to the scenario's
#'   test term (or the first non-intercept fixed effect).
#' @param nsim Number of null simulations (default 1000; Type I estimation needs
#'   more replicates than a power point estimate for a tight interval).
#' @param alpha Nominal significance level being checked (default 0.05).
#' @param seed Optional seed for reproducibility.
#' @param conf_level Confidence level for the Type I interval (default 0.95).
#' @param failure_policy Passed to [mp_power()].
#' @return An object of class `mp_calibration`: a list with `term`, `alpha`,
#'   `type1` (empirical Type I rate), `ci`, `conf_level`, `mcse`, `nsim`,
#'   `verdict`, and the underlying `power_result`.
#' @seealso [mp_recommend_method()], [mp_power()].
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   d <- mp_design(list(subject = 40), trials_per_cell = 6)
#'   a <- mp_assumptions(
#'     fixed_effects = list("(Intercept)" = 0, condition = 0.4),
#'     random_effects = list(subject = list(intercept_sd = 0.5)),
#'     residual_sd = 1
#'   )
#'   scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
#'   mp_calibrate(scn, nsim = 200, seed = 1)
#' }
#' }
mp_calibrate <- function(scenario,
                         term = NULL,
                         nsim = 1000,
                         alpha = 0.05,
                         seed = NULL,
                         conf_level = 0.95,
                         failure_policy = c("count_as_nondetect", "exclude")) {
  failure_policy <- match.arg(failure_policy)
  .assert_class(scenario, "mp_scenario", "scenario")
  term <- `%||%`(term, .mp_scenario_term(scenario))

  null_scenario <- mp_sesoi(scenario, effect = 0, term = term)
  res <- mp_power(
    null_scenario,
    nsim = nsim,
    alpha = alpha,
    seed = seed,
    conf_level = conf_level,
    ci_method = "clopper-pearson",
    failure_policy = failure_policy
  )

  ci <- res$ci
  verdict <- if (ci[[1]] > alpha) {
    "anti-conservative"
  } else if (ci[[2]] < alpha) {
    "conservative"
  } else {
    "well-calibrated"
  }

  out <- list(
    term = term,
    alpha = alpha,
    type1 = res$power,
    ci = ci,
    conf_level = conf_level,
    mcse = res$mcse,
    nsim = nsim,
    verdict = verdict,
    power_result = res
  )
  class(out) <- "mp_calibration"
  out
}

#' @export
print.mp_calibration <- function(x, ...) {
  cat("<mp_calibration>\n")
  cat(sprintf("  term:    %s (true effect set to 0)\n", x$term))
  cat(sprintf("  nominal alpha: %g\n", x$alpha))
  cat(sprintf("  empirical Type I: %.4f  (%g%% CI %.4f, %.4f)\n",
              x$type1, 100 * x$conf_level, x$ci[[1]], x$ci[[2]]))
  cat(sprintf("  verdict: %s\n", x$verdict))
  if (identical(x$verdict, "anti-conservative")) {
    cat("  -> This test rejects too often under the null; its power is not\n")
    cat("     trustworthy. Try a df-corrected or bootstrap test, or a model\n")
    cat("     that matches the data-generating random-effects structure.\n")
  }
  invisible(x)
}

#' @export
summary.mp_calibration <- function(object, ...) {
  data.frame(
    term = object$term,
    alpha = object$alpha,
    type1 = object$type1,
    ci_low = object$ci[[1]],
    ci_high = object$ci[[2]],
    verdict = object$verdict,
    nsim = object$nsim,
    stringsAsFactors = FALSE
  )
}
