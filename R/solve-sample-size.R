#' Solve for minimum sample size achieving target power
#'
#' Evaluates power on a user-supplied grid of values for one parameter (e.g.
#' cluster size) via [mp_power_curve()], then returns the smallest grid value
#' whose power estimate meets or exceeds the target. Diagnostics (failure rate,
#' singular rate, n_effective) are exposed in the returned `results` table.
#'
#' @param scenario An `mp_scenario`.
#' @param parameter Dotted path of the single parameter to vary (e.g. `"clusters.subject"`).
#' @param grid Numeric vector of candidate values.
#' @param target_power Target power threshold (default 0.8).
#' @param nsim Number of simulations per grid point (default 100).
#' @param alpha Significance level (default 0.05).
#' @param seed Optional seed for reproducibility.
#' @param failure_policy How to treat failed fits: `"count_as_nondetect"` or `"exclude"`.
#' @param conf_level Confidence level for power intervals (default 0.95).
#' @return A list with `target_power`, `parameter`, `solution` (numeric: minimum
#'   grid value achieving target power, or `NA` if none), and `results` (data
#'   frame with estimate, failure_rate, singular_rate, n_effective, etc., per
#'   grid point).
#' @export
mp_solve_sample_size <- function(scenario,
                                 parameter,
                                 grid,
                                 target_power = 0.8,
                                 nsim = 100,
                                 alpha = 0.05,
                                 seed = NULL,
                                 failure_policy = c("count_as_nondetect", "exclude"),
                                 conf_level = 0.95) {
  failure_policy <- match.arg(failure_policy)

  if (!inherits(scenario, "mp_scenario")) {
    stop("`scenario` must be an `mp_scenario` object.", call. = FALSE)
  }
  if (!is.character(parameter) || length(parameter) != 1L) {
    stop("`parameter` must be a single string key.", call. = FALSE)
  }
  if (!is.numeric(grid) || length(grid) == 0L) {
    stop("`grid` must be a non-empty numeric vector.", call. = FALSE)
  }

  vary <- list()
  vary[[parameter]] <- grid

  curve <- mp_power_curve(
    scenario = scenario,
    vary = vary,
    nsim = nsim,
    alpha = alpha,
    seed = seed,
    failure_policy = failure_policy,
    conf_level = conf_level
  )

  dat <- curve$results[order(curve$results[[parameter]]), , drop = FALSE]
  ok <- which(dat$estimate >= target_power)
  solution <- if (length(ok) > 0) dat[[parameter]][min(ok)] else NA_real_

  list(
    target_power = target_power,
    parameter = parameter,
    solution = solution,
    results = dat
  )
}
