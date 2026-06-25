#' Create a grid of values for sample-size search
#'
#' Returns a numeric vector suitable for [mp_solve_sample_size()]'s `grid`
#' argument. Specify either the number of points (`length.out`) or the step
#' size (`by`); bounds are always explicit (`from`, `to`).
#'
#' @param from Lower bound (inclusive).
#' @param to Upper bound (inclusive).
#' @param length.out Number of points (optional). Uses [seq()] with
#'   `length.out`; `from` and `to` are the first and last values.
#' @param by Step size (optional). Uses [seq()] with `by`; sequence runs from
#'   `from` to `to` in steps of `by`.
#' @return A numeric vector. For integer cluster sizes, use
#'   `round(mp_grid_sample_size(...))` or pass `by` as an integer (e.g. `by = 10`).
#' @export
#' @examples
#' mp_grid_sample_size(20, 100, length.out = 9)
#' mp_grid_sample_size(20, 100, by = 10)
mp_grid_sample_size <- function(from, to, length.out = NULL, by = NULL) {
  if (!is.numeric(from) || length(from) != 1L || !is.numeric(to) || length(to) != 1L) {
    stop("`from` and `to` must be numeric scalars.", call. = FALSE)
  }
  if (is.null(length.out) && is.null(by)) {
    stop("Specify either `length.out` or `by`.", call. = FALSE)
  }
  if (!is.null(length.out) && !is.null(by)) {
    stop("Specify only one of `length.out` or `by`.", call. = FALSE)
  }
  if (!is.null(length.out)) {
    if (!is.numeric(length.out) || length(length.out) != 1L || length.out < 1) {
      stop("`length.out` must be a positive integer.", call. = FALSE)
    }
    return(seq(from, to, length.out = as.integer(length.out)))
  }
  if (!is.numeric(by) || length(by) != 1L || by == 0) {
    stop("`by` must be a non-zero numeric scalar.", call. = FALSE)
  }
  seq(from, to, by = by)
}

#' Solve for minimum sample size achieving target power
#'
#' Evaluates power on a user-supplied grid of values for one parameter (e.g.
#' cluster size) via [mp_power_curve()], then returns the smallest grid value
#' whose power estimate meets or exceeds the target. Diagnostics (failure rate,
#' singular rate, n_effective) are exposed in the returned `results` table.
#'
#' @param scenario An `mp_scenario`.
#' @param parameter Dotted path of the single parameter to vary (e.g. `"clusters.subject"`).
#' @param grid Numeric vector of candidate values. Use [mp_grid_sample_size()] to
#'   build a grid from bounds and either `length.out` or `by`.
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
