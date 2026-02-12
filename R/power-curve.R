#' Power curve for a single design/assumption parameter
#'
#' Runs [mp_power()] across a one-dimensional grid of values for one parameter
#' (e.g. cluster size) via [mp_sensitivity()]. Results include power estimates
#' and per-grid-point diagnostics: failure rate, singular rate, and effective N.
#'
#' @param scenario An `mp_scenario`.
#' @param vary Named list with a single key (e.g. `clusters.subject`).
#' @param nsim Number of simulations per grid point (default 100).
#' @param alpha Significance level (default 0.05).
#' @param seed Optional seed for reproducibility.
#' @param failure_policy How to treat failed fits: `"count_as_nondetect"` or `"exclude"`.
#' @param conf_level Confidence level for power intervals (default 0.95).
#' @return An object of class `mp_power_curve` with components `vary`, `grid`,
#'   `results` (estimate, mcse, conf_low, conf_high, failure_rate, singular_rate,
#'   n_effective, nsim, plus the varying parameter column), `alpha`, `failure_policy`,
#'   and `conf_level`.
#' @export
mp_power_curve <- function(scenario,
                           vary,
                           nsim = 100,
                           alpha = 0.05,
                           seed = NULL,
                           failure_policy = c("count_as_nondetect", "exclude"),
                           conf_level = 0.95) {
  failure_policy <- match.arg(failure_policy)

  if (!inherits(scenario, "mp_scenario")) {
    stop("`scenario` must be an `mp_scenario` object.", call. = FALSE)
  }

  if (!is.list(vary) || length(vary) != 1L || is.null(names(vary)) || names(vary) == "") {
    stop("`vary` must be a named list with exactly one entry.", call. = FALSE)
  }

  sens <- mp_sensitivity(
    scenario = scenario,
    vary = vary,
    nsim = nsim,
    alpha = alpha,
    seed = seed,
    failure_policy = failure_policy,
    conf_level = conf_level
  )

  out <- list(
    vary = sens$vary,
    grid = sens$grid,
    results = sens$results,
    alpha = alpha,
    failure_policy = failure_policy,
    conf_level = conf_level
  )
  class(out) <- "mp_power_curve"
  out
}

#' @export
print.mp_power_curve <- function(x, ...) {
  cat("<mp_power_curve>\n")
  cat("  parameter:", names(x$vary), "\n")
  cat("  grid cells:", nrow(x$grid), "\n")
  invisible(x)
}

#' @export
summary.mp_power_curve <- function(object, ...) {
  object$results
}

#' Plot a power curve
#'
#' @param x An `mp_power_curve` object.
#' @param y What to plot on the y-axis: `"estimate"` (power), `"failure_rate"`,
#'   `"singular_rate"`, or `"n_effective"`.
#' @param ... Arguments passed to [graphics::plot()].
#' @return Invisibly returns the plotted data.
#' @export
plot.mp_power_curve <- function(x, y = c("estimate", "failure_rate", "singular_rate", "n_effective"), ...) {
  y <- match.arg(y)

  param <- names(x$vary)[[1]]
  dat <- x$results[order(x$results[[param]]), , drop = FALSE]

  graphics::plot(dat[[param]], dat[[y]], xlab = param, ylab = y, ...)

  if (identical(y, "estimate")) {
    graphics::segments(
      x0 = dat[[param]],
      y0 = dat$conf_low,
      x1 = dat[[param]],
      y1 = dat$conf_high
    )
  }

  invisible(dat)
}
