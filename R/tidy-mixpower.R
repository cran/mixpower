#' Coerce mixpower results to a tibble
#'
#' Requires the \pkg{tibble} package (installed with the tidyverse).
#'
#' @param x An `mp_power`, `mp_sensitivity`, or `mp_power_curve` object.
#' @param ... Passed to [tibble::as_tibble()] on the underlying data frame.
#' @return A [tibble::tibble()].
#' @exportS3Method tibble::as_tibble
#' @aliases as_tibble.mp_power
as_tibble.mp_power <- function(x, ...) {
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package `tibble` is required for `as_tibble()` on mixpower objects.", call. = FALSE)
  }
  tibble::as_tibble(x$sims, ...)
}

#' @exportS3Method tibble::as_tibble
as_tibble.mp_sensitivity <- function(x, ...) {
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package `tibble` is required for `as_tibble()` on mixpower objects.", call. = FALSE)
  }
  tibble::as_tibble(x$results, ...)
}

#' @exportS3Method tibble::as_tibble
as_tibble.mp_power_curve <- function(x, ...) {
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package `tibble` is required for `as_tibble()` on mixpower objects.", call. = FALSE)
  }
  tibble::as_tibble(x$results, ...)
}

#' ggplot2 diagnostic plot for sensitivity or power curve
#'
#' Requires \pkg{ggplot2}. Intended as an optional alternative to base
#' [plot.mp_sensitivity()] / [plot.mp_power_curve()].
#'
#' @param object An `mp_sensitivity` or `mp_power_curve` object.
#' @param ... Unused; reserved for consistency with ggplot2 generics.
#' @param y For sensitivity/curve objects: same as `plot()` — `"estimate"`,
#'   `"failure_rate"`, `"singular_rate"`, or `"n_effective"`.
#' @return A \pkg{ggplot2} object.
#' @exportS3Method ggplot2::autoplot
autoplot.mp_sensitivity <- function(object,
                                    ...,
                                    y = c("estimate", "failure_rate", "singular_rate", "n_effective")) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package `ggplot2` is required for `autoplot()`.", call. = FALSE)
  }
  y <- match.arg(y)
  nv <- length(object$vary)
  if (nv != 1L) {
    stop("`autoplot.mp_sensitivity` currently supports only one varying parameter.", call. = FALSE)
  }
  param <- names(object$vary)[[1L]]
  dat <- object$results[order(object$results[[param]]), , drop = FALSE]
  ggplot2::ggplot(dat, ggplot2::aes(x = .data[[param]], y = .data[[y]])) +
    ggplot2::geom_line() +
    ggplot2::labs(x = param, y = y, title = "mixpower sensitivity")
}

#' @exportS3Method ggplot2::autoplot
autoplot.mp_power_curve <- function(object,
                                    ...,
                                    y = c("estimate", "failure_rate", "singular_rate", "n_effective")) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package `ggplot2` is required for `autoplot()`.", call. = FALSE)
  }
  y <- match.arg(y)
  param <- names(object$vary)[[1L]]
  dat <- object$results[order(object$results[[param]]), , drop = FALSE]
  ggplot2::ggplot(dat, ggplot2::aes(x = .data[[param]], y = .data[[y]])) +
    ggplot2::geom_line() +
    ggplot2::labs(x = param, y = y, title = "mixpower power curve")
}
