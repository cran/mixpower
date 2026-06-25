#' Generate a methods paragraph for a power analysis
#'
#' Produces a ready-to-edit prose description of a simulation-based power
#' analysis from an [mp_power()] result: the design, model, effect tested,
#' inference method, number of simulations, and the estimated power with its
#' interval. Intended to seed the "Power analysis" paragraph of a methods
#' section.
#'
#' @param result An `mp_power` object (from [mp_power()]).
#' @param software Include a sentence naming the mixpower package and version
#'   (default `TRUE`).
#' @return A length-1 character string with class `mp_methods_text` (its
#'   `print()` method word-wraps the paragraph).
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   d <- mp_design(list(subject = 30), trials_per_cell = 6)
#'   a <- mp_assumptions(list("(Intercept)" = 0, condition = 0.4),
#'                       random_effects = list(subject = list(intercept_sd = 0.5)),
#'                       residual_sd = 1)
#'   scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
#'   mp_methods_text(mp_power(scn, nsim = 50, seed = 1))
#' }
#' }
mp_methods_text <- function(result, software = TRUE) {
  if (!inherits(result, "mp_power")) {
    .stop("`result` must be an `mp_power` object (from mp_power()).")
  }
  scn <- result$scenario
  test <- scn$test
  term <- if (is.list(test)) test$term else NA_character_
  method <- if (is.list(test)) `%||%`(test$method, "wald") else as.character(test)

  clusters <- scn$design$clusters
  cl_txt <- paste(sprintf("%d %s", unlist(clusters), names(clusters)), collapse = ", ")
  tpc <- scn$design$trials_per_cell
  tpc_txt <- if (length(tpc) == 1L) {
    sprintf("%d observations each", tpc)
  } else {
    "an unbalanced number of observations each"
  }
  formula_txt <- paste(deparse(scn$formula), collapse = " ")

  effect <- if (length(term) == 1L && !is.na(term)) scn$assumptions$fixed_effects[[term]] else NULL
  effect_sentence <- if (!is.null(effect) && is.numeric(effect) && length(effect) == 1L) {
    sprintf("The focal effect (%s) was set to %g and tested with a %s test at alpha = %g.",
            term, effect, .mp_method_label(method), result$alpha)
  } else {
    sprintf("The focal effect was tested with a %s test at alpha = %g.",
            .mp_method_label(method), result$alpha)
  }

  ci <- result$ci
  parts <- c(
    sprintf("A simulation-based power analysis was conducted%s.",
            if (isTRUE(software)) {
              sprintf(" using the mixpower R package (version %s)", utils::packageVersion("mixpower"))
            } else {
              ""
            }),
    sprintf("Data were simulated under the model %s, with %s (%s).",
            formula_txt, cl_txt, tpc_txt),
    effect_sentence,
    sprintf("Across %d simulated datasets, estimated power was %.1f%% (%g%% %s confidence interval %.1f%% to %.1f%%).",
            result$nsim, 100 * result$power, 100 * result$conf_level,
            .mp_ci_label(result$ci_method), 100 * ci[[1]], 100 * ci[[2]])
  )

  tm <- result$diagnostics$type_m
  if (!is.null(tm) && is.finite(tm)) {
    parts <- c(parts, sprintf(
      "Among significant replicates the average exaggeration ratio (Type M) was %.2f.", tm
    ))
  }

  structure(paste(parts, collapse = " "), class = "mp_methods_text")
}

#' @export
print.mp_methods_text <- function(x, ...) {
  cat(strwrap(unclass(x), width = getOption("width", 80L)), sep = "\n")
  invisible(x)
}

.mp_method_label <- function(method) {
  switch(method,
    wald = "Wald z/t",
    lrt = "likelihood-ratio",
    satterthwaite = "Satterthwaite-approximated t",
    "kenward-roger" = "Kenward-Roger-approximated t",
    pb = "parametric-bootstrap likelihood-ratio",
    method
  )
}

.mp_ci_label <- function(ci_method) {
  if (identical(ci_method, "wald")) "Wald" else "Clopper-Pearson"
}

#' Plot the p-value distribution of a power analysis
#'
#' A histogram of the per-replicate p-values from [mp_power()], with the `alpha`
#' threshold marked. The shaded area to the left of `alpha` is the estimated
#' power. Requires a run with `aggregate = "full"` (the default), which retains
#' per-replicate p-values.
#'
#' @param x An `mp_power` object.
#' @param ... Passed to [graphics::hist()].
#' @return Invisibly, the p-values plotted.
#' @export
plot.mp_power <- function(x, ...) {
  p <- x$sims$p_value
  p <- p[is.finite(p)]
  if (length(p) == 0L) {
    .stop("No per-replicate p-values to plot (was the run streaming?).")
  }
  graphics::hist(p, breaks = seq(0, 1, by = 0.05),
                 xlab = "p-value", main = sprintf("Power = %.1f%%", 100 * x$power), ...)
  graphics::abline(v = x$alpha, col = "red", lty = 2)
  invisible(p)
}
