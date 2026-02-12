#' Create a power-analysis scenario
#'
#' A scenario combines: (1) a design, (2) assumptions, (3) a model specification,
#' and (4) an analysis engine.
#'
#' In Phase 1, the engine is *pluggable* via three functions:
#' - `simulate_fun(scenario, seed)` returns a data.frame
#' - `fit_fun(data, scenario)` returns a fit object
#' - `test_fun(fit, scenario)` returns a list with at least `p_value` (numeric scalar)
#'
#' This allows `mp_power()` to run before selecting a specific backend (e.g., lme4).
#'
#' @param formula A model formula (stored for later backends).
#' @param design An `mp_design`.
#' @param assumptions An `mp_assumptions`.
#' @param test Character string or list identifying the test type (metadata).
#' @param simulate_fun Function or NULL.
#' @param fit_fun Function or NULL.
#' @param test_fun Function or NULL.
#' @param notes Optional free text.
#'
#' @return An object of class `mp_scenario`.
#' @export
#'
#' @examples
#' d <- mp_design(list(subject = 20), trials_per_cell = 5)
#' a <- mp_assumptions(list(condition = 0.3), residual_sd = 1)
#' s <- mp_scenario(y ~ condition, d, a, test = "wald")
#' s
mp_scenario <- function(formula,
                        design,
                        assumptions,
                        test = c("wald", "lrt", "custom"),
                        simulate_fun = NULL,
                        fit_fun = NULL,
                        test_fun = NULL,
                        notes = NULL) {
  if (!inherits(formula, "formula")) .stop("`formula` must be a formula.")
  .assert_class(design, "mp_design", "design")
  .assert_class(assumptions, "mp_assumptions", "assumptions")
  if (is.character(test)) {
    test <- match.arg(test)
  } else if (!is.list(test)) {
    .stop("`test` must be a character string or list.")
  }

  .assert_fun_or_null(simulate_fun, "simulate_fun")
  .assert_fun_or_null(fit_fun, "fit_fun")
  .assert_fun_or_null(test_fun, "test_fun")

  if (!is.null(notes) && (!is.character(notes) || length(notes) != 1)) {
    .stop("`notes` must be a length-1 character string or NULL.")
  }

  out <- list(
    formula = formula,
    design = design,
    assumptions = assumptions,
    test = test,
    engine = list(
      simulate_fun = simulate_fun,
      fit_fun = fit_fun,
      test_fun = test_fun
    ),
    notes = notes
  )
  class(out) <- "mp_scenario"
  out
}

#' @export
print.mp_scenario <- function(x, ...) {
  cat("<mp_scenario>\n")
  cat(sprintf("  formula: %s\n", deparse(x$formula)))
  if (is.list(x$test)) {
    method <- x$test$method %||% "custom"
    cat(sprintf("  test: %s\n", method))
  } else {
    cat(sprintf("  test: %s\n", x$test))
  }
  cat("  engine:\n")
  cat(sprintf("    - simulate_fun: %s\n", ifelse(is.null(x$engine$simulate_fun), "NULL", "set")))
  cat(sprintf("    - fit_fun: %s\n", ifelse(is.null(x$engine$fit_fun), "NULL", "set")))
  cat(sprintf("    - test_fun: %s\n", ifelse(is.null(x$engine$test_fun), "NULL", "set")))
  invisible(x)
}

#' @export
summary.mp_scenario <- function(object, ...) {
  object
}
