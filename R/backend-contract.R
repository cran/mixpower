#' MixPower backend contract
#'
#' A **backend** is a list with three functions used by [mp_power()]:
#' \describe{
#'   \item{`simulate_fun`}{First argument receives the `mp_scenario` (often named
#'     `scenario`); optional `seed`. Must return a `data.frame`.}
#'   \item{`fit_fun`}{Two arguments: simulated `data.frame`, then the `mp_scenario`
#'     (names may differ; [mp_power()] passes them positionally).}
#'   \item{`test_fun`}{Two arguments: fitted model, then the `mp_scenario`; returns
#'     `list(p_value = <numeric scalar>)`.}
#' }
#'
#' Use `mp_backend()` to build a validated object of class `mp_backend`. Custom
#' backends can also be plain lists with those three names; `validate_mp_backend()`
#' checks the contract without requiring the class.
#'
#' @param simulate_fun A data.frame-generating simulator (see Details).
#' @param fit_fun Model fitter (see Details).
#' @param test_fun Extracts a scalar p-value (see Details).
#' @param name Short label for printing and manifests (default `"custom"`).
#' @param version Optional character version string for the backend implementation.
#' @param notes Optional longer notes.
#' @param capabilities Optional named list of flags (documentation only), e.g.
#'   `list(families = c("gaussian"), supports_lrt = TRUE)`.
#'
#' @return An object of class `c("mp_backend", "list")` with the components above.
#' @export
mp_backend <- function(simulate_fun,
                       fit_fun,
                       test_fun,
                       name = "custom",
                       version = NULL,
                       notes = NULL,
                       capabilities = NULL) {
  obj <- list(
    simulate_fun = simulate_fun,
    fit_fun = fit_fun,
    test_fun = test_fun,
    name = name,
    version = version,
    notes = notes,
    capabilities = capabilities
  )
  validate_mp_backend(obj)
  class(obj) <- c("mp_backend", "list")
  obj
}

#' Validate a MixPower backend
#'
#' Checks that `simulate_fun`, `fit_fun`, and `test_fun` are functions and that
#' their formal arguments are compatible with what [mp_power()] and
#' `.run_one_rep()` invoke. Does not run simulations.
#'
#' @param engine A `mp_backend` object or a plain list with `simulate_fun`,
#'   `fit_fun`, and `test_fun`.
#' @return Invisibly `TRUE` if valid; otherwise throws an error.
#' @export
validate_mp_backend <- function(engine) {
  if (!is.list(engine)) {
    stop("`engine` must be a list or mp_backend object.", call. = FALSE)
  }
  for (nm in c("simulate_fun", "fit_fun", "test_fun")) {
    if (is.null(engine[[nm]]) || !is.function(engine[[nm]])) {
      stop(sprintf("Backend must have a function `%s`.", nm), call. = FALSE)
    }
  }

  sim_fm <- formals(engine[["simulate_fun"]])
  if (length(sim_fm) < 1L) {
    stop("`simulate_fun` must have at least one formal argument (the scenario object).", call. = FALSE)
  }
  if (identical(names(sim_fm)[[1L]], "...")) {
    stop("`simulate_fun` cannot take only `...` as its first argument.", call. = FALSE)
  }

  fit_fm <- formals(engine[["fit_fun"]])
  if (length(fit_fm) < 2L) {
    stop("`fit_fun` must have at least two arguments (data, then scenario).", call. = FALSE)
  }

  test_fm <- formals(engine[["test_fun"]])
  if (length(test_fm) < 2L) {
    stop("`test_fun` must have at least two arguments (fit, then scenario).", call. = FALSE)
  }

  invisible(TRUE)
}

#' @export
print.mp_backend <- function(x, ...) {
  cat("<mp_backend>\n")
  cat("  name:", if (is.null(x$name)) "custom" else x$name, "\n")
  if (!is.null(x$version)) cat("  version:", x$version, "\n")
  cat("  simulate_fun:", deparse(utils::head(formals(x$simulate_fun), 1L)), "...\n")
  invisible(x)
}
