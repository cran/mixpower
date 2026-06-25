# Inference methods supported by the Gaussian lme4 (lmer) backend.
.mp_lme4_methods <- c("wald", "lrt", "satterthwaite", "kenward-roger", "pb")

#' Build an lme4 backend for MixPower scenarios
#' @param predictor Predictor column name.
#' @param subject Subject ID column name.
#' @param outcome Outcome column name.
#' @param item Optional item ID column name.
#' @param test_method Inference method: `"wald"` (normal-approximation z test,
#'   the fast default), `"satterthwaite"` or `"kenward-roger"` (df-corrected
#'   t tests via lmerTest/pbkrtest; recommended for small samples), `"lrt"`
#'   (likelihood-ratio test), or `"pb"` (parametric-bootstrap LRT via pbkrtest).
#' @param null_formula Null-model formula required for `"lrt"` and `"pb"`.
#' @param pb_nsim Bootstrap replicates for `test_method = "pb"` (default 100).
#'   Note this multiplies cost: each power replicate refits the model `pb_nsim`
#'   times.
#' @return A list containing `simulate_fun`, `fit_fun`, and `test_fun`.
#' @export
mp_backend_lme4 <- function(predictor = "condition",
                            subject = "subject",
                            outcome = "y",
                            item = NULL,
                            test_method = c("wald", "lrt", "satterthwaite",
                                            "kenward-roger", "pb"),
                            null_formula = NULL,
                            pb_nsim = 100L) {
  test_method <- match.arg(test_method)

  simulate_fun <- function(scenario, seed = NULL) {
    simulate_lmm_data(
      scenario = scenario,
      seed = seed,
      predictor = predictor,
      subject = subject,
      outcome = outcome,
      item = item
    )
  }

  fit_fun <- function(data, scenario) {
    if (!requireNamespace("lme4", quietly = TRUE)) {
      stop("Package `lme4` is required for `mp_backend_lme4()`.", call. = FALSE)
    }
    # Fit via lmerTest when available so Satterthwaite/Kenward-Roger df can be
    # computed directly; the fit is otherwise identical (same lme4 engine), so
    # Wald and LRT results are unchanged.
    fit <- if (requireNamespace("lmerTest", quietly = TRUE)) {
      lmerTest::lmer(formula = scenario$formula, data = data, REML = FALSE)
    } else {
      lme4::lmer(formula = scenario$formula, data = data, REML = FALSE)
    }

    attr(fit, "singular") <- lme4::isSingular(fit, tol = 1e-04)
    fit
  }

  test_fun <- function(fit, scenario) {
    .mp_dispatch_test(fit, scenario, predictor, test_method, null_formula, pb_nsim)
  }

  mp_backend(
    simulate_fun = simulate_fun,
    fit_fun = fit_fun,
    test_fun = test_fun,
    name = "lme4",
    capabilities = list(
      families = "gaussian",
      test_methods = .mp_lme4_methods,
      supports_random_slopes = TRUE
    )
  )
}

#' Create a fully specified MixPower scenario with the lme4 backend
#' @param formula Model formula.
#' @param design A `mp_design` object.
#' @param assumptions A `mp_assumptions` object.
#' @param predictor Predictor column name.
#' @param subject Subject ID column name.
#' @param outcome Outcome column name.
#' @param item Optional item ID column name.
#' @param test_term Term to test. A single fixed effect (default `predictor`),
#'   or a character vector of terms for an omnibus / multi-degree-of-freedom
#'   test (joint Wald for `"wald"`; for `"lrt"`/`"pb"` the `null_formula`
#'   defines the joint test).
#' @param test_method Inference method: `"wald"` (default), `"satterthwaite"`,
#'   `"kenward-roger"`, `"lrt"`, or `"pb"`. See [mp_backend_lme4()].
#' @param null_formula Null-model formula required for `"lrt"` and `"pb"`.
#' @param pb_nsim Bootstrap replicates for `test_method = "pb"` (default 100).
#' @param contrast Optional named numeric vector of fixed-effect weights
#'   defining a linear contrast `L'beta` to test (e.g. weights from `emmeans`).
#'   When supplied it overrides `test_term`/`test_method` with a Wald test of
#'   the contrast.
#' @return An object of class `mp_scenario`.
#' @export
mp_scenario_lme4 <- function(formula,
                             design,
                             assumptions,
                             predictor = "condition",
                             subject = "subject",
                             outcome = "y",
                             item = NULL,
                             test_term = predictor,
                             test_method = c("wald", "lrt", "satterthwaite",
                                             "kenward-roger", "pb"),
                             null_formula = NULL,
                             pb_nsim = 100L,
                             contrast = NULL) {
  test_method <- .mp_resolve_test_method(test_method, null_formula, .mp_lme4_methods)
  if (!is.null(contrast) && (!is.numeric(contrast) || is.null(names(contrast)))) {
    .stop("`contrast` must be a named numeric vector of coefficient weights.")
  }

  backend <- mp_backend_lme4(
    predictor = predictor,
    subject = subject,
    outcome = outcome,
    item = item,
    test_method = test_method,
    null_formula = null_formula,
    pb_nsim = pb_nsim
  )

  mp_scenario(
    formula = formula,
    design = design,
    assumptions = assumptions,
    test = list(
      term = test_term,
      method = test_method,
      null_formula = null_formula,
      pb_nsim = pb_nsim,
      contrast = contrast
    ),
    simulate_fun = backend$simulate_fun,
    fit_fun = backend$fit_fun,
    test_fun = backend$test_fun
  )
}
