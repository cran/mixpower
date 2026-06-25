#' Build a glmmTMB backend for Gaussian LMM scenarios
#'
#' Fits with the **glmmTMB** function `glmmTMB()` (Gaussian). Useful for comparing simulation-based
#' power to [mp_backend_lme4()] and for workflows that later extend to families
#' supported by glmmTMB but not lme4.
#'
#' @inheritParams mp_backend_lme4
#' @return An object of class `mp_backend`.
#' @export
mp_backend_glmmtmb <- function(predictor = "condition",
                               subject = "subject",
                               outcome = "y",
                               item = NULL,
                               test_method = c("wald", "lrt"),
                               null_formula = NULL) {
  if (!requireNamespace("glmmTMB", quietly = TRUE)) {
    stop("Package `glmmTMB` is required for `mp_backend_glmmtmb()`.", call. = FALSE)
  }
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
    fit <- glmmTMB::glmmTMB(
      formula = scenario$formula,
      data = data,
      family = stats::gaussian()
    )
    se <- tryCatch(
      summary(fit)$coefficients$cond[, "Std. Error"],
      error = function(e) NA_real_
    )
    attr(fit, "singular") <- isTRUE(anyNA(se)) || isTRUE(any(se <= 0, na.rm = TRUE))
    fit
  }

  test_fun <- function(fit, scenario) {
    .mp_dispatch_test(fit, scenario, predictor, test_method, null_formula)
  }

  mp_backend(
    simulate_fun = simulate_fun,
    fit_fun = fit_fun,
    test_fun = test_fun,
    name = "glmmtmb_gaussian",
    capabilities = list(families = "gaussian",
                        test_methods = c("wald", "lrt"), engine = "glmmTMB")
  )
}

#' Gaussian LMM scenario using glmmTMB
#'
#' Same data-generating process as [mp_scenario_lme4()] but fits with **glmmTMB**
#' (`glmmTMB()`).
#'
#' @inheritParams mp_scenario_lme4
#' @export
mp_scenario_glmmtmb_lmm <- function(formula,
                                    design,
                                    assumptions,
                                    predictor = "condition",
                                    subject = "subject",
                                    outcome = "y",
                                    item = NULL,
                                    test_term = predictor,
                                    test_method = c("wald", "lrt"),
                                    null_formula = NULL) {
  test_method <- match.arg(test_method)
  if (identical(test_method, "lrt") && (is.null(null_formula) || !inherits(null_formula, "formula"))) {
    stop("`null_formula` must be provided as a formula when `test_method = \"lrt\"`.", call. = FALSE)
  }

  backend <- mp_backend_glmmtmb(
    predictor = predictor,
    subject = subject,
    outcome = outcome,
    item = item,
    test_method = test_method,
    null_formula = null_formula
  )

  mp_scenario(
    formula = formula,
    design = design,
    assumptions = assumptions,
    test = list(
      term = test_term,
      method = test_method,
      null_formula = null_formula
    ),
    simulate_fun = backend$simulate_fun,
    fit_fun = backend$fit_fun,
    test_fun = backend$test_fun
  )
}
