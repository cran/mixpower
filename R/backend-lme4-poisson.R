#' Build an lme4 backend for Poisson GLMM scenarios
#' @param predictor Predictor column name.
#' @param subject Subject ID column name.
#' @param outcome Outcome column name.
#' @param item Optional item ID column name.
#' @param test_method Inference method: `"wald"` (default), `"lrt"`, or `"pb"`
#'   (parametric-bootstrap LRT via pbkrtest).
#' @param null_formula Null-model formula required for `"lrt"` and `"pb"`.
#' @param pb_nsim Bootstrap replicates for `test_method = "pb"` (default 100).
#' @return A list containing `simulate_fun`, `fit_fun`, and `test_fun`.
#' @export
mp_backend_lme4_poisson <- function(predictor = "condition",
                                    subject = "subject",
                                    outcome = "y",
                                    item = NULL,
                                    test_method = c("wald", "lrt", "pb"),
                                    null_formula = NULL,
                                    pb_nsim = 100L) {
  test_method <- match.arg(test_method)

  simulate_fun <- function(scenario, seed = NULL) {
    simulate_glmm_poisson_data(
      scenario = scenario,
      predictor = predictor,
      subject = subject,
      outcome = outcome,
      item = item
    )
  }

  fit_fun <- function(data, scenario) {
    if (!requireNamespace("lme4", quietly = TRUE)) {
      stop("Package `lme4` is required for `mp_backend_lme4_poisson()`.", call. = FALSE)
    }

    fit <- lme4::glmer(
      formula = scenario$formula,
      data = data,
      family = stats::poisson(link = "log")
    )

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
    name = "lme4_poisson",
    capabilities = list(families = "poisson", test_methods = .mp_glmm_methods)
  )
}

#' Create a fully specified MixPower scenario with the Poisson lme4 backend
#' @param formula Model formula.
#' @param design A `mp_design` object.
#' @param assumptions A `mp_assumptions` object.
#' @param predictor Predictor column name.
#' @param subject Subject ID column name.
#' @param outcome Outcome column name.
#' @param item Optional item ID column name.
#' @param test_term Optional explicit term to test. Defaults to `predictor`.
#' @param test_method Inference method: `"wald"` (default), `"lrt"`, or `"pb"`.
#' @param null_formula Null-model formula required for `"lrt"` and `"pb"`.
#' @param pb_nsim Bootstrap replicates for `test_method = "pb"` (default 100).
#' @return An object of class `mp_scenario`.
#' @export
mp_scenario_lme4_poisson <- function(formula,
                                     design,
                                     assumptions,
                                     predictor = "condition",
                                     subject = "subject",
                                     outcome = "y",
                                     item = NULL,
                                     test_term = predictor,
                                     test_method = c("wald", "lrt", "pb"),
                                     null_formula = NULL,
                                     pb_nsim = 100L) {
  test_method <- .mp_resolve_test_method(test_method, null_formula, .mp_glmm_methods)

  backend <- mp_backend_lme4_poisson(
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
    test = list(term = test_term, method = test_method,
                null_formula = null_formula, pb_nsim = pb_nsim),
    simulate_fun = backend$simulate_fun,
    fit_fun = backend$fit_fun,
    test_fun = backend$test_fun
  )
}
