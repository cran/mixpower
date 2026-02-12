#' Build an lme4 backend for Negative Binomial GLMM scenarios
#' @param predictor Predictor column name.
#' @param subject Subject ID column name.
#' @param outcome Outcome column name.
#' @param item Optional item ID column name.
#' @param test_method Inference method (`"wald"` or `"lrt"`).
#' @param null_formula Optional null model formula for `"lrt"` tests.
#' @return A list containing `simulate_fun`, `fit_fun`, and `test_fun`.
#' @export
mp_backend_lme4_nb <- function(predictor = "condition",
                               subject = "subject",
                               outcome = "y",
                               item = NULL,
                               test_method = c("wald", "lrt"),
                               null_formula = NULL) {
  test_method <- match.arg(test_method)

  simulate_fun <- function(scenario, seed = NULL) {
    simulate_glmm_nb_data(
      scenario = scenario,
      predictor = predictor,
      subject = subject,
      outcome = outcome,
      item = item
    )
  }

  fit_fun <- function(data, scenario) {
    if (!requireNamespace("lme4", quietly = TRUE)) {
      stop("Package `lme4` is required for `mp_backend_lme4_nb()`.", call. = FALSE)
    }

    fit <- lme4::glmer.nb(
      formula = scenario$formula,
      data = data
    )

    attr(fit, "singular") <- lme4::isSingular(fit, tol = 1e-04)
    fit
  }

  test_fun <- function(fit, scenario) {
    method <- if (is.list(scenario$test)) scenario$test$method else NULL
    method <- `%||%`(method, test_method)

    if (identical(method, "wald")) {
      term <- if (is.list(scenario$test)) scenario$test$term else NULL
      term <- `%||%`(term, predictor)
      beta <- lme4::fixef(fit)[[term]]
      se <- sqrt(diag(stats::vcov(fit)))[[term]]
      z <- beta / se
      p_val <- 2 * stats::pnorm(abs(z), lower.tail = FALSE)
      return(list(p_value = as.numeric(p_val)))
    }

    if (identical(method, "lrt")) {
      null_formula_use <- if (is.list(scenario$test)) scenario$test$null_formula else NULL
      null_formula_use <- `%||%`(null_formula_use, null_formula)
      if (is.null(null_formula_use) || !inherits(null_formula_use, "formula")) {
        stop("`null_formula` must be supplied as a formula when `test_method = \"lrt\"`.", call. = FALSE)
      }

      null_fit <- stats::update(fit, formula = null_formula_use)
      lrt_tab <- stats::anova(null_fit, fit)
      p_col <- grep("Pr\\(>Chi", colnames(lrt_tab), value = TRUE)
      if (length(p_col) != 1L) {
        stop("Could not locate LRT p-value column in anova output.", call. = FALSE)
      }

      return(list(p_value = as.numeric(lrt_tab[2, p_col])))
    }

    stop("Unsupported `test_method`: ", method, call. = FALSE)
  }

  list(simulate_fun = simulate_fun, fit_fun = fit_fun, test_fun = test_fun)
}

#' Create a fully specified MixPower scenario with the NB lme4 backend
#' @param formula Model formula.
#' @param design A `mp_design` object.
#' @param assumptions A `mp_assumptions` object.
#' @param predictor Predictor column name.
#' @param subject Subject ID column name.
#' @param outcome Outcome column name.
#' @param item Optional item ID column name.
#' @param test_term Optional explicit term to test. Defaults to `predictor`.
#' @param test_method Inference method (`"wald"` or `"lrt"`).
#' @param null_formula Optional null model formula for `"lrt"` tests.
#' @return An object of class `mp_scenario`.
#' @export
mp_scenario_lme4_nb <- function(formula,
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
    stop("`null_formula` must be supplied as a formula when `test_method = \"lrt\"`.", call. = FALSE)
  }

  backend <- mp_backend_lme4_nb(
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
    test = list(term = test_term, method = test_method, null_formula = null_formula),
    simulate_fun = backend$simulate_fun,
    fit_fun = backend$fit_fun,
    test_fun = backend$test_fun
  )
}
