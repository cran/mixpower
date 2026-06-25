#' Simulate binary outcome data for a GLMM with random effects
#'
#' Thin wrapper over the shared simulation engine (`.mp_simulate_mixed`) with a
#' logit link and Bernoulli response. Random-effect sizes (intercept and
#' optional slope on `predictor`) come from
#' `scenario$assumptions$random_effects`.
#'
#' @param scenario An `mp_scenario` object.
#' @param predictor Predictor column name.
#' @param subject Subject ID column name.
#' @param outcome Outcome column name.
#' @param item Optional item ID column name.
#' @return A data.frame with outcome and predictors.
#' @export
simulate_glmm_binomial_data <- function(scenario,
                                        predictor = "condition",
                                        subject = "subject",
                                        outcome = "y",
                                        item = NULL) {
  .mp_simulate_mixed(
    scenario,
    family = "binomial",
    predictor = predictor,
    subject = subject,
    outcome = outcome,
    item = item,
    intercept_default = 0
  )
}
