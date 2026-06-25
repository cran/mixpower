#' Simulate count outcome data for a Negative Binomial GLMM with random effects
#'
#' Thin wrapper over the shared simulation engine (`.mp_simulate_mixed`) with a
#' log link and negative-binomial response. Random-effect sizes (intercept and
#' optional slope on `predictor`) come from
#' `scenario$assumptions$random_effects`.
#'
#' @param scenario An `mp_scenario` object.
#' @param predictor Predictor column name.
#' @param subject Subject ID column name.
#' @param outcome Outcome column name.
#' @param item Optional item ID column name.
#' @param theta NB dispersion parameter (size); larger means less
#'   over-dispersion. Defaults to `scenario$assumptions$theta` or 1.
#' @return A data.frame with outcome and predictors.
#' @export
simulate_glmm_nb_data <- function(scenario,
                                  predictor = "condition",
                                  subject = "subject",
                                  outcome = "y",
                                  item = NULL,
                                  theta = NULL) {
  .mp_simulate_mixed(
    scenario,
    family = "nbinom",
    predictor = predictor,
    subject = subject,
    outcome = outcome,
    item = item,
    intercept_default = 0,
    theta = theta
  )
}
