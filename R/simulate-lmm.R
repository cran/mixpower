#' Simulate Gaussian data for a linear mixed-effects design
#'
#' Thin wrapper over the shared simulation engine (`.mp_simulate_mixed`).
#' Random-effect sizes (intercept and optional slope on `predictor`) come from
#' `scenario$assumptions$random_effects`; an unspecified subject intercept SD
#' defaults to 1 so a `(1 | subject)` term is not degenerate.
#'
#' @param scenario An `mp_scenario` object.
#' @param seed Unused (seeding is handled by `mp_power()`); kept for signature
#'   compatibility.
#' @param outcome Outcome column name.
#' @param predictor Predictor column name.
#' @param subject Subject grouping-factor column name.
#' @param item Optional item grouping-factor column name.
#' @param re_subject_intercept_sd Optional override for the subject
#'   random-intercept SD (otherwise read from assumptions).
#' @param re_item_intercept_sd Optional override for the item random-intercept SD.
#' @return A data.frame with outcome and predictors.
#' @noRd
simulate_lmm_data <- function(scenario,
                              seed = NULL,
                              outcome = "y",
                              predictor = "condition",
                              subject = "subject",
                              item = NULL,
                              re_subject_intercept_sd = NULL,
                              re_item_intercept_sd = NULL) {
  overrides <- list()
  if (!is.null(re_subject_intercept_sd)) overrides[[subject]] <- re_subject_intercept_sd
  if (!is.null(item) && !is.null(re_item_intercept_sd)) overrides[[item]] <- re_item_intercept_sd

  .mp_simulate_mixed(
    scenario,
    family = "gaussian",
    predictor = predictor,
    subject = subject,
    outcome = outcome,
    item = item,
    intercept_default = 1,
    intercept_overrides = overrides
  )
}
