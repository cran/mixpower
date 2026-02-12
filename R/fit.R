#' Fit a model for a single simulated dataset
#' @param data A data.frame of simulated data.
#' @param formula A model formula.
#' @return A fitted model object.
#' @export
fit_model <- function(data, formula) {
  stats::lm(formula = formula, data = data)
}
