#' Extract a test statistic for a model term
#' @param fit A fitted model object.
#' @param term Term name to test.
#' @return A data.frame with coefficient information.
#' @export
test_effect <- function(fit, term) {
  coefficients <- summary(fit)$coefficients
  data.frame(term = term, coefficients[term, , drop = FALSE])
}
