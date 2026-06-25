#' Effect-size converters for eliciting assumptions
#'
#' Helpers that translate standardized or published effect sizes into the raw
#' coefficients and variance components that [mp_assumptions()] expects, so you
#' do not have to hand-pick regression coefficients. They compose directly:
#'
#' ```
#' mp_assumptions(
#'   fixed_effects = list("(Intercept)" = 0, condition = mp_d_to_beta(0.5, sd = 1.12)),
#'   random_effects = list(subject = list(intercept_sd = mp_icc_to_sd(0.1, 1))),
#'   residual_sd = 1
#' )
#' ```
#'
#' @details
#' * `mp_d_to_beta()` / `mp_beta_to_d()`: Cohen's d for a 0/1 predictor is the
#'   mean difference divided by `sd`, so the coefficient is `d * sd`.
#' * `mp_r2_to_beta()` / `mp_beta_to_r2()`: for a predictor with SD
#'   `predictor_sd`, a target (partial) variance-explained `r2` against residual
#'   SD `sd_resid` implies `beta = sqrt(r2/(1-r2)) * sd_resid / predictor_sd`.
#' * `mp_icc_to_sd()` / `mp_sd_to_icc()`: an intraclass correlation `icc` with
#'   residual SD `sd_resid` implies a random-intercept SD
#'   `sqrt(icc/(1-icc)) * sd_resid`.
#' * `mp_or_to_logodds()` / `mp_logodds_to_or()`: a binomial GLMM coefficient is
#'   the log odds ratio.
#' * `mp_t_to_beta()` / `mp_f_to_beta()`: recover a coefficient from a published
#'   t (or one-numerator-df F) statistic and its standard error.
#'
#' @param d Cohen's d (standardized mean difference).
#' @param beta A raw coefficient.
#' @param sd Standard deviation defining the standardization (e.g. the total
#'   outcome SD `sqrt(tau^2 + sigma^2)`).
#' @param r2 Target (partial) proportion of variance explained, in `[0, 1)`.
#' @param sd_resid Residual standard deviation.
#' @param predictor_sd Standard deviation of the predictor (default 1).
#' @param icc Target intraclass correlation, in `[0, 1)`.
#' @param or Odds ratio (> 0).
#' @param t A t statistic.
#' @param f An F statistic with one numerator degree of freedom.
#' @param se Standard error of the coefficient.
#' @name effect_size
#' @return A numeric scalar.
#' @examples
#' mp_d_to_beta(0.5, sd = 1.12)
#' mp_icc_to_sd(0.1, sd_resid = 1)
#' mp_r2_to_beta(0.02, sd_resid = 1)
#' mp_or_to_logodds(1.5)
NULL

.mp_chk_num <- function(x, name, lo = -Inf, hi = Inf, inclusive_hi = TRUE) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x)) {
    .stop(sprintf("`%s` must be a single numeric value.", name))
  }
  too_hi <- if (inclusive_hi) x > hi else x >= hi
  if (x < lo || too_hi) {
    .stop(sprintf("`%s` is out of range.", name))
  }
  invisible(x)
}

#' @rdname effect_size
#' @export
mp_d_to_beta <- function(d, sd = 1) {
  .mp_chk_num(d, "d")
  .mp_chk_num(sd, "sd", lo = 0)
  d * sd
}

#' @rdname effect_size
#' @export
mp_beta_to_d <- function(beta, sd = 1) {
  .mp_chk_num(beta, "beta")
  .mp_chk_num(sd, "sd", lo = 0)
  if (sd == 0) .stop("`sd` must be > 0.")
  beta / sd
}

#' @rdname effect_size
#' @export
mp_r2_to_beta <- function(r2, sd_resid = 1, predictor_sd = 1) {
  .mp_chk_num(r2, "r2", lo = 0, hi = 1, inclusive_hi = FALSE)
  .mp_chk_num(sd_resid, "sd_resid", lo = 0)
  .mp_chk_num(predictor_sd, "predictor_sd", lo = 0)
  if (predictor_sd == 0) .stop("`predictor_sd` must be > 0.")
  sqrt(r2 / (1 - r2)) * sd_resid / predictor_sd
}

#' @rdname effect_size
#' @export
mp_beta_to_r2 <- function(beta, sd_resid = 1, predictor_sd = 1) {
  .mp_chk_num(beta, "beta")
  .mp_chk_num(sd_resid, "sd_resid", lo = 0)
  .mp_chk_num(predictor_sd, "predictor_sd", lo = 0)
  num <- (beta * predictor_sd)^2
  num / (num + sd_resid^2)
}

#' @rdname effect_size
#' @export
mp_icc_to_sd <- function(icc, sd_resid = 1) {
  .mp_chk_num(icc, "icc", lo = 0, hi = 1, inclusive_hi = FALSE)
  .mp_chk_num(sd_resid, "sd_resid", lo = 0)
  sqrt(icc / (1 - icc)) * sd_resid
}

#' @rdname effect_size
#' @export
mp_sd_to_icc <- function(sd, sd_resid = 1) {
  .mp_chk_num(sd, "sd", lo = 0)
  .mp_chk_num(sd_resid, "sd_resid", lo = 0)
  sd^2 / (sd^2 + sd_resid^2)
}

#' @rdname effect_size
#' @export
mp_or_to_logodds <- function(or) {
  .mp_chk_num(or, "or", lo = 0, inclusive_hi = TRUE)
  if (or <= 0) .stop("`or` must be > 0.")
  log(or)
}

#' @rdname effect_size
#' @export
mp_logodds_to_or <- function(beta) {
  .mp_chk_num(beta, "beta")
  exp(beta)
}

#' @rdname effect_size
#' @export
mp_t_to_beta <- function(t, se) {
  .mp_chk_num(t, "t")
  .mp_chk_num(se, "se", lo = 0)
  t * se
}

#' @rdname effect_size
#' @export
mp_f_to_beta <- function(f, se) {
  .mp_chk_num(f, "f", lo = 0)
  .mp_chk_num(se, "se", lo = 0)
  sqrt(f) * se
}
