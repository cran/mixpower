#' Internal helpers for power confidence intervals and Type S/M error rates
#' @noRd

# Confidence interval for a power estimate from `x` detections out of `n`.
# "clopper-pearson" (default) is the exact binomial interval (well-behaved at
# 0 and 1); "wald" is the normal approximation.
.mp_power_ci <- function(x, n, conf_level, method = c("clopper-pearson", "wald")) {
  method <- match.arg(method)
  if (is.na(x) || is.na(n) || n <= 0) {
    return(c(NA_real_, NA_real_))
  }
  a <- 1 - conf_level
  if (identical(method, "wald")) {
    p <- x / n
    se <- sqrt(p * (1 - p) / n)
    z <- stats::qnorm(1 - a / 2)
    return(c(max(0, p - z * se), min(1, p + z * se)))
  }
  lo <- if (x == 0) 0 else stats::qbeta(a / 2, x, n - x + 1)
  hi <- if (x == n) 1 else stats::qbeta(1 - a / 2, x + 1, n - x)
  c(lo, hi)
}

# True (data-generating) value of the tested fixed effect, or NA if unknown.
# Type S/M are only defined for a single scalar term, so omnibus (vector) terms
# and custom contrasts return NA.
.mp_true_effect <- function(scenario) {
  if (!is.list(scenario$test) || !is.null(scenario$test$contrast)) return(NA_real_)
  term <- scenario$test$term
  if (is.null(term) || length(term) != 1L) return(NA_real_)
  fe <- scenario$assumptions$fixed_effects[[term]]
  if (is.null(fe) || !is.numeric(fe) || length(fe) != 1L) return(NA_real_)
  as.numeric(fe)
}

# Type S (wrong-sign) and Type M (exaggeration ratio) errors among the
# *significant* replicates (Gelman & Carlin, 2014). `sig_estimates` are the
# effect estimates from replicates that reached significance.
.mp_type_sm <- function(sig_estimates, true_effect) {
  sig_estimates <- sig_estimates[is.finite(sig_estimates)]
  if (is.na(true_effect) || true_effect == 0 || length(sig_estimates) == 0L) {
    return(list(type_s = NA_real_, type_m = NA_real_))
  }
  list(
    type_s = mean(sign(sig_estimates) != sign(true_effect)),
    type_m = mean(abs(sig_estimates) / abs(true_effect))
  )
}
