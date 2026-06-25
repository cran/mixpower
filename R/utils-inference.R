#' Internal inference helpers
#' @noRd

# Inference methods available to GLMM (glmer/glmmTMB) backends. Satterthwaite
# and Kenward-Roger are linear-mixed-model-only and so excluded here.
.mp_glmm_methods <- c("wald", "lrt", "pb")

# Refit a mixed model with a given formula on an explicit data object, matching
# the original fit's engine/family. `stats::update()` is avoided because, inside
# the simulation loop, it re-evaluates `data` in an environment where the
# simulated data is not visible (silently yielding NA p-values).
.mp_refit_on <- function(fit, formula, data) {
  # Bind the data in the formula's environment under the name `data` so that
  # downstream re-simulation (e.g. pbkrtest::PBmodcomp) can recover it from the
  # fitted model's call without relying on the original simulation scope.
  fenv <- new.env(parent = environment(formula))
  fenv$data <- data
  environment(formula) <- fenv

  if (inherits(fit, "glmerMod")) {
    lme4::glmer(formula, data = data, family = stats::family(fit))
  } else if (inherits(fit, "lmerMod") || inherits(fit, "lmerModLmerTest")) {
    lme4::lmer(formula, data = data, REML = isTRUE(lme4::isREML(fit)))
  } else if (inherits(fit, "glmmTMB")) {
    glmmTMB::glmmTMB(formula, data = data, family = stats::family(fit))
  } else {
    stats::update(fit, formula. = formula, data = data)
  }
}

# Refit both the full and null models on the *same* data frame. Nested model
# comparison (anova / PBmodcomp) requires both models to share the data object.
.mp_refit_pair <- function(fit, null_formula) {
  mf <- stats::model.frame(fit)
  full <- .mp_refit_on(fit, stats::formula(fit), mf)
  null <- .mp_refit_on(fit, null_formula, mf)
  list(full = full, null = null)
}

# Refit a linear mixed model with REML (for Kenward-Roger), via lmerTest so the
# result carries the df machinery. Uses the model frame to avoid update() scope.
.mp_refit_reml_lmertest <- function(fit) {
  lmerTest::lmer(stats::formula(fit), data = stats::model.frame(fit), REML = TRUE)
}

# Two-sided Wald (normal-approximation) p-value for a single fixed-effect term.
#
# Robust to the object types returned by the supported backends:
#  * merMod (lme4): `coef(summary(fit))` is a numeric matrix with an
#    "Estimate" and "Std. Error" column. NOTE: `vcov(merMod)` is an S4
#    `dpoMatrix`, and `base::diag()` on it errors ("long vectors not
#    supported"); extracting from the coefficient table avoids that entirely.
#  * glmmTMB: `summary(fit)$coefficients` is a list with a `$cond` matrix.
#
# Returns NA_real_ when the term is absent or the SE is non-finite/non-positive,
# so mp_power() records it via its failure policy rather than erroring.
.mp_wald_p_value <- function(fit, term) {
  cf <- tryCatch(stats::coef(summary(fit)), error = function(e) NULL)
  if (is.list(cf) && !is.matrix(cf) && !is.null(cf$cond)) {
    cf <- cf$cond
  }
  if (is.null(cf) || !is.matrix(cf) || !term %in% rownames(cf)) {
    return(NA_real_)
  }
  if (!all(c("Estimate", "Std. Error") %in% colnames(cf))) {
    return(NA_real_)
  }
  est <- cf[term, "Estimate"]
  se <- cf[term, "Std. Error"]
  if (!is.finite(est) || !is.finite(se) || se <= 0) {
    return(NA_real_)
  }
  z <- est / se
  2 * stats::pnorm(abs(z), lower.tail = FALSE)
}

# df-corrected t-test p-value for a linear mixed model term, via lmerTest.
# `ddf` is "Satterthwaite" or "Kenward-Roger" (the latter also needs pbkrtest).
.mp_dfcorrect_p <- function(fit, term, ddf) {
  label <- if (identical(ddf, "Kenward-Roger")) "kenward-roger" else "satterthwaite"
  if (!requireNamespace("lmerTest", quietly = TRUE)) {
    .stop(sprintf("Package 'lmerTest' is required for test_method = '%s'.", label))
  }
  if (identical(ddf, "Kenward-Roger") && !requireNamespace("pbkrtest", quietly = TRUE)) {
    .stop("Package 'pbkrtest' is required for test_method = 'kenward-roger'.")
  }
  if (!inherits(fit, "lmerMod") && !inherits(fit, "lmerModLmerTest")) {
    .stop(sprintf(
      "test_method = '%s' applies to linear mixed models (lmer) only; use 'lrt' or 'pb' for GLMMs.",
      label
    ))
  }
  # Kenward-Roger is only defined for REML fits; the package fits with ML (for
  # LRT compatibility), so refit with REML for this method. Satterthwaite works
  # on the ML fit directly.
  ft <- tryCatch(
    if (identical(ddf, "Kenward-Roger") && !isTRUE(lme4::isREML(fit))) {
      .mp_refit_reml_lmertest(fit)
    } else if (inherits(fit, "lmerModLmerTest")) {
      fit
    } else {
      lmerTest::as_lmerModLmerTest(fit)
    },
    error = function(e) NULL
  )
  if (is.null(ft)) return(NA_real_)
  cf <- tryCatch(stats::coef(summary(ft, ddf = ddf)), error = function(e) NULL)
  if (is.null(cf) || !is.matrix(cf) || !term %in% rownames(cf) ||
      !"Pr(>|t|)" %in% colnames(cf)) {
    return(NA_real_)
  }
  as.numeric(cf[term, "Pr(>|t|)"])
}

# Likelihood-ratio test p-value comparing the fitted model to an explicit null.
.mp_lrt_p <- function(fit, null_formula) {
  if (is.null(null_formula) || !inherits(null_formula, "formula")) {
    .stop("`null_formula` must be provided as a formula when test_method = 'lrt'.")
  }
  pair <- tryCatch(.mp_refit_pair(fit, null_formula), error = function(e) NULL)
  if (is.null(pair)) return(NA_real_)
  tab <- tryCatch(suppressMessages(stats::anova(pair$null, pair$full)), error = function(e) NULL)
  if (is.null(tab)) return(NA_real_)
  p_col <- grep("Pr\\(>Chi", colnames(tab), value = TRUE)
  if (length(p_col) != 1L) return(NA_real_)
  as.numeric(tab[2, p_col])
}

# Parametric-bootstrap LRT p-value (pbkrtest). Works for LMMs and GLMMs but is
# expensive: it refits the model `nsim` times *per* power replicate.
.mp_pb_p <- function(fit, null_formula, nsim = 100L) {
  if (!requireNamespace("pbkrtest", quietly = TRUE)) {
    .stop("Package 'pbkrtest' is required for test_method = 'pb'.")
  }
  if (is.null(null_formula) || !inherits(null_formula, "formula")) {
    .stop("`null_formula` must be provided as a formula when test_method = 'pb'.")
  }
  pair <- tryCatch(.mp_refit_pair(fit, null_formula), error = function(e) NULL)
  if (is.null(pair)) return(NA_real_)
  res <- tryCatch(suppressMessages(pbkrtest::PBmodcomp(pair$full, pair$null, nsim = nsim)),
                  error = function(e) NULL)
  if (is.null(res)) return(NA_real_)
  tab <- res$test
  if (is.null(tab) || !"PBtest" %in% rownames(tab) || !"p.value" %in% colnames(tab)) {
    return(NA_real_)
  }
  as.numeric(tab["PBtest", "p.value"])
}

# Coefficient table for a fitted model, normalised to a numeric matrix with
# "Estimate"/"Std. Error" columns across the supported backends (NULL on error).
.mp_coef_table <- function(fit) {
  cf <- tryCatch(stats::coef(summary(fit)), error = function(e) NULL)
  if (is.list(cf) && !is.matrix(cf) && !is.null(cf$cond)) {
    cf <- cf$cond
  }
  if (is.null(cf) || !is.matrix(cf)) NULL else cf
}

# Point estimate of a fixed-effect term from a fitted model (NA if unavailable).
# Used for Type S / Type M error reporting.
.mp_fixef_estimate <- function(fit, term) {
  cf <- .mp_coef_table(fit)
  if (is.null(cf) || !term %in% rownames(cf) || !"Estimate" %in% colnames(cf)) {
    return(NA_real_)
  }
  as.numeric(cf[term, "Estimate"])
}

# Standard error of a fixed-effect term from a fitted model (NA if unavailable).
.mp_fixef_se <- function(fit, term) {
  cf <- .mp_coef_table(fit)
  if (is.null(cf) || !term %in% rownames(cf) || !"Std. Error" %in% colnames(cf)) {
    return(NA_real_)
  }
  as.numeric(cf[term, "Std. Error"])
}

# Joint (omnibus) Wald chi-square test that several fixed-effect coefficients
# are simultaneously zero: W = b' V^{-1} b ~ chi^2(df). lme4 fits only; returns
# NA when coefficients/covariance cannot be recovered.
.mp_wald_joint_p <- function(fit, terms) {
  b <- tryCatch(lme4::fixef(fit), error = function(e) NULL)
  v <- tryCatch(as.matrix(stats::vcov(fit)), error = function(e) NULL)
  if (is.null(b) || is.null(v) || !all(terms %in% names(b)) || !all(terms %in% rownames(v))) {
    return(NA_real_)
  }
  bb <- b[terms]
  vv <- v[terms, terms, drop = FALSE]
  w <- tryCatch(as.numeric(crossprod(bb, solve(vv, bb))), error = function(e) NA_real_)
  if (!is.finite(w) || w < 0) {
    return(NA_real_)
  }
  stats::pchisq(w, df = length(terms), lower.tail = FALSE)
}

# Wald test of a user-specified linear contrast L'beta = 0, where `contrast` is
# a named numeric vector of weights over fixed-effect coefficients (e.g. from
# emmeans). Returns p_value and the estimated contrast value. lme4 fits only.
.mp_contrast_test <- function(fit, contrast) {
  if (is.null(contrast) || !is.numeric(contrast) || is.null(names(contrast))) {
    .stop("`contrast` must be a named numeric vector of coefficient weights.")
  }
  b <- tryCatch(lme4::fixef(fit), error = function(e) NULL)
  v <- tryCatch(as.matrix(stats::vcov(fit)), error = function(e) NULL)
  if (is.null(b) || is.null(v) || !all(names(contrast) %in% names(b))) {
    return(list(p_value = NA_real_, estimate = NA_real_))
  }
  l <- stats::setNames(numeric(length(b)), names(b))
  l[names(contrast)] <- contrast
  est <- sum(l * b)
  var_l <- as.numeric(crossprod(l, v %*% l))
  if (!is.finite(var_l) || var_l <= 0) {
    return(list(p_value = NA_real_, estimate = est))
  }
  z <- est / sqrt(var_l)
  list(p_value = 2 * stats::pnorm(abs(z), lower.tail = FALSE), estimate = est)
}

# Single inference entry point used by every backend's test_fun.
.mp_p_value <- function(fit, term, method, null_formula = NULL, pb_nsim = 100L) {
  switch(method,
    wald = .mp_wald_p_value(fit, term),
    satterthwaite = .mp_dfcorrect_p(fit, term, "Satterthwaite"),
    "kenward-roger" = .mp_dfcorrect_p(fit, term, "Kenward-Roger"),
    lrt = .mp_lrt_p(fit, null_formula),
    pb = .mp_pb_p(fit, null_formula, pb_nsim),
    .stop(sprintf("Unsupported test method: %s", method))
  )
}

# Resolve method/term/null_formula/pb_nsim from scenario$test (falling back to
# backend defaults) and return list(p_value = ...). Centralizes inference so the
# backends do not duplicate the dispatch logic.
.mp_dispatch_test <- function(fit, scenario, predictor, default_method,
                              default_null = NULL, default_pb_nsim = 100L) {
  test <- scenario$test
  is_list <- is.list(test)
  method <- if (is_list) `%||%`(test$method, default_method) else default_method
  term <- if (is_list) `%||%`(test$term, predictor) else predictor
  null_f <- if (is_list) `%||%`(test$null_formula, default_null) else default_null
  pb_nsim <- if (is_list) `%||%`(test$pb_nsim, default_pb_nsim) else default_pb_nsim
  contrast <- if (is_list) test$contrast else NULL

  # Custom linear contrast (e.g. emmeans-style weights).
  if (!is.null(contrast)) {
    return(.mp_contrast_test(fit, contrast))
  }
  # Omnibus / multi-degree-of-freedom test of several coefficients at once.
  if (length(term) > 1L) {
    p <- if (method %in% c("lrt", "pb")) {
      .mp_p_value(fit, term[[1]], method, null_f, pb_nsim) # null_formula defines the joint test
    } else {
      .mp_wald_joint_p(fit, term)
    }
    return(list(p_value = p, estimate = NA_real_))
  }
  list(
    p_value = .mp_p_value(fit, term, method, null_f, pb_nsim),
    estimate = .mp_fixef_estimate(fit, term)
  )
}

# Shared constructor-time validation: match the method against the methods a
# backend allows, and require a null model formula for likelihood-based tests.
.mp_resolve_test_method <- function(test_method, null_formula, allowed) {
  method <- match.arg(test_method, choices = allowed)
  if (method %in% c("lrt", "pb") &&
      (is.null(null_formula) || !inherits(null_formula, "formula"))) {
    .stop(sprintf("`null_formula` must be provided as a formula when test_method = '%s'.", method))
  }
  method
}
