#' Build a power scenario from a fitted lme4 model
#'
#' Turns an existing `lmer`/`glmer` fit (e.g. from pilot or published data) into
#' an `mp_scenario`, so its estimated effects and variance components inform a
#' simulation-based power analysis. New responses are simulated from the fitted
#' model with [stats::simulate()] (keeping the estimated random-effect structure
#' and residual variance), the model is refit, and the focal term is tested.
#'
#' The fixed effects used to simulate are read from the scenario's assumptions,
#' which start at the fitted coefficients. This means `mp_sensitivity()` /
#' `mp_power_curve()` can vary an effect size (e.g.
#' `fixed_effects.condition`) for data-based vs smallest-effect-of-interest
#' comparisons. Sample size can be scaled up or down from the pilot with
#' [mp_extend()] (or the `extend.<group>` sensitivity key), which clones the
#' pilot's structure with fresh levels and fresh random effects.
#'
#' @param fit A fitted model of class `lmerMod`/`lmerModLmerTest` (Gaussian LMM)
#'   or `glmerMod` (binomial/Poisson/negative-binomial GLMM).
#' @param test_term Fixed-effect term to test. Defaults to the first
#'   non-intercept fixed effect.
#' @param test_method Inference method. Gaussian fits allow `"wald"` (default),
#'   `"satterthwaite"`, `"kenward-roger"`, `"lrt"`, `"pb"`; GLMM fits allow
#'   `"wald"`, `"lrt"`, `"pb"`.
#' @param null_formula Null-model formula required for `"lrt"`/`"pb"`. Defaults
#'   to the fitted formula with `test_term` removed.
#' @param pb_nsim Bootstrap replicates for `test_method = "pb"` (default 100).
#' @param extend Optional named list of target level counts per grouping factor
#'   (e.g. `list(Subject = 60)`) used to scale the pilot's sample size up or
#'   down. Levels are cloned from the pilot's structure with fresh ids and fresh
#'   random effects drawn from the fitted covariance. See [mp_extend()] and the
#'   `extend.<group>` sensitivity key for power curves over N.
#' @return An object of class `mp_scenario`.
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   m <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
#'   scn <- mp_from_fit(m, test_term = "Days")
#'   mp_power(scn, nsim = 20, seed = 1)
#' }
#' }
mp_from_fit <- function(fit,
                        test_term = NULL,
                        test_method = NULL,
                        null_formula = NULL,
                        pb_nsim = 100L,
                        extend = NULL) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package `lme4` is required for `mp_from_fit()`.", call. = FALSE)
  }
  if (!inherits(fit, c("lmerMod", "lmerModLmerTest", "glmerMod"))) {
    stop("`mp_from_fit()` currently supports lme4 fits (lmer / glmer).", call. = FALSE)
  }

  is_gaussian <- !inherits(fit, "glmerMod")
  ff <- stats::formula(fit)
  fixed <- lme4::fixef(fit)
  term <- `%||%`(test_term, .mp_default_test_term(fixed))
  if (!term %in% names(fixed)) {
    stop(sprintf("`test_term` '%s' is not a fixed effect in the model.", term), call. = FALSE)
  }

  grouping <- names(lme4::ngrps(fit))
  extend <- .mp_validate_extend(extend, grouping)

  allowed <- if (is_gaussian) .mp_lme4_methods else .mp_glmm_methods
  method <- `%||%`(test_method, "wald")
  if (is.null(null_formula) && method %in% c("lrt", "pb")) {
    null_formula <- stats::update.formula(ff, stats::as.formula(paste0(". ~ . - ", term)))
  }
  method <- .mp_resolve_test_method(method, null_formula, allowed)

  fam <- if (is_gaussian) NULL else stats::family(fit)
  reml <- isTRUE(lme4::isREML(fit))
  resp <- names(stats::model.frame(fit))[1]

  simulate_fun <- function(scenario, seed = NULL) {
    beta <- .mp_assumptions_beta(scenario$assumptions, names(fixed), fixed)
    ext <- scenario$extend
    dat <- stats::model.frame(fit)
    if (!is.null(ext) && length(ext) > 0L) {
      for (g in names(ext)) dat <- .mp_extend_frame(dat, g, as.integer(ext[[g]]))
      # Pass the fitted variance components explicitly (alongside beta) so lme4
      # does not warn about unspecified params on the new data.
      np <- list(beta = beta, theta = lme4::getME(fit, "theta"))
      if (is_gaussian) np$sigma <- stats::sigma(fit)
      ysim <- stats::simulate(fit, nsim = 1, newparams = np,
                              newdata = dat, allow.new.levels = TRUE)[[1]]
    } else {
      ysim <- stats::simulate(fit, nsim = 1, newparams = list(beta = beta))[[1]]
    }
    dat[[resp]] <- ysim
    dat
  }

  fit_fun <- function(data, scenario) {
    newfit <- if (is_gaussian) {
      if (requireNamespace("lmerTest", quietly = TRUE)) {
        lmerTest::lmer(ff, data = data, REML = reml)
      } else {
        lme4::lmer(ff, data = data, REML = reml)
      }
    } else {
      lme4::glmer(ff, data = data, family = fam)
    }
    attr(newfit, "singular") <- tryCatch(isTRUE(lme4::isSingular(newfit, tol = 1e-4)),
                                         error = function(e) NA)
    newfit
  }

  test_fun <- function(model, scenario) {
    .mp_dispatch_test(model, scenario, term, method, null_formula, pb_nsim)
  }

  backend <- mp_backend(
    simulate_fun = simulate_fun,
    fit_fun = fit_fun,
    test_fun = test_fun,
    name = if (is_gaussian) "from_fit:lmer" else "from_fit:glmer",
    capabilities = list(
      families = if (is_gaussian) "gaussian" else fam$family,
      test_methods = allowed,
      source = "fitted_model"
    )
  )

  assumptions <- mp_assumptions(
    fixed_effects = as.list(fixed),
    random_effects = .mp_extract_varcorr(fit),
    residual_sd = if (is_gaussian) stats::sigma(fit) else NULL
  )
  design <- .mp_design_from_fit(fit)

  scn <- mp_scenario(
    formula = ff,
    design = design,
    assumptions = assumptions,
    test = list(term = term, method = method,
                null_formula = null_formula, pb_nsim = pb_nsim),
    simulate_fun = backend$simulate_fun,
    fit_fun = backend$fit_fun,
    test_fun = backend$test_fun
  )
  # Slots that power the extend()-style N-scaling (see mp_extend()). The slot is
  # named `grouping_factors` (not `extendable_*`) so that `$extend` does not
  # partial-match it under R's `$` prefix matching.
  scn$grouping_factors <- grouping
  scn$extend <- extend
  scn
}

# First non-intercept fixed-effect name.
.mp_default_test_term <- function(fixed) {
  nm <- setdiff(names(fixed), "(Intercept)")
  if (length(nm) == 0L) {
    .stop("Model has no non-intercept fixed effect to test; supply `test_term`.")
  }
  nm[[1]]
}

# Build the fixed-effect vector (in the model's coefficient order) from the
# scenario assumptions, defaulting to the fitted values. Lets sensitivity over
# `fixed_effects.<term>` change the simulated effect size.
.mp_assumptions_beta <- function(assumptions, beta_names, default_beta) {
  vapply(beta_names, function(nm) {
    v <- assumptions$fixed_effects[[nm]]
    if (is.null(v) || !is.numeric(v) || length(v) != 1L) {
      as.numeric(default_beta[[nm]])
    } else {
      as.numeric(v)
    }
  }, numeric(1))
}

# Random-effect SDs and correlations per grouping factor, for the scenario's
# assumptions metadata. Captures the random intercept, every random slope, and
# their correlation structure (a scalar for the single intercept-slope case,
# otherwise the full correlation matrix over c("(Intercept)", slopes)).
.mp_extract_varcorr <- function(fit) {
  vc <- lme4::VarCorr(fit)
  out <- list()
  for (g in names(vc)) {
    m <- vc[[g]]
    sds <- sqrt(diag(m))
    nm <- names(sds)
    has_int <- "(Intercept)" %in% nm
    re <- list(intercept_sd = if (has_int) unname(sds[["(Intercept)"]]) else 0)
    slope_terms <- setdiff(nm, "(Intercept)")
    if (length(slope_terms) >= 1L) {
      re$slopes <- stats::setNames(as.list(unname(sds[slope_terms])), slope_terms)
      decl <- c("(Intercept)", slope_terms)
      cc <- suppressWarnings(stats::cov2cor(m))
      R <- diag(1, length(decl))
      dimnames(R) <- list(decl, decl)
      common <- intersect(decl, rownames(cc))
      R[common, common] <- cc[common, common]
      R[!is.finite(R)] <- 0
      diag(R) <- 1
      re$cor <- if (has_int && length(slope_terms) == 1L) unname(R["(Intercept)", slope_terms]) else R
    }
    out[[g]] <- re
  }
  if (length(out) == 0L) NULL else out
}

# Descriptive design (group sizes) for a fitted model.
.mp_design_from_fit <- function(fit) {
  ng <- lme4::ngrps(fit)
  clusters <- as.list(as.integer(ng))
  names(clusters) <- names(ng)
  tpc <- max(1L, as.integer(stats::nobs(fit) %/% max(ng)))
  mp_design(clusters = clusters, trials_per_cell = tpc)
}
