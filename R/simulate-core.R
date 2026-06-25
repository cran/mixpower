#' Unified data-generating engine for mixed-model power simulation
#' @noRd

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Draw a block of correlated random effects for one grouping factor.
#
# `sds` is a named vector of standard deviations over the declared terms
# (`(Intercept)`, then slope predictors); `R` is the matching correlation
# matrix. Returns an `n_levels` x `n_active_terms` matrix (columns named by the
# terms with positive SD), or NULL when no term has positive SD.
#
# A single active term reduces to one rnorm() draw (matching the historical
# intercept-only behaviour); multiple terms use a multivariate normal via the
# Cholesky factor (or an eigen square root for singular targets), so no
# MASS/mvtnorm dependency is needed.
.mp_draw_re_block <- function(n_levels, sds, R) {
  active <- which(sds > 0)
  if (length(active) == 0L) {
    return(NULL)
  }
  sds_a <- sds[active]
  terms_a <- names(sds_a)

  if (length(active) == 1L) {
    b <- stats::rnorm(n_levels, 0, sds_a[[1]])
    return(matrix(b, ncol = 1L, dimnames = list(NULL, terms_a)))
  }

  R_a <- R[active, active, drop = FALSE]
  d <- diag(sds_a, nrow = length(sds_a))
  sigma <- d %*% R_a %*% d
  z <- matrix(stats::rnorm(length(active) * n_levels), nrow = n_levels, ncol = length(active))
  r <- tryCatch(chol(sigma), error = function(e) NULL)
  b <- if (is.null(r)) {
    # Singular target (e.g. |cor| = 1): symmetric square root via eigen.
    e <- eigen(sigma, symmetric = TRUE)
    z %*% (e$vectors %*% diag(sqrt(pmax(e$values, 0)), nrow = length(e$values)) %*% t(e$vectors))
  } else {
    z %*% r # cov(b) = r'r = sigma
  }
  colnames(b) <- terms_a
  b
}

# Add a grouping factor's random-effect contribution to the linear predictor.
.mp_add_group_re <- function(assumptions, group, id, X, n_levels,
                             intercept_default, intercept_override) {
  spec <- .mp_re_block_spec(
    assumptions, group,
    available = colnames(X),
    intercept_default = intercept_default,
    intercept_override = intercept_override
  )
  b <- .mp_draw_re_block(n_levels, spec$sds, spec$R)
  if (is.null(b)) {
    return(0)
  }
  contrib <- numeric(length(id))
  for (tm in colnames(b)) {
    draws <- b[id, tm]
    contrib <- contrib + if (tm == "(Intercept)") draws else draws * X[, tm]
  }
  contrib
}

# Family-specific response given the linear predictor `eta`.
.mp_family_outcome <- function(family, eta, assumptions, theta = NULL) {
  n <- length(eta)
  switch(family,
    gaussian = {
      resid_sd <- `%||%`(assumptions$residual_sd, 1)
      .assert_is_nonneg_num(resid_sd, "residual_sd")
      eta + stats::rnorm(n, mean = 0, sd = resid_sd)
    },
    binomial = {
      p <- pmin(pmax(stats::plogis(eta), 1e-6), 1 - 1e-6)
      stats::rbinom(n, size = 1, prob = p)
    },
    poisson = stats::rpois(n, lambda = exp(eta)),
    nbinom = {
      th <- `%||%`(theta, `%||%`(assumptions$theta, 1))
      if (!is.numeric(th) || length(th) != 1L || is.na(th) || th <= 0) {
        .stop("`theta` must be a positive numeric value.")
      }
      stats::rnbinom(n, size = th, mu = exp(eta))
    },
    .stop(sprintf("Unsupported family: %s", family))
  )
}

# Resolve a predictor's design spec (type in {binary, continuous}, level in
# {within, between}) from `design$predictors`. Unspecified -> binary/within,
# the historical default.
.mp_resolve_predictor_spec <- function(name, design_predictors) {
  s <- if (is.null(design_predictors)) NULL else design_predictors[[name]]
  if (is.null(s)) {
    return(list(type = "within_binary", continuous = FALSE, between = FALSE))
  }
  if (is.character(s)) s <- list(type = s)
  type <- match.arg(`%||%`(s$type, "binary"), c("binary", "continuous"))
  level <- match.arg(`%||%`(s$level, "within"), c("within", "between"))
  list(continuous = identical(type, "continuous"), between = identical(level, "between"))
}

# Build one predictor column given its spec. Deterministic (no RNG), so the
# design matrix is fixed across replicates and the default binary/within case is
# byte-identical to the historical 0,1,0,1,... binary counter.
#   * binary  / within  : bit `bit_index` of the within-subject position.
#   * continuous / within: the within-subject position (time-like 0..t-1).
#   * binary  / between : alternating 0/1 across subjects (balanced).
#   * continuous / between: standard-normal quantiles across subjects (balanced).
.mp_make_predictor_column <- function(spec, pos, subject_id, n_subject, bit_index) {
  if (spec$between) {
    base <- if (spec$continuous) {
      stats::qnorm(stats::ppoints(n_subject))
    } else {
      rep(c(0, 1), length.out = n_subject)
    }
    return(base[subject_id])
  }
  if (spec$continuous) {
    return(as.numeric(pos))
  }
  as.numeric(bitwAnd(bitwShiftR(pos, bit_index), 1L))
}

# Single data-generating process shared by every backend. Each non-intercept
# entry of `fixed_effects` becomes a design predictor (binary/continuous,
# within/between per `design$predictors`); binary-within predictors are crossed
# via a binary counter. Subjects may be nested in a higher grouping factor
# (`design$nesting`) for three-level designs, and `trials_per_cell` may be a
# vector for unbalanced within-subject sample sizes. Correlated random effects
# (intercept + any declared slopes) are added per grouping factor before the
# family-specific response is drawn.
#
# `intercept_default` is the random-intercept SD used when neither
# `random_effects` nor `icc` specify one (1 for Gaussian so a `(1 | g)` term is
# not degenerate; 0 for GLMMs). `intercept_overrides` lets a backend force a
# group's intercept SD (used by the deprecated explicit `simulate_lmm_data`
# arguments).
.mp_simulate_mixed <- function(scenario,
                               family = c("gaussian", "binomial", "poisson", "nbinom"),
                               predictor = "condition",
                               subject = "subject",
                               outcome = "y",
                               item = NULL,
                               intercept_default = 0,
                               intercept_overrides = list(),
                               theta = NULL) {
  family <- match.arg(family)
  .assert_class(scenario, "mp_scenario", "scenario")

  des <- scenario$design
  asm <- scenario$assumptions

  trials <- des$trials_per_cell
  if (is.null(des$clusters[[subject]]) || is.null(trials)) {
    .stop(sprintf("Design must include clusters$%s and trials_per_cell.", subject))
  }

  # Optional nesting: subject may sit inside a higher grouping factor. When it
  # does, clusters[[subject]] is read as the number of subjects *per parent*.
  parent <- if (!is.null(des$nesting) && subject %in% names(des$nesting)) {
    unname(des$nesting[[subject]])
  } else {
    NULL
  }
  if (is.null(parent)) {
    n_subject <- des$clusters[[subject]]
    subj_parent <- NULL
    n_parent <- NULL
  } else {
    n_parent <- des$clusters[[parent]]
    if (is.null(n_parent)) {
      .stop(sprintf("Design must include clusters$%s (nesting parent of %s).", parent, subject))
    }
    n_subject <- n_parent * des$clusters[[subject]]
    subj_parent <- rep(seq_len(n_parent), each = des$clusters[[subject]])
  }

  # Per-subject trial counts (scalar or recycled vector -> unbalanced designs).
  trials_vec <- if (length(trials) == 1L) rep(trials, n_subject) else rep_len(trials, n_subject)
  total_n <- sum(trials_vec)
  subject_id <- rep(seq_len(n_subject), times = trials_vec)
  pos <- unlist(lapply(trials_vec, function(t) seq_len(t) - 1L), use.names = FALSE)

  fe <- asm$fixed_effects
  predictors <- setdiff(names(fe), "(Intercept)")
  if (!(predictor %in% predictors)) {
    .stop(sprintf("Assumptions must include a numeric scalar fixed_effects$%s.", predictor))
  }
  for (p in predictors) {
    v <- fe[[p]]
    if (!is.numeric(v) || length(v) != 1L || is.na(v)) {
      .stop(sprintf("`fixed_effects$%s` must be a numeric scalar.", p))
    }
  }
  beta0 <- `%||%`(fe[["(Intercept)"]], 0)

  X <- matrix(0, nrow = total_n, ncol = length(predictors),
              dimnames = list(NULL, predictors))
  bit_index <- 0L
  for (p in predictors) {
    spec <- .mp_resolve_predictor_spec(p, des$predictors)
    X[, p] <- .mp_make_predictor_column(spec, pos, subject_id, n_subject, bit_index)
    if (!spec$between && !spec$continuous) bit_index <- bit_index + 1L
  }

  dat <- data.frame(.id = subject_id, stringsAsFactors = FALSE)
  names(dat) <- subject
  for (p in predictors) dat[[p]] <- X[, p]

  beta_vec <- vapply(predictors, function(p) as.numeric(fe[[p]]), numeric(1))
  eta <- beta0 + as.vector(X %*% beta_vec)

  eta <- eta + .mp_add_group_re(
    asm, subject, subject_id, X, n_subject,
    intercept_default = intercept_default,
    intercept_override = intercept_overrides[[subject]]
  )

  if (!is.null(parent)) {
    parent_obs <- subj_parent[subject_id]
    eta <- eta + .mp_add_group_re(
      asm, parent, parent_obs, X, n_parent,
      intercept_default = intercept_default,
      intercept_override = intercept_overrides[[parent]]
    )
    dat[[parent]] <- factor(parent_obs)
  }

  if (!is.null(item)) {
    n_item <- des$clusters[[item]]
    if (is.null(n_item)) {
      .stop(sprintf("Design must include clusters$%s when item is specified.", item))
    }
    item_id <- rep(seq_len(n_item), length.out = total_n)
    eta <- eta + .mp_add_group_re(
      asm, item, item_id, X, n_item,
      intercept_default = intercept_default,
      intercept_override = intercept_overrides[[item]]
    )
    dat[[item]] <- factor(item_id)
  }

  dat[[outcome]] <- .mp_family_outcome(family, eta, asm, theta)
  dat[[subject]] <- factor(dat[[subject]])
  dat
}
