#' Internal helpers for the canonical random-effects specification
#' @noRd

# Canonical form stored on an `mp_assumptions` object:
#   assumptions$random_effects = list(
#     subject = list(
#       intercept_sd = <non-negative numeric scalar>,
#       slopes       = list(<predictor> = <non-negative numeric scalar>, ...),  # optional, one or more
#       cor          = <scalar in [-1, 1]>  OR  <correlation matrix>            # optional
#     ),
#     item = list(intercept_sd = ...)
#   )
#
# SDs are on the linear-predictor scale (what lme4 reports). One or more
# correlated random slopes per grouping factor are supported. The correlation
# `cor` may be:
#   * a single scalar, applied to every pair of random-effect terms
#     (compound symmetric), generalising the intercept-slope correlation of the
#     single-slope case; or
#   * a full correlation matrix over the declared terms, ordered
#     `c("(Intercept)", names(slopes))`.
#
# The legacy `icc` field (a named list interpreted, incorrectly, as a
# random-intercept SD) is still honoured as a fallback so that code written
# against mixpower <= 0.1.0 keeps working. New code should use
# `random_effects`. Resolution order: `random_effects` first, then legacy
# `icc`, then the supplied default.

.mp_re_intercept_sd <- function(assumptions, group, default = NULL) {
  re <- assumptions$random_effects
  if (!is.null(re) && !is.null(re[[group]]) && !is.null(re[[group]]$intercept_sd)) {
    return(re[[group]]$intercept_sd)
  }
  if (!is.null(assumptions$icc) && !is.null(assumptions$icc[[group]])) {
    return(assumptions$icc[[group]])
  }
  default
}

# SD of the random slope on `predictor` for `group` (0 if none specified).
.mp_re_slope_sd <- function(assumptions, group, predictor) {
  slopes <- assumptions$random_effects[[group]]$slopes
  if (is.null(slopes) || is.null(slopes[[predictor]])) {
    return(0)
  }
  slopes[[predictor]]
}

# Correlation specification for `group` (scalar or matrix; 0 if unspecified).
.mp_re_cor <- function(assumptions, group) {
  `%||%`(assumptions$random_effects[[group]]$cor, 0)
}

# Build a correlation matrix over `term_names` from a `cor` specification:
#   * NULL   -> identity (independent terms)
#   * scalar -> compound symmetric (unit diagonal, off-diagonal = cor)
#   * matrix -> used as supplied (diagonal forced to 1)
.mp_cor_matrix <- function(cor_spec, term_names) {
  q <- length(term_names)
  if (is.null(cor_spec)) {
    R <- diag(1, q)
  } else if (is.matrix(cor_spec)) {
    R <- cor_spec
    diag(R) <- 1
  } else {
    R <- matrix(cor_spec, q, q)
    diag(R) <- 1
  }
  dimnames(R) <- list(term_names, term_names)
  R
}

# Resolve the random-effect block for one grouping factor into a named SD
# vector and correlation matrix over the declared terms
# (`c("(Intercept)", names(slopes))`). `available` is the set of predictor
# columns in the design; slopes must reference one of them.
.mp_re_block_spec <- function(assumptions, group, available = NULL,
                              intercept_default = 0, intercept_override = NULL) {
  spec <- assumptions$random_effects[[group]]
  intercept_sd <- `%||%`(
    intercept_override,
    .mp_re_intercept_sd(assumptions, group, default = intercept_default)
  )
  intercept_sd <- `%||%`(intercept_sd, 0)

  slopes <- if (is.null(spec)) NULL else spec$slopes
  term_names <- "(Intercept)"
  sds <- intercept_sd
  if (!is.null(slopes) && length(slopes) > 0L) {
    for (p in names(slopes)) {
      if (!is.null(available) && !(p %in% available)) {
        .stop(sprintf(
          "random_effects$%s slope '%s' is not among the design predictors (%s).",
          group, p, paste(available, collapse = ", ")
        ))
      }
      term_names <- c(term_names, p)
      sds <- c(sds, slopes[[p]])
    }
  }
  names(sds) <- term_names
  cor_spec <- if (is.null(spec)) NULL else spec$cor
  list(sds = sds, R = .mp_cor_matrix(cor_spec, term_names), terms = term_names)
}

# Validate a user-supplied `random_effects` list. Supports `intercept_sd`,
# an optional named list of `slopes` (one or more, keyed by predictor), and an
# optional intercept/slope correlation `cor` (scalar or full matrix).
.mp_validate_random_effects <- function(random_effects) {
  if (is.null(random_effects)) {
    return(invisible(NULL))
  }
  .assert_named_list(random_effects, "random_effects")
  for (group in names(random_effects)) {
    spec <- random_effects[[group]]
    if (!is.list(spec) || is.null(names(spec)) || any(names(spec) == "")) {
      .stop(sprintf("`random_effects$%s` must be a named list, e.g. list(intercept_sd = 0.5).", group))
    }
    unknown <- setdiff(names(spec), c("intercept_sd", "slopes", "cor"))
    if (length(unknown) > 0) {
      .stop(sprintf(
        "`random_effects$%s` has unsupported field(s): %s. Supported: intercept_sd, slopes, cor.",
        group, paste(unknown, collapse = ", ")
      ))
    }
    if (is.null(spec$intercept_sd)) {
      .stop(sprintf("`random_effects$%s` must include a numeric `intercept_sd`.", group))
    }
    .assert_is_nonneg_num(spec$intercept_sd, sprintf("random_effects$%s$intercept_sd", group))

    slope_names <- character(0)
    if (!is.null(spec$slopes)) {
      .assert_named_list(spec$slopes, sprintf("random_effects$%s$slopes", group))
      slope_names <- names(spec$slopes)
      for (p in slope_names) {
        .assert_is_nonneg_num(spec$slopes[[p]], sprintf("random_effects$%s$slopes$%s", group, p))
      }
    }

    if (!is.null(spec$cor)) {
      term_names <- c("(Intercept)", slope_names)
      q <- length(term_names)
      if (is.matrix(spec$cor)) {
        if (nrow(spec$cor) != q || ncol(spec$cor) != q) {
          .stop(sprintf(
            "`random_effects$%s$cor` matrix must be %d x %d (intercept + %d slope(s)).",
            group, q, q, length(slope_names)
          ))
        }
        if (!isSymmetric(unname(spec$cor), tol = 1e-8)) {
          .stop(sprintf("`random_effects$%s$cor` matrix must be symmetric.", group))
        }
        if (any(abs(spec$cor) > 1 + 1e-8)) {
          .stop(sprintf("`random_effects$%s$cor` entries must be in [-1, 1].", group))
        }
      } else {
        .assert_is_num(spec$cor, sprintf("random_effects$%s$cor", group))
        if (spec$cor < -1 || spec$cor > 1) {
          .stop(sprintf("`random_effects$%s$cor` must be in [-1, 1].", group))
        }
      }
      # The implied correlation matrix must be positive (semi-)definite.
      R <- .mp_cor_matrix(spec$cor, term_names)
      ev <- tryCatch(min(eigen(R, symmetric = TRUE, only.values = TRUE)$values),
                     error = function(e) -Inf)
      if (!is.finite(ev) || ev < -1e-8) {
        .stop(sprintf(
          "`random_effects$%s$cor` does not yield a positive-definite correlation matrix.",
          group
        ))
      }
    }
  }
  invisible(NULL)
}

# Fold a legacy `icc` list into the canonical `random_effects` form, warning
# once per session. Returns the merged `random_effects` list.
.mp_absorb_icc <- function(random_effects, icc) {
  if (is.null(icc)) {
    return(random_effects)
  }
  if (!isTRUE(getOption("mixpower.icc_deprecation_warned", FALSE))) {
    warning(
      "`icc` is deprecated and is interpreted as the random-intercept SD ",
      "(not an intraclass correlation). Use `random_effects` instead, e.g. ",
      "random_effects = list(subject = list(intercept_sd = 0.5)).",
      call. = FALSE
    )
    options(mixpower.icc_deprecation_warned = TRUE)
  }
  if (is.null(random_effects)) random_effects <- list()
  for (group in names(icc)) {
    if (is.null(random_effects[[group]]$intercept_sd)) {
      random_effects[[group]] <- list(intercept_sd = icc[[group]])
    }
  }
  random_effects
}
