#' Create modeling assumptions for simulation-based power
#'
#' Assumptions encode effect sizes and the variance components used to simulate
#' data. Random-effect sizes are given as standard deviations on the linear
#' predictor (the scale lme4 reports), via `random_effects`.
#'
#' @param fixed_effects Named list of numeric values (e.g.,
#'   `list("(Intercept)" = 0, condition = 0.4)`).
#' @param random_effects Optional named list keyed by grouping factor. Each
#'   element is a named list with `intercept_sd` (the random-intercept SD on the
#'   linear-predictor scale) and, optionally, `slopes` (a named list of
#'   random-slope SDs, one per predictor) and `cor`. For example,
#'   `list(subject = list(intercept_sd = 0.5, slopes = list(condition = 0.3), cor = 0.2))`
#'   encodes a correlated random intercept and slope, i.e.
#'   `(1 + condition | subject)`. With several slopes, `cor` may be a single
#'   scalar (applied to every pair of terms) or a full correlation matrix over
#'   `c("(Intercept)", names(slopes))`; each fixed effect named in
#'   `fixed_effects` also becomes a balanced design predictor.
#' @param icc Deprecated. Previously documented as an intraclass correlation
#'   but used as a random-intercept SD. If supplied it is interpreted as
#'   `intercept_sd` and folded into `random_effects` with a warning. Use
#'   `random_effects` instead.
#' @param residual_sd Optional non-negative numeric residual SD (Gaussian).
#' @param notes Optional free text.
#'
#' @return An object of class `mp_assumptions`.
#' @export
#'
#' @examples
#' a <- mp_assumptions(
#'   fixed_effects = list("(Intercept)" = 0, condition = 0.4),
#'   random_effects = list(subject = list(intercept_sd = 0.5)),
#'   residual_sd = 1
#' )
#' a
mp_assumptions <- function(fixed_effects,
                           random_effects = NULL,
                           icc = NULL,
                           residual_sd = NULL,
                           notes = NULL) {
  .assert_named_list(fixed_effects, "fixed_effects")
  for (nm in names(fixed_effects)) {
    val <- fixed_effects[[nm]]
    if (!is.numeric(val) || anyNA(val)) {
      .stop(sprintf("`fixed_effects$%s` must be numeric without NA.", nm))
    }
  }

  .mp_validate_random_effects(random_effects)

  # Legacy `icc` is interpreted as a random-intercept SD and folded in.
  if (!is.null(icc)) {
    .assert_named_list(icc, "icc")
    for (nm in names(icc)) {
      v <- icc[[nm]]
      if (!is.numeric(v) || length(v) != 1 || is.na(v) || v < 0) {
        .stop(sprintf("`icc$%s` must be a single non-negative number.", nm))
      }
    }
    random_effects <- .mp_absorb_icc(random_effects, icc)
  }

  if (!is.null(residual_sd)) .assert_is_nonneg_num(residual_sd, "residual_sd")

  if (!is.null(notes) && (!is.character(notes) || length(notes) != 1)) {
    .stop("`notes` must be a length-1 character string or NULL.")
  }

  out <- list(
    fixed_effects = fixed_effects,
    random_effects = random_effects,
    # Legacy echo kept for back-compat with code reading `$icc` directly.
    icc = icc,
    residual_sd = residual_sd,
    notes = notes
  )
  class(out) <- "mp_assumptions"
  out
}

#' @export
print.mp_assumptions <- function(x, ...) {
  cat("<mp_assumptions>\n")
  cat("  fixed_effects:\n")
  for (nm in names(x$fixed_effects)) {
    v <- x$fixed_effects[[nm]]
    if (length(v) == 1) {
      cat(sprintf("    - %s: %g\n", nm, v))
    } else {
      cat(sprintf("    - %s: [%s]\n", nm, paste(v, collapse = ", ")))
    }
  }
  if (!is.null(x$random_effects)) {
    cat("  random_effects (SD on linear predictor):\n")
    for (nm in names(x$random_effects)) {
      spec <- x$random_effects[[nm]]
      line <- sprintf("    - %s: intercept_sd = %g", nm, spec$intercept_sd)
      if (!is.null(spec$slopes) && length(spec$slopes) > 0) {
        sl <- vapply(names(spec$slopes),
                     function(s) sprintf("%s = %g", s, spec$slopes[[s]]),
                     character(1))
        line <- paste0(line, sprintf(", slope_sd(%s)", paste(sl, collapse = ", ")))
      }
      if (!is.null(spec$cor)) {
        line <- paste0(line, if (is.matrix(spec$cor)) ", cor = <matrix>" else sprintf(", cor = %g", spec$cor))
      }
      cat(line, "\n")
    }
  }
  if (!is.null(x$residual_sd)) cat(sprintf("  residual_sd: %g\n", x$residual_sd))
  if (!is.null(x$notes)) cat(sprintf("  notes: %s\n", x$notes))
  invisible(x)
}

#' @export
summary.mp_assumptions <- function(object, ...) {
  object
}
