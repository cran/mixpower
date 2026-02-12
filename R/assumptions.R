#' Create modeling assumptions for simulation-based power
#'
#' Assumptions encode effect sizes and nuisance parameters. Values may be scalars
#' or vectors (for later sensitivity workflows), but `mp_power()` expects scalars
#' unless used inside a grid wrapper.
#'
#' @param fixed_effects Named list of numeric values (e.g., `list(condition = 0.4)`).
#' @param icc Optional named list of ICC values in [0, 1).
#' @param residual_sd Optional non-negative numeric residual SD.
#' @param notes Optional free text.
#'
#' @return An object of class `mp_assumptions`.
#' @export
#'
#' @examples
#' a <- mp_assumptions(fixed_effects = list(condition = 0.4), residual_sd = 1)
#' a
mp_assumptions <- function(fixed_effects,
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

  if (!is.null(icc)) {
    .assert_named_list(icc, "icc")
    for (nm in names(icc)) {
      v <- icc[[nm]]
      if (!is.numeric(v) || length(v) != 1 || is.na(v) || v < 0 || v >= 1) {
        .stop(sprintf("`icc$%s` must be a single number in [0, 1).", nm))
      }
    }
  }

  if (!is.null(residual_sd)) .assert_is_nonneg_num(residual_sd, "residual_sd")

  if (!is.null(notes) && (!is.character(notes) || length(notes) != 1)) {
    .stop("`notes` must be a length-1 character string or NULL.")
  }

  out <- list(
    fixed_effects = fixed_effects,
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
  if (!is.null(x$icc)) {
    cat("  icc:\n")
    for (nm in names(x$icc)) cat(sprintf("    - %s: %g\n", nm, x$icc[[nm]]))
  }
  if (!is.null(x$residual_sd)) cat(sprintf("  residual_sd: %g\n", x$residual_sd))
  if (!is.null(x$notes)) cat(sprintf("  notes: %s\n", x$notes))
  invisible(x)
}

#' @export
summary.mp_assumptions <- function(object, ...) {
  object
}
