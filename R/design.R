#' Create a study design specification
#'
#' `mp_design()` encodes how data will be collected: cluster sizes and repeated
#' measurements. It does not encode effect sizes or analysis decisions.
#'
#' @param clusters Named list of positive integers. Example:
#'   `list(subject = 50, item = 30)`. For a nested (three-level) design, a
#'   nested factor's count is interpreted as the number of units *per parent*
#'   (see `nesting`).
#' @param trials_per_cell Number of repeated observations per subject. A single
#'   positive integer (balanced), or a positive-integer vector recycled across
#'   subjects for an unbalanced design.
#' @param predictors Optional named list giving the design type of each
#'   predictor (each non-intercept fixed effect). Each entry is either a string
#'   (`"binary"` or `"continuous"`) or a list with `type` and `level`
#'   (`"within"` or `"between"`). Unspecified predictors default to a balanced
#'   within-subject binary factor. Example:
#'   `list(time = list(type = "continuous", level = "within"), group = "binary")`.
#' @param nesting Optional named character vector mapping a child grouping
#'   factor to its parent, e.g. `c(subject = "site")` for subjects nested in
#'   sites. The parent must also appear in `clusters`.
#' @param notes Optional free text.
#'
#' @return An object of class `mp_design`.
#' @export
#'
#' @examples
#' d <- mp_design(clusters = list(subject = 40), trials_per_cell = 10)
#' d
#'
#' # Three-level: 8 sites, 5 subjects per site, 4 trials each.
#' d3 <- mp_design(
#'   clusters = list(site = 8, subject = 5),
#'   trials_per_cell = 4,
#'   nesting = c(subject = "site")
#' )
mp_design <- function(clusters, trials_per_cell = 1, predictors = NULL,
                      nesting = NULL, notes = NULL) {
  .assert_named_list(clusters, "clusters")
  for (nm in names(clusters)) {
    .assert_is_pos_int(clusters[[nm]], sprintf("clusters$%s", nm))
  }

  if (!is.numeric(trials_per_cell) || length(trials_per_cell) < 1L ||
      anyNA(trials_per_cell) || any(trials_per_cell < 1) ||
      any(trials_per_cell != as.integer(trials_per_cell))) {
    .stop("`trials_per_cell` must be a positive integer or a vector of positive integers.")
  }

  .mp_validate_predictors(predictors)
  nesting <- .mp_validate_nesting(nesting, names(clusters))

  if (!is.null(notes) && (!is.character(notes) || length(notes) != 1)) {
    .stop("`notes` must be a length-1 character string or NULL.")
  }

  out <- list(
    clusters = clusters,
    trials_per_cell = as.integer(trials_per_cell),
    predictors = predictors,
    nesting = nesting,
    notes = notes
  )
  class(out) <- "mp_design"
  out
}

# Validate the optional `predictors` design spec.
.mp_validate_predictors <- function(predictors) {
  if (is.null(predictors)) {
    return(invisible(NULL))
  }
  .assert_named_list(predictors, "predictors")
  for (nm in names(predictors)) {
    s <- predictors[[nm]]
    if (is.character(s)) {
      if (length(s) != 1L || !s %in% c("binary", "continuous")) {
        .stop(sprintf("`predictors$%s` must be 'binary' or 'continuous'.", nm))
      }
    } else if (is.list(s)) {
      type <- `%||%`(s$type, "binary")
      level <- `%||%`(s$level, "within")
      if (!type %in% c("binary", "continuous")) {
        .stop(sprintf("`predictors$%s$type` must be 'binary' or 'continuous'.", nm))
      }
      if (!level %in% c("within", "between")) {
        .stop(sprintf("`predictors$%s$level` must be 'within' or 'between'.", nm))
      }
    } else {
      .stop(sprintf("`predictors$%s` must be a string or a list(type=, level=).", nm))
    }
  }
  invisible(NULL)
}

# Validate the optional `nesting` map; returns it (or NULL).
.mp_validate_nesting <- function(nesting, cluster_names) {
  if (is.null(nesting)) {
    return(NULL)
  }
  if (!is.character(nesting) || is.null(names(nesting)) || any(names(nesting) == "")) {
    .stop("`nesting` must be a named character vector, e.g. c(subject = 'site').")
  }
  for (child in names(nesting)) {
    parent <- nesting[[child]]
    if (!child %in% cluster_names) {
      .stop(sprintf("`nesting` child '%s' is not in clusters.", child))
    }
    if (!parent %in% cluster_names) {
      .stop(sprintf("`nesting` parent '%s' is not in clusters.", parent))
    }
  }
  nesting
}

#' @export
print.mp_design <- function(x, ...) {
  cat("<mp_design>\n")
  cat("  clusters:\n")
  for (nm in names(x$clusters)) {
    suffix <- if (!is.null(x$nesting) && nm %in% names(x$nesting)) {
      sprintf(" (per %s)", x$nesting[[nm]])
    } else {
      ""
    }
    cat(sprintf("    - %s: %d%s\n", nm, x$clusters[[nm]], suffix))
  }
  tpc <- x$trials_per_cell
  if (length(tpc) == 1L) {
    cat(sprintf("  trials_per_cell: %d\n", tpc))
  } else {
    cat(sprintf("  trials_per_cell: [%s] (unbalanced)\n", paste(tpc, collapse = ", ")))
  }
  if (!is.null(x$predictors)) {
    cat("  predictors:", paste(names(x$predictors), collapse = ", "), "\n")
  }
  if (!is.null(x$notes)) cat(sprintf("  notes: %s\n", x$notes))
  invisible(x)
}

#' @export
summary.mp_design <- function(object, ...) {
  object
}
