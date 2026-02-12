#' Create a study design specification
#'
#' `mp_design()` encodes how data will be collected: cluster sizes and repeated
#' measurements. It does not encode effect sizes or analysis decisions.
#'
#' @param clusters Named list of positive integers. Example: `list(subject = 50, item = 30)`.
#' @param trials_per_cell Positive integer. Number of repeated observations per design cell.
#' @param notes Optional free text.
#'
#' @return An object of class `mp_design`.
#' @export
#'
#' @examples
#' d <- mp_design(clusters = list(subject = 40), trials_per_cell = 10)
#' d
mp_design <- function(clusters, trials_per_cell = 1, notes = NULL) {
  .assert_named_list(clusters, "clusters")
  for (nm in names(clusters)) {
    .assert_is_pos_int(clusters[[nm]], sprintf("clusters$%s", nm))
  }
  .assert_is_pos_int(trials_per_cell, "trials_per_cell")

  if (!is.null(notes) && (!is.character(notes) || length(notes) != 1)) {
    .stop("`notes` must be a length-1 character string or NULL.")
  }

  out <- list(
    clusters = clusters,
    trials_per_cell = as.integer(trials_per_cell),
    notes = notes
  )
  class(out) <- "mp_design"
  out
}

#' @export
print.mp_design <- function(x, ...) {
  cat("<mp_design>\n")
  cat("  clusters:\n")
  for (nm in names(x$clusters)) {
    cat(sprintf("    - %s: %d\n", nm, x$clusters[[nm]]))
  }
  cat(sprintf("  trials_per_cell: %d\n", x$trials_per_cell))
  if (!is.null(x$notes)) cat(sprintf("  notes: %s\n", x$notes))
  invisible(x)
}

#' @export
summary.mp_design <- function(object, ...) {
  object
}
