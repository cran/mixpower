#' Recommend an inference method for a scenario
#'
#' Heuristic guidance on the `test_method` for a scenario, based on the number of
#' levels of the random grouping factors. Wald (normal-approximation z/t) tests
#' and, to a lesser extent, likelihood-ratio tests are known to be
#' anti-conservative when the number of clusters is small (Luke, 2017): the
#' degrees of freedom are overstated, so the test rejects too often. With few
#' clusters, a degrees-of-freedom-corrected test (Satterthwaite or Kenward-Roger,
#' for linear mixed models) or a parametric bootstrap (any family) controls Type
#' I error far better.
#'
#' This is a fast, design-based heuristic; to *measure* a specific design and
#' method, use [mp_calibrate()].
#'
#' @param scenario An `mp_scenario`.
#' @param small_clusters Threshold below which the smallest grouping factor is
#'   treated as "few clusters" (default 30).
#' @return An object of class `mp_recommendation`: a list with `method` (the
#'   scenario's current method), `n_groups` (smallest grouping-factor size),
#'   `is_lmm`, `caution` (logical), `recommended` (character vector), and
#'   `rationale`.
#' @seealso [mp_calibrate()].
#' @export
#' @examples
#' d <- mp_design(list(subject = 12), trials_per_cell = 8)
#' a <- mp_assumptions(
#'   fixed_effects = list("(Intercept)" = 0, condition = 0.4),
#'   random_effects = list(subject = list(intercept_sd = 0.5)),
#'   residual_sd = 1
#' )
#' scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
#' mp_recommend_method(scn)
mp_recommend_method <- function(scenario, small_clusters = 30L) {
  .assert_class(scenario, "mp_scenario", "scenario")

  clusters <- scenario$design$clusters
  n_groups <- if (length(clusters)) {
    min(vapply(clusters, function(v) as.integer(v[[1]]), integer(1)))
  } else {
    NA_integer_
  }

  method <- if (is.list(scenario$test)) {
    `%||%`(scenario$test$method, "wald")
  } else {
    scenario$test
  }
  is_lmm <- !is.null(scenario$assumptions$residual_sd)

  df_corrected <- c("satterthwaite", "kenward-roger")
  robust_few <- if (is_lmm) c("kenward-roger", "satterthwaite", "pb") else "pb"

  few <- !is.na(n_groups) && n_groups < small_clusters
  caution <- FALSE
  recommended <- character(0)
  rationale <- ""

  if (few && method %in% c("wald", "lrt")) {
    caution <- TRUE
    recommended <- robust_few
    rationale <- sprintf(
      paste0("Smallest grouping factor has %d levels (< %d). '%s' tests can be ",
             "anti-conservative with few clusters; prefer %s. Verify with mp_calibrate()."),
      n_groups, small_clusters, method,
      paste(sprintf("'%s'", robust_few), collapse = " or ")
    )
  } else if (few) {
    recommended <- method
    rationale <- sprintf(
      "Smallest grouping factor has %d levels; '%s' is an appropriate small-sample choice.",
      n_groups, method
    )
  } else {
    recommended <- method
    rationale <- sprintf(
      "Smallest grouping factor has %s levels; '%s' is reasonable. Confirm with mp_calibrate() if unsure.",
      ifelse(is.na(n_groups), "an unknown number of", as.character(n_groups)), method
    )
  }

  if (!is_lmm) {
    recommended <- setdiff(recommended, df_corrected)
    if (length(recommended) == 0L) recommended <- "pb"
  }

  out <- list(
    method = method,
    n_groups = n_groups,
    is_lmm = is_lmm,
    caution = caution,
    recommended = recommended,
    rationale = rationale
  )
  class(out) <- "mp_recommendation"
  out
}

#' @export
print.mp_recommendation <- function(x, ...) {
  cat("<mp_recommendation>\n")
  cat(sprintf("  current method: %s\n", x$method))
  cat(sprintf("  smallest grouping factor: %s levels\n",
              ifelse(is.na(x$n_groups), "unknown", as.character(x$n_groups))))
  cat(sprintf("  caution: %s\n", if (isTRUE(x$caution)) "yes" else "no"))
  cat(sprintf("  recommended: %s\n", paste(x$recommended, collapse = ", ")))
  cat(sprintf("  %s\n", x$rationale))
  invisible(x)
}
