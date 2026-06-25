#' Scale a fitted-model scenario's sample size up or down
#'
#' Sets target level counts for one or more grouping factors of a scenario built
#' with [mp_from_fit()], so power can be evaluated at a sample size different
#' from the pilot's. This is the analogue of `simr::extend()`: levels are cloned
#' from the pilot's within-level structure with fresh ids, and fresh random
#' effects are drawn from the fitted covariance, so the extended data represent a
#' larger (or smaller) sample from the same population.
#'
#' Pair with [mp_power()] for a single N, or with [mp_power_curve()] /
#' [mp_solve_sample_size()] using the `extend.<group>` key for a curve over N.
#'
#' @param scenario An `mp_scenario` created by [mp_from_fit()].
#' @param ... Named target level counts, e.g. `Subject = 60`.
#' @return The scenario with its `extend` targets set.
#' @seealso [mp_from_fit()].
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   m <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
#'   scn <- mp_from_fit(m, test_term = "Days")
#'   big <- mp_extend(scn, Subject = 40)
#'   mp_power(big, nsim = 20, seed = 1)
#' }
#' }
mp_extend <- function(scenario, ...) {
  .assert_class(scenario, "mp_scenario", "scenario")
  if (is.null(scenario$grouping_factors)) {
    .stop("`mp_extend()` is only supported for scenarios built with `mp_from_fit()`.")
  }
  targets <- list(...)
  if (length(targets) == 0L || is.null(names(targets)) || any(names(targets) == "")) {
    .stop("Provide named target sizes, e.g. mp_extend(scenario, Subject = 60).")
  }
  for (g in names(targets)) {
    if (!g %in% scenario$grouping_factors) {
      .stop(sprintf("'%s' is not a grouping factor of this scenario (have: %s).",
                    g, paste(scenario$grouping_factors, collapse = ", ")))
    }
    .assert_is_pos_int(targets[[g]], sprintf("extend target for '%s'", g))
  }
  if (is.null(scenario$extend)) scenario$extend <- list()
  for (g in names(targets)) scenario$extend[[g]] <- as.integer(targets[[g]])
  scenario
}

# Validate an `extend` argument (named list of positive-integer target sizes
# keyed by grouping factor). Returns the coerced list (or NULL).
.mp_validate_extend <- function(extend, grouping) {
  if (is.null(extend)) {
    return(NULL)
  }
  if (!is.list(extend) || is.null(names(extend)) || any(names(extend) == "")) {
    .stop("`extend` must be a named list, e.g. list(Subject = 60).")
  }
  out <- list()
  for (g in names(extend)) {
    if (!g %in% grouping) {
      .stop(sprintf("`extend` names a non-grouping factor '%s' (have: %s).",
                    g, paste(grouping, collapse = ", ")))
    }
    .assert_is_pos_int(extend[[g]], sprintf("extend$%s", g))
    out[[g]] <- as.integer(extend[[g]])
  }
  out
}

# Expand a model frame so grouping factor `group` has `n_target` levels, by
# recycling the existing per-level row blocks under fresh, unique level labels.
# Fresh labels make every level "new", so lme4::simulate() draws fresh random
# effects for all of them (a clean population draw).
.mp_extend_frame <- function(mf, group, n_target) {
  if (!group %in% names(mf)) {
    .stop(sprintf("Grouping factor '%s' is not a column of the model frame.", group))
  }
  levs <- as.character(unique(mf[[group]]))
  n_have <- length(levs)
  pick <- levs[((seq_len(n_target) - 1L) %% n_have) + 1L]
  blocks <- split(mf, factor(mf[[group]], levels = levs))
  out <- vector("list", n_target)
  for (i in seq_len(n_target)) {
    b <- blocks[[pick[i]]]
    b[[group]] <- sprintf("%s_%d", group, i)
    out[[i]] <- b
  }
  res <- do.call(rbind, out)
  res[[group]] <- factor(res[[group]])
  rownames(res) <- NULL
  res
}
