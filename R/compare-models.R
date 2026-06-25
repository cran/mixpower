#' Compare analysis models on the same simulated data
#'
#' Simulates data once per replicate from the first scenario, then fits and
#' tests *every* supplied scenario on that same dataset. This is the analogue of
#' powerlmm's `sim_formula`: because the models see identical data, differences
#' in their rejection rates isolate the effect of the analysis choice. Use it to
#' study power across competing specifications, or to expose Type I inflation
#' from a misspecified model (e.g. dropping a random slope that is present in the
#' data-generating process).
#'
#' All scenarios must share the same data-generating process (design and
#' assumptions); they should differ only in their analysis model (formula /
#' random-effects structure / test). The first scenario's `simulate_fun` drives
#' data generation.
#'
#' @param scenarios A named list of `mp_scenario` objects.
#' @param nsim Positive integer number of simulations.
#' @param alpha Significance threshold (default 0.05).
#' @param seed Optional seed for reproducibility.
#' @param conf_level Confidence level for the per-model power intervals.
#' @param failure_policy How to treat failed fits (see [mp_power()]).
#' @return An object of class `mp_model_comparison` with a `results` data frame
#'   (one row per model: `power`, `conf_low`, `conf_high`, `failure_rate`,
#'   `n_effective`, `nsim`).
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   d <- mp_design(list(subject = 30), trials_per_cell = 8)
#'   a <- mp_assumptions(
#'     fixed_effects = list("(Intercept)" = 0, condition = 0),
#'     random_effects = list(subject = list(intercept_sd = 0.5,
#'                                           slopes = list(condition = 0.8))),
#'     residual_sd = 1
#'   )
#'   maximal <- mp_scenario_lme4(y ~ condition + (1 + condition | subject), d, a)
#'   reduced <- mp_scenario_lme4(y ~ condition + (1 | subject), d, a)
#'   mp_compare_models(list(maximal = maximal, reduced = reduced),
#'                     nsim = 50, seed = 1)
#' }
#' }
mp_compare_models <- function(scenarios,
                              nsim,
                              alpha = 0.05,
                              seed = NULL,
                              conf_level = 0.95,
                              failure_policy = c("count_as_nondetect", "exclude")) {
  failure_policy <- match.arg(failure_policy)
  if (inherits(scenarios, "mp_scenario") || !is.list(scenarios) ||
      length(scenarios) < 1L || is.null(names(scenarios)) ||
      any(names(scenarios) == "") ||
      !all(vapply(scenarios, inherits, logical(1), "mp_scenario"))) {
    .stop("`scenarios` must be a named list of mp_scenario objects.")
  }
  .assert_is_pos_int(nsim, "nsim")

  base <- scenarios[[1]]
  if (is.null(base$engine$simulate_fun)) {
    .stop("The first scenario must provide a `simulate_fun` (it generates the shared data).")
  }
  nm <- names(scenarios)
  rep_seeds <- .rep_seeds(seed, nsim)
  takes_seed <- "seed" %in% names(formals(base$engine$simulate_fun))

  sig <- matrix(FALSE, nrow = nsim, ncol = length(scenarios), dimnames = list(NULL, nm))
  failed <- matrix(FALSE, nrow = nsim, ncol = length(scenarios), dimnames = list(NULL, nm))

  for (i in seq_len(nsim)) {
    si <- rep_seeds[[i]]
    dat <- tryCatch(
      .with_seed(si, if (takes_seed) base$engine$simulate_fun(base, seed = si) else base$engine$simulate_fun(base)),
      error = function(e) NULL
    )
    if (is.null(dat) || !is.data.frame(dat)) {
      failed[i, ] <- TRUE
      next
    }
    for (j in seq_along(scenarios)) {
      s <- scenarios[[j]]
      p <- tryCatch(
        suppressWarnings(s$engine$test_fun(s$engine$fit_fun(dat, s), s)$p_value),
        error = function(e) NA_real_
      )
      if (is.na(p)) failed[i, j] <- TRUE else if (p < alpha) sig[i, j] <- TRUE
    }
  }

  rows <- lapply(seq_along(scenarios), function(j) {
    nf <- sum(failed[, j])
    n_eff <- if (failure_policy == "exclude") nsim - nf else nsim
    x <- sum(sig[, j])
    power <- if (n_eff > 0) x / n_eff else NA_real_
    ci <- .mp_power_ci(x, n_eff, conf_level, "clopper-pearson")
    data.frame(model = nm[[j]], power = power, conf_low = ci[[1]], conf_high = ci[[2]],
               failure_rate = nf / nsim, n_effective = n_eff, nsim = nsim,
               stringsAsFactors = FALSE)
  })

  out <- list(
    results = do.call(rbind, rows),
    alpha = alpha,
    nsim = nsim,
    failure_policy = failure_policy
  )
  class(out) <- "mp_model_comparison"
  out
}

#' @export
print.mp_model_comparison <- function(x, ...) {
  cat("<mp_model_comparison>\n")
  cat(sprintf("  nsim: %d, alpha: %g\n", x$nsim, x$alpha))
  print(x$results, row.names = FALSE)
  invisible(x)
}

#' @export
summary.mp_model_comparison <- function(object, ...) {
  object$results
}
