#' Add a missing-data / dropout mechanism to a scenario
#'
#' Wraps a scenario so that, on every replicate, observations are deleted from
#' the simulated data before the model is fit. This lets a power analysis
#' reflect realistic incomplete data (Gallop & Liu, 2017; Magnusson, 2018).
#' Three mechanisms are supported:
#'
#' * `"mcar"`: each observation is deleted independently with probability
#'   `prob` (missing completely at random).
#' * `"mar"`: each observation is deleted with a probability that depends on an
#'   *observed* column `on` through a logistic model
#'   `plogis(qlogis(prob) + slope * on)` (missing at random).
#' * `"dropout"`: monotone longitudinal dropout along `time` within each
#'   subject --- once a subject drops out it contributes no later observations.
#'   The dropout pattern is given either by `dropout` as a vector of cumulative
#'   dropout proportions (one per ordered timepoint) or as
#'   `list(shape =, scale =)` for a Weibull dropout time on the `time` scale.
#'
#' @param scenario An `mp_scenario` (any backend).
#' @param mechanism One of `"mcar"`, `"mar"`, `"dropout"`.
#' @param prob Baseline deletion probability for `"mcar"`/`"mar"`.
#' @param on Name of the observed column the `"mar"` probability depends on.
#' @param slope Logit-scale slope for `"mar"` (default 0).
#' @param time Name of the within-subject ordering column for `"dropout"`.
#' @param dropout For `"dropout"`: a numeric vector of cumulative dropout
#'   proportions per ordered timepoint, or `list(shape=, scale=)` for Weibull.
#' @param subject Subject grouping column (default `"subject"`).
#' @return The scenario with its simulator wrapped to apply missingness.
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   d <- mp_design(list(subject = 30), trials_per_cell = 6,
#'                  predictors = list(time = "continuous"))
#'   a <- mp_assumptions(list("(Intercept)" = 0, time = 0.4),
#'                       random_effects = list(subject = list(intercept_sd = 0.5)),
#'                       residual_sd = 1)
#'   scn <- mp_scenario_lme4(y ~ time + (1 | subject), design = d,
#'                           assumptions = a, predictor = "time")
#'   scn_drop <- mp_missing(scn, "dropout", time = "time",
#'                          dropout = c(0, 0.1, 0.2, 0.35, 0.5, 0.6))
#'   mp_power(scn_drop, nsim = 20, seed = 1)
#' }
#' }
mp_missing <- function(scenario,
                       mechanism = c("mcar", "mar", "dropout"),
                       prob = NULL,
                       on = NULL,
                       slope = 0,
                       time = NULL,
                       dropout = NULL,
                       subject = "subject") {
  .assert_class(scenario, "mp_scenario", "scenario")
  mechanism <- match.arg(mechanism)
  spec <- list(mechanism = mechanism, prob = prob, on = on, slope = slope,
               time = time, dropout = dropout, subject = subject)
  .mp_validate_missing(spec)

  orig <- scenario$engine$simulate_fun
  if (is.null(orig)) .stop("Scenario has no `simulate_fun` to wrap.")
  takes_seed <- "seed" %in% names(formals(orig))

  scenario$engine$simulate_fun <- function(scn, seed = NULL) {
    dat <- if (takes_seed) orig(scn, seed = seed) else orig(scn)
    .mp_apply_missing(dat, spec)
  }
  scenario$missing <- spec
  scenario
}

.mp_validate_missing <- function(spec) {
  m <- spec$mechanism
  if (m %in% c("mcar", "mar")) {
    if (is.null(spec$prob) || !is.numeric(spec$prob) || length(spec$prob) != 1L ||
        spec$prob < 0 || spec$prob >= 1) {
      .stop("`prob` must be a single number in [0, 1) for mechanism 'mcar'/'mar'.")
    }
  }
  if (m == "mar" && (is.null(spec$on) || !is.character(spec$on) || length(spec$on) != 1L)) {
    .stop("mechanism 'mar' requires `on`, the name of an observed column.")
  }
  if (m == "dropout") {
    if (is.null(spec$time) || !is.character(spec$time) || length(spec$time) != 1L) {
      .stop("mechanism 'dropout' requires `time`, the within-subject ordering column.")
    }
    if (is.null(spec$dropout)) {
      .stop("mechanism 'dropout' requires `dropout` (a cumulative-proportion vector or list(shape=, scale=)).")
    }
    if (is.list(spec$dropout)) {
      if (is.null(spec$dropout$shape) || is.null(spec$dropout$scale)) {
        .stop("Weibull `dropout` must be list(shape=, scale=).")
      }
    } else if (is.numeric(spec$dropout)) {
      if (any(spec$dropout < 0 | spec$dropout > 1) || is.unsorted(spec$dropout)) {
        .stop("Vector `dropout` must be non-decreasing cumulative proportions in [0, 1].")
      }
    } else {
      .stop("`dropout` must be a numeric vector or list(shape=, scale=).")
    }
  }
  invisible(NULL)
}

# Apply the missingness spec to a simulated data frame, returning the rows that
# remain (with unused factor levels dropped).
.mp_apply_missing <- function(dat, spec) {
  n <- nrow(dat)
  keep <- rep(TRUE, n)

  if (spec$mechanism == "mcar") {
    keep <- stats::runif(n) > spec$prob

  } else if (spec$mechanism == "mar") {
    if (!spec$on %in% names(dat)) {
      .stop(sprintf("mechanism 'mar' column `on` = '%s' is not in the simulated data.", spec$on))
    }
    z <- as.numeric(dat[[spec$on]])
    p <- stats::plogis(stats::qlogis(spec$prob) + spec$slope * z)
    keep <- stats::runif(n) > p

  } else if (spec$mechanism == "dropout") {
    if (!all(c(spec$time, spec$subject) %in% names(dat))) {
      .stop("mechanism 'dropout' requires both `time` and `subject` columns in the simulated data.")
    }
    tvals <- dat[[spec$time]]
    subj <- as.character(dat[[spec$subject]])
    subjects <- unique(subj)
    u <- stats::setNames(stats::runif(length(subjects)), subjects)

    if (is.list(spec$dropout)) {
      # Weibull dropout time per subject; keep observations before it.
      tdrop <- stats::setNames(
        stats::qweibull(u, shape = spec$dropout$shape, scale = spec$dropout$scale),
        subjects
      )
      keep <- tvals < tdrop[subj]
    } else {
      # Cumulative dropout proportion per ordered timepoint: a subject with draw
      # u is still present at a timepoint while u exceeds that point's proportion.
      tp <- sort(unique(tvals))
      prop <- spec$dropout
      if (length(prop) != length(tp)) {
        prop <- stats::approx(seq_along(prop), prop, n = length(tp), rule = 2)$y
      }
      prop_at <- stats::setNames(prop, as.character(tp))
      keep <- u[subj] > prop_at[as.character(tvals)]
    }
  }

  keep[is.na(keep)] <- FALSE
  droplevels(dat[keep, , drop = FALSE])
}
