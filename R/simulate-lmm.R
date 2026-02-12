#' Simulate Gaussian data for a minimal linear mixed-effects design
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
simulate_lmm_data <- function(scenario,
                              seed,
                              outcome = "y",
                              predictor = "condition",
                              subject = "subject",
                              item = NULL,
                              re_subject_intercept_sd = 1,
                              re_item_intercept_sd = NULL) {
  .assert_class(scenario, "mp_scenario", "scenario")

  des <- scenario$design
  asm <- scenario$assumptions

  # Required: subject cluster size
  if (is.null(des$clusters[[subject]])) {
    .stop(sprintf("Design must include clusters$%s for this backend.", subject))
  }
  n_subject <- des$clusters[[subject]]
  trials <- des$trials_per_cell

  # Optional: item cluster size
  n_item <- NULL
  if (!is.null(item)) {
    if (is.null(des$clusters[[item]])) {
      .stop(sprintf("Design must include clusters$%s when item is specified.", item))
    }
    n_item <- des$clusters[[item]]
  }

  # Fixed effect value: must exist
  if (is.null(asm$fixed_effects[[predictor]])) {
    .stop(sprintf("Assumptions must include fixed_effects$%s for this backend.", predictor))
  }
  beta <- asm$fixed_effects[[predictor]]
  if (!is.numeric(beta) || length(beta) != 1 || is.na(beta)) .stop("Focal fixed effect must be a numeric scalar.")

  # Residual SD required for simulation; default to 1 if not provided
  resid_sd <- asm$residual_sd
  if (is.null(resid_sd)) resid_sd <- 1
  .assert_is_nonneg_num(resid_sd, "residual_sd")

  # Build dataset
  # Minimal design: each subject has `trials` observations, predictor randomized 0/1 within-subject.
  # If item is provided: each subject-trial assigned to an item uniformly at random.
  N <- n_subject * trials

  dat <- data.frame(
    rep(seq_len(n_subject), each = trials),
    stringsAsFactors = FALSE
  )
  names(dat) <- subject

  # Predictor: within-subject Bernoulli(0.5)
  dat[[predictor]] <- stats::rbinom(N, 1, 0.5)

  if (!is.null(item)) {
    dat[[item]] <- base::sample(seq_len(n_item), size = N, replace = TRUE)
  }

  # Random intercepts
  b_subject <- stats::rnorm(n_subject, mean = 0, sd = re_subject_intercept_sd)
  eta <- beta * dat[[predictor]] + b_subject[dat[[subject]]]

  if (!is.null(item)) {
    sd_item <- if (is.null(re_item_intercept_sd)) 1 else re_item_intercept_sd
    b_item <- stats::rnorm(n_item, mean = 0, sd = sd_item)
    eta <- eta + b_item[dat[[item]]]
  }

  # Outcome
  dat[[outcome]] <- eta + stats::rnorm(N, mean = 0, sd = resid_sd)

  # Coerce grouping factors to factors for lme4
  dat[[subject]] <- factor(dat[[subject]])
  if (!is.null(item)) dat[[item]] <- factor(dat[[item]])

  dat
}
