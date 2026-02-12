#' Simulate count outcome data for a Negative Binomial GLMM
#' @param scenario An `mp_scenario` object.
#' @param predictor Predictor column name.
#' @param subject Subject ID column name.
#' @param outcome Outcome column name.
#' @param item Optional item ID column name.
#' @param theta NB dispersion parameter (size). Larger = less over-dispersion.
#' @return A data.frame with outcome and predictors.
#' @export
simulate_glmm_nb_data <- function(scenario,
                                  predictor = "condition",
                                  subject = "subject",
                                  outcome = "y",
                                  item = NULL,
                                  theta = NULL) {
  design <- scenario$design
  assumptions <- scenario$assumptions

  n_subjects <- design$clusters$subject
  n_trials <- design$trials_per_cell

  if (is.null(n_subjects) || is.null(n_trials)) {
    stop("`design` must include `clusters$subject` and `trials_per_cell`.", call. = FALSE)
  }

  n <- n_subjects * n_trials
  x <- rep(c(0, 1), length.out = n)

  beta0 <- `%||%`(assumptions$fixed_effects[["(Intercept)"]], 0)
  beta1 <- `%||%`(assumptions$fixed_effects[[predictor]], 0)

  re_subject <- if (!is.null(assumptions$icc$subject)) {
    stats::rnorm(n_subjects, mean = 0, sd = assumptions$icc$subject)
  } else {
    rep(0, n_subjects)
  }

  subject_id <- rep(seq_len(n_subjects), each = n_trials)
  eta <- beta0 + beta1 * x + re_subject[subject_id]

  if (!is.null(item) && !is.null(assumptions$icc$item)) {
    n_items <- `%||%`(design$clusters$item, n_trials)
    item_id <- rep(seq_len(n_items), length.out = n)
    re_item <- stats::rnorm(n_items, mean = 0, sd = assumptions$icc$item)
    eta <- eta + re_item[item_id]
  } else {
    item_id <- NULL
  }

  mu <- exp(eta)

  # Dispersion parameter: allow explicit or stored in assumptions (e.g., assumptions$theta)
  theta_use <- `%||%`(theta, `%||%`(assumptions$theta, 1))
  if (!is.numeric(theta_use) || length(theta_use) != 1L || theta_use <= 0) {
    stop("`theta` must be a positive numeric value.", call. = FALSE)
  }

  y <- stats::rnbinom(n, size = theta_use, mu = mu)

  dat <- data.frame(
    y = y,
    condition = x,
    subject = subject_id,
    stringsAsFactors = FALSE
  )

  if (!is.null(item_id)) {
    dat[[item]] <- item_id
  }

  names(dat)[names(dat) == "condition"] <- predictor
  names(dat)[names(dat) == "subject"] <- subject
  names(dat)[names(dat) == "y"] <- outcome

  dat
}
