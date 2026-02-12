#' Simulation-based power estimation (engine-agnostic core)
#'
#' `mp_power()` runs repeated simulations under a scenario and estimates power for
#' the scenario's test decision rule (typically p < alpha).
#'
#' In Phase 4 core, the scenario must provide engine functions:
#' `simulate_fun`, `fit_fun`, and `test_fun`. Later phases will supply defaults
#' based on specific backends (e.g., lme4).
#'
#' @param scenario An `mp_scenario`.
#' @param nsim Positive integer number of simulations.
#' @param alpha Significance threshold for a detection (default 0.05).
#' @param seed Optional seed for reproducibility.
#' @param failure_policy How to treat failed fits / missing p-values:
#'   - `"count_as_nondetect"` (default): failures count as non-detections.
#'   - `"exclude"`: drop failures from the denominator (always reported).
#' @param keep What to store:
#'   - `"minimal"`: only per-sim summary rows.
#'   - `"fits"`: also store fit objects (may be large).
#'   - `"data"`: also store simulated data (can be very large).
#' @param conf_level Confidence level for the Wald interval (default 0.95).
#'
#' @return An object of class `mp_power`.
#' @export
#'
#' @examples
#' # A tiny toy engine (not mixed models) just to demonstrate the workflow:
#' d <- mp_design(list(subject = 30), trials_per_cell = 1)
#' a <- mp_assumptions(list(condition = 0.3), residual_sd = 1)
#'
#' sim_fun <- function(scn, seed) {
#'   n <- scn$design$clusters$subject
#'   x <- stats::rbinom(n, 1, 0.5)
#'   y <- scn$assumptions$fixed_effects$condition * x +
#'     stats::rnorm(n, sd = scn$assumptions$residual_sd)
#'   data.frame(y = y, condition = x)
#' }
#' fit_fun <- function(dat, scn) stats::lm(scn$formula, data = dat)
#' test_fun <- function(fit, scn) {
#'   sm <- summary(fit)
#'   p <- sm$coefficients["condition", "Pr(>|t|)"]
#'   list(p_value = as.numeric(p))
#' }
#'
#' s <- mp_scenario(
#'   y ~ condition, d, a,
#'   simulate_fun = sim_fun,
#'   fit_fun = fit_fun,
#'   test_fun = test_fun
#' )
#' res <- mp_power(s, nsim = 50, seed = 1)
#' summary(res)
mp_power <- function(scenario,
                     nsim,
                     alpha = 0.05,
                     seed = NULL,
                     failure_policy = c("count_as_nondetect", "exclude"),
                     keep = c("minimal", "fits", "data"),
                     conf_level = 0.95) {
  .assert_class(scenario, "mp_scenario", "scenario")
  .assert_is_pos_int(nsim, "nsim")
  .assert_is_num(alpha, "alpha")
  if (alpha <= 0 || alpha >= 1) .stop("`alpha` must be in (0, 1).")
  .assert_is_num(conf_level, "conf_level")
  if (conf_level <= 0 || conf_level >= 1) .stop("`conf_level` must be in (0, 1).")
  failure_policy <- match.arg(failure_policy)
  keep <- match.arg(keep)

  eng <- scenario$engine
  if (is.null(eng$simulate_fun) || is.null(eng$fit_fun) || is.null(eng$test_fun)) {
    .stop("Scenario engine is incomplete. Set `simulate_fun`, `fit_fun`, and `test_fun` in `mp_scenario()`.")
  }

  rep_seeds <- .rep_seeds(seed, nsim)

  # Storage
  rows <- vector("list", nsim)
  fits <- if (keep %in% c("fits", "data")) vector("list", nsim) else NULL
  datas <- if (keep == "data") vector("list", nsim) else NULL

  for (i in seq_len(nsim)) {
    si <- rep_seeds[[i]]
    out_i <- .run_one_rep(scenario, alpha = alpha, seed = si)
    rows[[i]] <- out_i$row
    if (!is.null(fits)) fits[[i]] <- out_i$fit
    if (!is.null(datas)) datas[[i]] <- out_i$data
  }

  sim_tbl <- do.call(rbind, lapply(rows, as.data.frame))
  sim_tbl$replicate <- seq_len(nsim)

  # Determine detection
  # p_value may be NA when fit/test fails
  detected_raw <- !is.na(sim_tbl$p_value) & sim_tbl$p_value < alpha

  if (failure_policy == "count_as_nondetect") {
    denom <- nrow(sim_tbl)
    detected <- sum(detected_raw, na.rm = TRUE)
  } else {
    ok <- !is.na(sim_tbl$p_value)
    denom <- sum(ok)
    detected <- sum(detected_raw[ok])
  }

  power_hat <- if (denom == 0) NA_real_ else detected / denom
  # MCSE and CI (simple Wald CI; conservative alternatives can be added later)
  mcse <- if (is.na(power_hat)) NA_real_ else sqrt(power_hat * (1 - power_hat) / denom)
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  ci <- if (is.na(power_hat)) c(NA_real_, NA_real_) else {
    lo <- max(0, power_hat - z * mcse)
    hi <- min(1, power_hat + z * mcse)
    c(lo, hi)
  }

  fail_rate <- mean(!sim_tbl$fit_ok)
  singular_rate <- mean(sim_tbl$singular %in% TRUE, na.rm = TRUE)

  res <- list(
    scenario = scenario,
    nsim = as.integer(nsim),
    alpha = alpha,
    seed = seed,
    failure_policy = failure_policy,
    keep = keep,
    conf_level = conf_level,
    sims = sim_tbl,
    power = power_hat,
    mcse = mcse,
    ci = ci,
    diagnostics = list(
      fail_rate = fail_rate,
      singular_rate = singular_rate
    ),
    fits = fits,
    data = datas
  )
  class(res) <- "mp_power"
  res
}

# Run one replicate with safe error handling
.run_one_rep <- function(scenario, alpha, seed) {
  eng <- scenario$engine

  data <- NULL
  fit <- NULL
  pval <- NA_real_
  fit_ok <- FALSE
  singular <- NA
  warn <- NA_character_
  err <- NA_character_

  # helper: append warning strings safely
  .append_warn <- function(existing, new_msg) {
    out <- stats::na.omit(c(existing, new_msg))
    paste(out, collapse = " | ")
  }

  # helper: evaluate expr, capturing warnings into `warn`, muffling if possible
  .capture_warnings <- function(expr) {
    withCallingHandlers(
      expr,
      warning = function(w) {
        warn <<- .append_warn(warn, conditionMessage(w))
        # Muffle when possible; if no restart, just continue.
        r <- tryCatch(findRestart("muffleWarning"), error = function(e) NULL)
        if (!is.null(r)) invokeRestart("muffleWarning")
      }
    )
  }

  # ---- Simulate ----
  sim_res <- tryCatch(
    .capture_warnings(.with_seed(
      seed,
      if (!is.null(names(formals(eng$simulate_fun))) &&
        "seed" %in% names(formals(eng$simulate_fun))) {
        eng$simulate_fun(scenario, seed = seed)
      } else {
        eng$simulate_fun(scenario)
      }
    )),
    error = function(e) {
      err <<- conditionMessage(e)
      NULL
    }
  )

  if (is.null(sim_res) || !is.data.frame(sim_res)) {
    row <- list(p_value = NA_real_, detected = FALSE, fit_ok = FALSE, singular = NA,
                warning = warn, error = err)
    return(list(row = row, fit = NULL, data = data))
  }
  data <- sim_res

  # ---- Fit ----
  fit_res <- tryCatch(
    .capture_warnings(eng$fit_fun(data, scenario)),
    error = function(e) {
      err <<- .append_warn(err, conditionMessage(e))
      NULL
    }
  )

  if (is.null(fit_res)) {
    row <- list(p_value = NA_real_, detected = FALSE, fit_ok = FALSE, singular = NA,
                warning = warn, error = err)
    return(list(row = row, fit = NULL, data = data))
  }

  fit <- fit_res
  fit_ok <- TRUE

  # Optional singular detection hook (backend-specific).
  if (!is.null(attr(fit, "singular"))) singular <- isTRUE(attr(fit, "singular"))

  # ---- Test ----
  test_res <- tryCatch(
    .capture_warnings(eng$test_fun(fit, scenario)),
    error = function(e) {
      err <<- .append_warn(err, conditionMessage(e))
      NULL
    }
  )

  if (!is.null(test_res) && !is.null(test_res$p_value)) {
    pval <- as.numeric(test_res$p_value)
    if (!is.finite(pval)) pval <- NA_real_
  } else {
    pval <- NA_real_
  }

  row <- list(
    p_value = pval,
    detected = !is.na(pval) && pval < alpha,
    fit_ok = fit_ok,
    singular = singular,
    warning = warn,
    error = err
  )
  list(row = row, fit = fit, data = data)
}

#' @export
print.mp_power <- function(x, ...) {
  cat("<mp_power>\n")
  cat(sprintf("  nsim: %d\n", x$nsim))
  cat(sprintf("  alpha: %g\n", x$alpha))
  cat(sprintf("  failure_policy: %s\n", x$failure_policy))
  cat(sprintf("  power: %s\n", ifelse(is.na(x$power), "NA", formatC(x$power, digits = 3, format = "f"))))
  cat(sprintf("  mcse: %s\n", ifelse(is.na(x$mcse), "NA", formatC(x$mcse, digits = 3, format = "f"))))
  cat(sprintf(
    "  %d%% CI: [%s, %s]\n",
    round(x$conf_level * 100),
    ifelse(is.na(x$ci[1]), "NA", formatC(x$ci[1], digits = 3, format = "f")),
    ifelse(is.na(x$ci[2]), "NA", formatC(x$ci[2], digits = 3, format = "f"))
  ))
  cat("  diagnostics:\n")
  cat(sprintf("    - fail_rate: %s\n", formatC(x$diagnostics$fail_rate, digits = 3, format = "f")))
  cat(sprintf("    - singular_rate: %s\n", formatC(x$diagnostics$singular_rate, digits = 3, format = "f")))
  invisible(x)
}

#' @export
summary.mp_power <- function(object, ...) {
  list(
    power = object$power,
    mcse = object$mcse,
    ci = object$ci,
    diagnostics = object$diagnostics,
    nsim = object$nsim,
    alpha = object$alpha,
    failure_policy = object$failure_policy,
    conf_level = object$conf_level
  )
}
