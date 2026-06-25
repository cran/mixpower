#' Parallel power curve evaluation
#'
#' Evaluates power over a one-parameter grid by running [mp_power()] for each
#' grid cell in parallel. Uses explicit per-cell seeds (seed + cell_index - 1L)
#' so results are deterministic and match serial [mp_power_curve()] for the same
#' seed. Does not modify [mp_power()]; parallelization is at the scenario-grid
#' level only.
#'
#' @param scenario An `mp_scenario`.
#' @param vary Named list with exactly one parameter (e.g. `clusters.subject`).
#' @param workers Number of parallel workers (default 2).
#' @param nsim Number of simulations per grid point (default 100).
#' @param alpha Significance level (default 0.05).
#' @param seed Optional base seed; each cell gets `seed + cell_index - 1L`.
#' @param failure_policy How to treat failed fits: `"count_as_nondetect"` or `"exclude"`.
#' @param conf_level Confidence level for power intervals (default 0.95).
#' @param progress If `TRUE`, run serially with a progress bar; if `FALSE`, run in parallel.
#' @param ... Unused; reserved for future arguments.
#' @return An object of class `mp_power_curve` (same structure as [mp_power_curve()]).
#' @note Parallel execution requires the \pkg{parallel} package (base R) and that
#'   \pkg{mixpower} is installed (e.g. \code{install.packages()} or
#'   \code{devtools::install()}) so that workers can load it.
#' @export
mp_power_curve_parallel <- function(scenario,
                                     vary,
                                     workers = 2L,
                                     nsim = 100,
                                     alpha = 0.05,
                                     seed = NULL,
                                     failure_policy = c("count_as_nondetect", "exclude"),
                                     conf_level = 0.95,
                                     progress = FALSE,
                                     ...) {
  failure_policy <- match.arg(failure_policy)

  if (!inherits(scenario, "mp_scenario")) {
    stop("`scenario` must be an `mp_scenario` object.", call. = FALSE)
  }
  if (!is.list(vary) || length(vary) != 1L || is.null(names(vary)) || names(vary) == "") {
    stop("`vary` must be a named list with exactly one entry.", call. = FALSE)
  }

  mp_validate_vary(vary)
  grid <- expand.grid(vary, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  n_cells <- nrow(grid)
  param <- names(vary)[[1]]
  vals <- grid[[param]]

  if (progress) {
    rows <- vector("list", n_cells)
    pb <- utils::txtProgressBar(0L, n_cells, style = 3L)
    on.exit(close(pb), add = TRUE)
    for (i in seq_len(n_cells)) {
      seed_i <- if (is.null(seed)) NULL else seed + i - 1L
      scn_i <- mp_apply_variation(scenario, param, vals[[i]])
      out <- mp_power(
        scenario = scn_i,
        nsim = nsim,
        alpha = alpha,
        seed = seed_i,
        failure_policy = failure_policy,
        conf_level = conf_level
      )
      n_effective <- if (failure_policy == "exclude") {
        sum(!is.na(out$sims$p_value))
      } else {
        out$nsim
      }
      row <- list()
      row[[param]] <- vals[[i]]
      row$estimate <- out$power
      row$mcse <- out$mcse
      row$conf_low <- out$ci[1]
      row$conf_high <- out$ci[2]
      row$failure_rate <- out$diagnostics$fail_rate
      row$singular_rate <- out$diagnostics$singular_rate
      row$n_effective <- n_effective
      row$nsim <- out$nsim
      rows[[i]] <- as.data.frame(row, stringsAsFactors = FALSE)
      utils::setTxtProgressBar(pb, i)
    }
    dat <- do.call(rbind, rows)
  } else {
    cl <- mp_parallel_cluster(workers)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterExport(
      cl,
      c("scenario", "param", "vals", "nsim", "alpha", "seed", "failure_policy", "conf_level"),
      envir = environment()
    )

    rows <- parallel::parLapply(cl, seq_len(n_cells), function(i) {
      seed_i <- if (is.null(seed)) NULL else seed + i - 1L
      scn_i <- mp_apply_variation(scenario, param, vals[[i]])
      out <- mp_power(
        scenario = scn_i,
        nsim = nsim,
        alpha = alpha,
        seed = seed_i,
        failure_policy = failure_policy,
        conf_level = conf_level
      )
      n_effective <- if (failure_policy == "exclude") {
        sum(!is.na(out$sims$p_value))
      } else {
        out$nsim
      }
      row <- list()
      row[[param]] <- vals[[i]]
      row$estimate <- out$power
      row$mcse <- out$mcse
      row$conf_low <- out$ci[1]
      row$conf_high <- out$ci[2]
      row$failure_rate <- out$diagnostics$fail_rate
      row$singular_rate <- out$diagnostics$singular_rate
      row$n_effective <- n_effective
      row$nsim <- out$nsim
      as.data.frame(row, stringsAsFactors = FALSE)
    })

    dat <- do.call(rbind, rows)
  }

  rownames(dat) <- NULL
  out <- list(
    vary = vary,
    grid = grid,
    results = dat,
    alpha = alpha,
    failure_policy = failure_policy,
    conf_level = conf_level
  )
  class(out) <- "mp_power_curve"
  out
}
