#' @noRd
.sensitivity_checkpoint_manifest <- function(scenario, vary, nsim, alpha, failure_policy, conf_level, seed) {
  list(
    version = 1L,
    vary = vary,
    nsim = as.integer(nsim),
    alpha = alpha,
    failure_policy = failure_policy,
    conf_level = conf_level,
    seed = seed,
    scenario = list(
      formula = deparse(scenario$formula),
      design = scenario$design,
      assumptions = scenario$assumptions,
      test = scenario$test
    )
  )
}

#' @noRd
.sensitivity_checkpoint_paths <- function(checkpoint_dir) {
  d <- normalizePath(checkpoint_dir, winslash = "/", mustWork = FALSE)
  list(
    dir = d,
    manifest = file.path(d, "_mixpower_sensitivity_manifest.rds")
  )
}

#' Run one sensitivity grid cell (for serial and parallel workers)
#'
#' @keywords internal
#' @export
mp_sensitivity_cell_run <- function(i,
                                    scenario,
                                    vary,
                                    grid,
                                    nsim,
                                    alpha,
                                    seed,
                                    failure_policy,
                                    conf_level,
                                    checkpoint_dir,
                                    resume) {
  cell_path <- if (!is.null(checkpoint_dir)) {
    file.path(checkpoint_dir, sprintf("cell_%05d.rds", i))
  } else {
    NULL
  }

  if (!is.null(cell_path) && isTRUE(resume) && file.exists(cell_path)) {
    saved <- readRDS(cell_path)
    if (inherits(saved, "data.frame") && nrow(saved) == 1L) {
      return(saved)
    }
  }

  scenario_i <- scenario
  for (nm in names(vary)) {
    scenario_i <- mp_apply_variation(scenario_i, nm, grid[[nm]][[i]])
  }

  seed_i <- if (is.null(seed)) NULL else seed + i - 1L

  power_i <- mp_power(
    scenario = scenario_i,
    nsim = nsim,
    alpha = alpha,
    seed = seed_i,
    failure_policy = failure_policy,
    conf_level = conf_level
  )

  n_effective <- if (failure_policy == "exclude") {
    sum(!is.na(power_i$sims$p_value))
  } else {
    power_i$nsim
  }

  row <- data.frame(
    grid[i, , drop = FALSE],
    estimate = power_i$power,
    mcse = power_i$mcse,
    conf_low = power_i$ci[1],
    conf_high = power_i$ci[2],
    failure_rate = power_i$diagnostics$fail_rate,
    singular_rate = power_i$diagnostics$singular_rate,
    n_effective = n_effective,
    nsim = power_i$nsim,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (!is.null(cell_path)) {
    saveRDS(row, cell_path)
  }

  row
}

#' Parallel sensitivity analysis over a parameter grid
#'
#' Like [mp_sensitivity()], but evaluates each grid cell in parallel (or with a
#' progress bar when `progress = TRUE`). Uses per-cell seeds `seed + cell_index - 1L`
#' to match a serial ordering convention. Does not modify [mp_power()].
#'
#' @inheritParams mp_sensitivity
#' @param workers Number of parallel workers when `progress = FALSE` (default 2).
#' @param progress If `TRUE`, run serially with a text progress bar.
#' @param checkpoint_dir Optional directory to save per-cell RDS results and a
#'   manifest. When `resume = TRUE`, existing cell files are reused if the manifest
#'   matches the current run. Use a path on **shared storage** if `workers > 1`.
#' @param resume Logical; only used when `checkpoint_dir` is set.
#' @param ... Reserved.
#'
#' @return An object of class `mp_sensitivity` (same structure as [mp_sensitivity()]).
#' @export
#' @note Parallel execution requires the \pkg{parallel} package and that \pkg{mixpower}
#'   can be loaded on workers (installed package).
mp_sensitivity_parallel <- function(scenario,
                                   vary,
                                   nsim = 100,
                                   alpha = 0.05,
                                   seed = NULL,
                                   failure_policy = c("count_as_nondetect", "exclude"),
                                   conf_level = 0.95,
                                   workers = 2L,
                                   progress = FALSE,
                                   checkpoint_dir = NULL,
                                   resume = TRUE,
                                   ...) {
  failure_policy <- match.arg(failure_policy)

  if (!inherits(scenario, "mp_scenario")) {
    stop("`scenario` must be an `mp_scenario` object.", call. = FALSE)
  }

  mp_validate_vary(vary)

  grid <- expand.grid(vary, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  n_cells <- nrow(grid)

  use_resume <- resume
  if (!is.null(checkpoint_dir)) {
    dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
    paths <- .sensitivity_checkpoint_paths(checkpoint_dir)
    manifest <- .sensitivity_checkpoint_manifest(
      scenario, vary, nsim, alpha, failure_policy, conf_level, seed
    )
    if (isTRUE(resume) && file.exists(paths$manifest)) {
      old <- readRDS(paths$manifest)
      if (!identical(old$manifest, manifest)) {
        warning(
          "Checkpoint manifest mismatch; recomputing all cells. ",
          "Remove `checkpoint_dir` or set `resume = FALSE` to ignore old checkpoints.",
          call. = FALSE
        )
        use_resume <- FALSE
      }
    }
  } else {
    paths <- NULL
    manifest <- NULL
  }

  ck_dir <- if (!is.null(checkpoint_dir)) paths$dir else NULL

  run_i <- function(i) {
    mp_sensitivity_cell_run(
      i = i,
      scenario = scenario,
      vary = vary,
      grid = grid,
      nsim = nsim,
      alpha = alpha,
      seed = seed,
      failure_policy = failure_policy,
      conf_level = conf_level,
      checkpoint_dir = ck_dir,
      resume = use_resume
    )
  }

  if (progress) {
    rows <- vector("list", n_cells)
    pb <- utils::txtProgressBar(0L, n_cells, style = 3L)
    on.exit(close(pb), add = TRUE)
    for (i in seq_len(n_cells)) {
      rows[[i]] <- run_i(i)
      utils::setTxtProgressBar(pb, i)
    }
  } else {
    cl <- mp_parallel_cluster(workers)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterExport(
      cl,
      c(
        "scenario", "vary", "grid", "nsim", "alpha", "seed",
        "failure_policy", "conf_level", "ck_dir", "use_resume"
      ),
      envir = environment()
    )
    rows <- parallel::parLapply(cl, seq_len(n_cells), function(i) {
      mp_sensitivity_cell_run(
        i = i,
        scenario = scenario,
        vary = vary,
        grid = grid,
        nsim = nsim,
        alpha = alpha,
        seed = seed,
        failure_policy = failure_policy,
        conf_level = conf_level,
        checkpoint_dir = ck_dir,
        resume = use_resume
      )
    })
  }

  if (!is.null(checkpoint_dir)) {
    saveRDS(list(manifest = manifest), paths$manifest)
  }

  out <- list(
    vary = vary,
    grid = grid,
    results = do.call(rbind, rows),
    alpha = alpha,
    failure_policy = failure_policy,
    conf_level = conf_level
  )
  class(out) <- "mp_sensitivity"
  out
}
