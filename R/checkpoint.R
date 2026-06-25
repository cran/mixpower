#' Resumable, checkpointed power simulation
#'
#' Runs [mp_power()] in batches, saving the accumulated per-replicate results to
#' `file` (an `.rds`) after each batch. If `file` already exists for the same
#' `seed`, the run resumes from where it left off and only the remaining
#' replicates are simulated. This makes very large or long-running power
#' analyses robust to interruption, and lets you grow `nsim` later without
#' recomputing finished replicates.
#'
#' Because replicate seeds are deterministic (`seed + i - 1`), the result is
#' identical to a single `mp_power(scenario, nsim, seed = seed)` call: batching
#' only changes when work happens, not what is computed.
#'
#' @param scenario An `mp_scenario`.
#' @param nsim Total number of simulations desired.
#' @param file Path to the checkpoint `.rds` file.
#' @param batch_size Replicates per batch (default 100).
#' @param alpha,seed,failure_policy,conf_level,ci_method As in [mp_power()].
#'   `seed` must be non-NULL for reproducible, resumable batches.
#' @param progress Emit a progress message after each batch (default `TRUE`).
#' @return An object of class `mp_power` for all `nsim` replicates.
#' @seealso [mp_power()].
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   d <- mp_design(list(subject = 30), trials_per_cell = 6)
#'   a <- mp_assumptions(list("(Intercept)" = 0, condition = 0.4),
#'                       random_effects = list(subject = list(intercept_sd = 0.5)),
#'                       residual_sd = 1)
#'   scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
#'   f <- tempfile(fileext = ".rds")
#'   mp_power_checkpoint(scn, nsim = 60, file = f, batch_size = 20, seed = 1)
#' }
#' }
mp_power_checkpoint <- function(scenario,
                                nsim,
                                file,
                                batch_size = 100,
                                alpha = 0.05,
                                seed = NULL,
                                failure_policy = c("count_as_nondetect", "exclude"),
                                conf_level = 0.95,
                                ci_method = c("clopper-pearson", "wald"),
                                progress = TRUE) {
  failure_policy <- match.arg(failure_policy)
  ci_method <- match.arg(ci_method)
  .assert_class(scenario, "mp_scenario", "scenario")
  .assert_is_pos_int(nsim, "nsim")
  .assert_is_pos_int(batch_size, "batch_size")
  if (is.null(seed)) {
    .stop("`mp_power_checkpoint()` requires a non-NULL `seed` for resumable, reproducible batches.")
  }
  if (!is.character(file) || length(file) != 1L) {
    .stop("`file` must be a single file path.")
  }

  sims <- NULL
  if (file.exists(file)) {
    ck <- tryCatch(readRDS(file), error = function(e) NULL)
    if (!is.null(ck) && identical(ck$seed, as.integer(seed)) && is.data.frame(ck$sims)) {
      sims <- ck$sims
    }
  }
  n_done <- if (is.null(sims)) 0L else nrow(sims)

  while (n_done < nsim) {
    this_n <- min(batch_size, nsim - n_done)
    batch <- mp_power(
      scenario, nsim = this_n, alpha = alpha, seed = as.integer(seed) + n_done,
      failure_policy = failure_policy, conf_level = conf_level,
      ci_method = ci_method, keep = "minimal", aggregate = "full"
    )
    sims <- if (is.null(sims)) batch$sims else rbind(sims, batch$sims)
    n_done <- n_done + this_n
    saveRDS(list(sims = sims, seed = as.integer(seed)), file)
    if (isTRUE(progress)) {
      message(sprintf("mp_power_checkpoint: %d / %d replicates done", n_done, nsim))
    }
  }

  if (nrow(sims) > nsim) sims <- sims[seq_len(nsim), , drop = FALSE]
  .mp_aggregate_sims(
    sims, scenario, alpha, as.integer(seed), failure_policy, "minimal",
    conf_level, ci_method, "full", nsim, .mp_true_effect(scenario), NULL, NULL
  )
}
