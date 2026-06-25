#' Placeholder for parallel execution
#' @param fun Function to run.
#' @param ... Additional arguments to pass to fun.
#' @return The result of fun.
#' @export
run_parallel <- function(fun, ...) {
  fun(...)
}

#' Create a PSOCK cluster with mixpower loaded on workers
#'
#' Caller is responsible for [parallel::stopCluster()] (typically via
#' `on.exit(..., add = TRUE)`).
#'
#' @param workers Number of workers (integer >= 1).
#' @return A cluster object from [parallel::makeCluster()].
#' @keywords internal
#' @export
mp_parallel_cluster <- function(workers) {
  if (!requireNamespace("parallel", quietly = TRUE)) {
    stop("Package \"parallel\" is required for parallel execution.", call. = FALSE)
  }
  workers <- as.integer(workers)
  if (is.na(workers) || workers < 1L) {
    stop("`workers` must be at least 1.", call. = FALSE)
  }
  cl <- parallel::makeCluster(workers)
  parallel::clusterEvalQ(cl, suppressPackageStartupMessages(library(mixpower, character.only = TRUE)))
  cl
}
