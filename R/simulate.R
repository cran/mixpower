#' Run a simple simulation-based power study
#' @param scenario A scenario object.
#' @param nsim Number of simulations.
#' @param seed Optional random seed.
#' @return A data.frame of simulated p-values.
#' @export
simulate_power <- function(scenario, nsim = 100, seed = NULL) {
  if (!is.null(seed)) {
    base::set.seed(seed)
  }

  data.frame(
    simulation = seq_len(nsim),
    p_value = stats::runif(nsim)
  )
}
