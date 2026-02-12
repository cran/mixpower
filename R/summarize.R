#' Summarize simulation outputs
#' @param simulations A data.frame of simulations.
#' @return A summary data.frame.
#' @export
summarize_simulations <- function(simulations) {
  data.frame(
    n = nrow(simulations),
    mean_p = mean(simulations$p_value, na.rm = TRUE),
    sd_p = stats::sd(simulations$p_value, na.rm = TRUE)
  )
}
