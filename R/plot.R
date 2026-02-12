#' Plot power results
#' @param results A data.frame with effect and power columns.
#' @param ... Additional arguments passed to plot.
#' @return Invisibly returns the plot data.
#' @export
plot_power <- function(results, ...) {
  graphics::plot(results$effect, results$power, ...)
  invisible(results)
}
