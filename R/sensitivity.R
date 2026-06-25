#' @noRd
mp_validate_variation_key <- function(key) {
  if (!is.character(key) || length(key) != 1L || is.na(key) || key == "") {
    stop("Each `vary` name must be a non-empty character string.", call. = FALSE)
  }

  path <- strsplit(key, "\\.", fixed = FALSE)[[1]]
  if (any(path == "")) {
    stop("Variation keys must not contain empty path segments.", call. = FALSE)
  }

  root <- path[[1]]
  allowed_roots <- c("fixed_effects", "random_effects", "residual_sd", "icc",
                     "clusters", "trials_per_cell", "extend")
  if (!root %in% allowed_roots) {
    stop("Unsupported variation key: ", key, call. = FALSE)
  }

  if (root %in% c("fixed_effects", "icc", "clusters") && length(path) < 2L) {
    stop("`", root, "` keys must include a subfield, e.g. `", root, ".name`.", call. = FALSE)
  }

  if (root == "extend" && length(path) != 2L) {
    stop("`extend` keys must be of the form `extend.<group>`.", call. = FALSE)
  }

  if (root == "random_effects") {
    ok <- (length(path) == 3L && path[[3]] %in% c("intercept_sd", "cor")) ||
      (length(path) == 4L && identical(path[[3]], "slopes"))
    if (!ok) {
      stop("`random_effects` keys must be `random_effects.<group>.intercept_sd`, ",
           "`random_effects.<group>.cor`, or ",
           "`random_effects.<group>.slopes.<predictor>`.", call. = FALSE)
    }
  }

  if (root %in% c("residual_sd", "trials_per_cell") && length(path) != 1L) {
    stop("`", root, "` must be provided as a top-level key without subfields.", call. = FALSE)
  }

  path
}

#' @noRd
mp_validate_vary <- function(vary) {
  if (!is.list(vary) || length(vary) == 0L || is.null(names(vary)) || any(names(vary) == "")) {
    stop("`vary` must be a named, non-empty list.", call. = FALSE)
  }

  for (nm in names(vary)) {
    mp_validate_variation_key(nm)

    values <- vary[[nm]]
    if (length(values) == 0L) {
      stop("Each `vary` entry must contain at least one value.", call. = FALSE)
    }

    if (is.list(values) && !is.data.frame(values)) {
      stop("`vary` values must be atomic vectors or factors, not lists.", call. = FALSE)
    }

    if (anyNA(values)) {
      stop("`vary` values must not contain `NA`.", call. = FALSE)
    }
  }

  invisible(TRUE)
}

#' @noRd
mp_set_nested_value <- function(x, path, value) {
  if (length(path) == 1L) {
    x[[path]] <- value
    return(x)
  }

  head <- path[[1]]
  tail <- path[-1]

  if (is.null(x[[head]])) {
    x[[head]] <- list()
  }

  x[[head]] <- mp_set_nested_value(x[[head]], tail, value)
  x
}

#' @noRd
mp_apply_variation <- function(scenario, key, value) {
  path <- strsplit(key, "\\.", fixed = FALSE)[[1]]
  root <- path[[1]]

  if (root %in% c("fixed_effects", "random_effects", "icc")) {
    scenario$assumptions <- mp_set_nested_value(scenario$assumptions, path, value)
    return(scenario)
  }

  if (root == "residual_sd") {
    scenario$assumptions$residual_sd <- value
    return(scenario)
  }

  if (root == "clusters") {
    scenario$design <- mp_set_nested_value(scenario$design, path, value)
    return(scenario)
  }

  if (root == "trials_per_cell") {
    scenario$design$trials_per_cell <- value
    return(scenario)
  }

  if (root == "extend") {
    group <- path[[2]]
    if (is.null(scenario$extend)) scenario$extend <- list()
    scenario$extend[[group]] <- as.integer(value)
    return(scenario)
  }

  stop("Unsupported variation key: ", key, call. = FALSE)
}

#' Run power sensitivity analysis over a parameter grid
#' @param scenario A base `mp_scenario` object.
#' @param vary Named list of vectors. Names are dotted paths such as
#'   `"fixed_effects.condition"` or `"clusters.subject"`.
#' @param nsim Number of simulations for each grid cell.
#' @param alpha Significance threshold.
#' @param seed Optional seed for reproducible cell-wise execution.
#' @param failure_policy Failure policy passed to [mp_power()].
#' @param conf_level Confidence level passed to [mp_power()].
#' @return An object of class `mp_sensitivity`.
#' @export
mp_sensitivity <- function(scenario,
                           vary,
                           nsim = 100,
                           alpha = 0.05,
                           seed = NULL,
                           failure_policy = c("count_as_nondetect", "exclude"),
                           conf_level = 0.95) {
  failure_policy <- match.arg(failure_policy)

  if (!inherits(scenario, "mp_scenario")) {
    stop("`scenario` must be an `mp_scenario` object.", call. = FALSE)
  }

  mp_validate_vary(vary)

  grid <- expand.grid(vary, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  n_cells <- nrow(grid)

  rows <- vector("list", n_cells)

  for (i in seq_len(n_cells)) {
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

    rows[[i]] <- data.frame(
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

#' @export
print.mp_sensitivity <- function(x, ...) {
  cat("<mp_sensitivity>\n")
  cat("  parameters:", paste(names(x$vary), collapse = ", "), "\n")
  cat("  grid cells:", nrow(x$grid), "\n")
  invisible(x)
}

#' @export
summary.mp_sensitivity <- function(object, ...) {
  object$results
}

#' Plot a sensitivity analysis
#'
#' For one varying parameter: line plot with optional CI segments when
#' `y = "estimate"`. For two varying parameters: heatmap. More than two
#' parameters is not supported.
#'
#' @param x An `mp_sensitivity` object.
#' @param y What to plot: `"estimate"` (power), `"failure_rate"`,
#'   `"singular_rate"`, or `"n_effective"`.
#' @param ... Additional graphical arguments passed to [graphics::plot()] (1D)
#'   or [graphics::image()] (2D).
#' @return Invisibly returns the plotted data (1D: ordered data frame; 2D: matrix).
#' @export
plot.mp_sensitivity <- function(x, y = c("estimate", "failure_rate", "singular_rate", "n_effective"), ...) {
  y <- match.arg(y)

  nv <- length(x$vary)
  if (nv == 1L) {
    param <- names(x$vary)[[1]]
    dat <- x$results[order(x$results[[param]]), , drop = FALSE]
    graphics::plot(dat[[param]], dat[[y]], xlab = param, ylab = y, ...)
    if (identical(y, "estimate")) {
      graphics::segments(
        x0 = dat[[param]],
        y0 = dat$conf_low,
        x1 = dat[[param]],
        y1 = dat$conf_high
      )
    }
    return(invisible(dat))
  }

  if (nv == 2L) {
    p1 <- names(x$vary)[[1]]
    p2 <- names(x$vary)[[2]]
    u1 <- sort(unique(x$results[[p1]]))
    u2 <- sort(unique(x$results[[p2]]))
    z <- matrix(
      NA_real_,
      nrow = length(u1),
      ncol = length(u2),
      dimnames = list(as.character(u1), as.character(u2))
    )
    for (i in seq_len(nrow(x$results))) {
      r <- x$results[i, , drop = FALSE]
      i1 <- match(r[[p1]], u1)
      i2 <- match(r[[p2]], u2)
      z[i1, i2] <- r[[y]]
    }
    graphics::image(u1, u2, z, xlab = p1, ylab = p2, ...)
    return(invisible(z))
  }

  stop("`plot.mp_sensitivity()` supports one varying parameter (line plot) or two (heatmap), not ", nv, ".", call. = FALSE)
}
