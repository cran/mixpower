#' RNG helpers for reproducible simulation
#' @noRd

# Evaluate an expression under a specific seed.
# If seed is NULL, no seed is set (not reproducible).
.with_seed <- function(seed, expr) {
  if (is.null(seed)) return(force(expr))

  if (!is.numeric(seed) || length(seed) != 1 || is.na(seed)) {
    .stop("`seed` must be a single non-missing number or NULL.")
  }

  seed <- as.integer(seed)
  old_seed <- NULL
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    if (!is.null(old_seed)) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    }
  }, add = TRUE)

  base::set.seed(seed)
  force(expr)
}

# Derive deterministic per-replicate seeds for sequential simulation.
.rep_seeds <- function(seed, nsim) {
  if (is.null(seed)) return(rep(NA_integer_, nsim))
  seed <- as.integer(seed)
  seed + seq_len(nsim) - 1L
}
