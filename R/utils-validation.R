#' Internal validation helpers
#' @noRd

.stop <- function(msg, call. = FALSE) {
  stop(msg, call. = call.)
}

.assert_is_scalar <- function(x, name) {
  if (length(x) != 1) .stop(sprintf("`%s` must be length 1.", name))
  invisible(TRUE)
}

.assert_is_flag <- function(x, name) {
  .assert_is_scalar(x, name)
  if (!is.logical(x) || is.na(x)) .stop(sprintf("`%s` must be TRUE/FALSE (not NA).", name))
  invisible(TRUE)
}

.assert_is_pos_int <- function(x, name, allow_zero = FALSE) {
  .assert_is_scalar(x, name)
  if (!is.numeric(x) || is.na(x) || x %% 1 != 0) .stop(sprintf("`%s` must be an integer.", name))
  if (allow_zero) {
    if (x < 0) .stop(sprintf("`%s` must be >= 0.", name))
  } else {
    if (x <= 0) .stop(sprintf("`%s` must be > 0.", name))
  }
  invisible(TRUE)
}

.assert_is_nonneg_num <- function(x, name) {
  .assert_is_scalar(x, name)
  if (!is.numeric(x) || is.na(x) || x < 0) .stop(sprintf("`%s` must be a non-negative number.", name))
  invisible(TRUE)
}

.assert_is_num <- function(x, name) {
  .assert_is_scalar(x, name)
  if (!is.numeric(x) || is.na(x)) .stop(sprintf("`%s` must be numeric (not NA).", name))
  invisible(TRUE)
}

.assert_named_list <- function(x, name) {
  if (!is.list(x)) .stop(sprintf("`%s` must be a list.", name))
  nms <- names(x)
  if (is.null(nms) || any(nms == "")) .stop(sprintf("`%s` must be a *named* list.", name))
  invisible(TRUE)
}

.assert_in <- function(x, choices, name) {
  .assert_is_scalar(x, name)
  if (!x %in% choices) {
    .stop(sprintf("`%s` must be one of: %s.", name, paste(choices, collapse = ", ")))
  }
  invisible(TRUE)
}

.assert_class <- function(x, cls, name) {
  if (!inherits(x, cls)) .stop(sprintf("`%s` must inherit from class '%s'.", name, cls))
  invisible(TRUE)
}

# Simple check for "function or NULL"
.assert_fun_or_null <- function(x, name) {
  if (!is.null(x) && !is.function(x)) .stop(sprintf("`%s` must be a function or NULL.", name))
  invisible(TRUE)
}
