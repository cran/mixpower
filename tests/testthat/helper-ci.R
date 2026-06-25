is_ci_fast <- function() identical(Sys.getenv("CI_FAST"), "true")

# TRUE only when glmmTMB is installed AND its compiled TMB ABI is *positively*
# confirmed to match the installed TMB. A mismatch (glmmTMB built against a
# different TMB ABI) yields unreliable fits, so numeric cross-engine comparisons
# must skip rather than fail. We bias conservatively: any uncertainty (the
# internal check is unavailable, errors, or emits the build-version warning) is
# treated as NOT ok, so the fragile numeric test only runs on a known-good env.
glmmtmb_tmb_ok <- function() {
  if (!requireNamespace("glmmTMB", quietly = TRUE)) {
    return(FALSE)
  }
  saw_warning <- FALSE
  ok <- withCallingHandlers(
    tryCatch(
      glmmTMB:::check_dep_version(dep_pkg = "TMB"),
      error = function(e) NA
    ),
    warning = function(w) {
      saw_warning <<- TRUE
      invokeRestart("muffleWarning")
    }
  )
  isTRUE(ok) && !saw_warning
}
