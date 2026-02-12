#' Reproducibility manifest for power analyses
#'
#' Captures scenario fingerprint, seed strategy, session info, timestamp, and
#' optional git SHA so results can be reproduced or audited. Output is a plain
#' list (and one-row data frame via [as.data.frame()]) suitable for saving
#' alongside results.
#'
#' @param scenario An `mp_scenario` object (used for digest).
#' @param seed The seed value used (or `NULL`). Stored as-is; strategy is
#'   inferred as `"fixed"` if non-null else `"none"`.
#' @param session Include full `sessionInfo()` (default `TRUE`). If `FALSE`,
#'   only R version and mixpower version are stored.
#' @return A list with components: `scenario_digest`, `seed`, `seed_strategy`,
#'   `timestamp`, `r_version`, `mixpower_version`, `session_info` (if requested),
#'   `git_sha` (if in a git repo). Use [as.data.frame()] on the list for a
#'   single-row table (list components become columns where possible).
#' @export
mp_manifest <- function(scenario,
                        seed = NULL,
                        session = TRUE) {
  if (!inherits(scenario, "mp_scenario")) {
    stop("`scenario` must be an `mp_scenario` object.", call. = FALSE)
  }

  scenario_digest <- if (requireNamespace("digest", quietly = TRUE)) {
    fingerprint <- list(
      formula = deparse(scenario$formula),
      design = scenario$design,
      assumptions = scenario$assumptions,
      test = scenario$test
    )
    digest::digest(fingerprint, algo = "sha256")
  } else {
    NA_character_
  }

  seed_strategy <- if (is.null(seed)) "none" else "fixed"
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

  si <- utils::sessionInfo()
  r_version <- paste(si$R.version$major, si$R.version$minor, sep = ".")
  pkg <- si$otherPkgs
  mixpower_version <- if (!is.null(pkg$mixpower)) pkg$mixpower$Version else utils::packageVersion("mixpower")

  session_info <- if (session) {
    utils::capture.output(print(si))
  } else {
    character(0)
  }

  git_sha <- NA_character_
  tryCatch({
    out <- system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = NULL)
    if (length(out) == 1L && nzchar(out)) git_sha <- out[[1]]
  }, error = function(e) NULL)

  out <- list(
    scenario_digest = scenario_digest,
    seed = seed,
    seed_strategy = seed_strategy,
    timestamp = timestamp,
    r_version = r_version,
    mixpower_version = as.character(mixpower_version),
    session_info = list(session_info),
    git_sha = git_sha
  )
  class(out) <- "mp_manifest"
  out
}

#' @export
print.mp_manifest <- function(x, ...) {
  cat("<mp_manifest>\n")
  cat("  scenario_digest:", if (is.na(x$scenario_digest)) "(digest pkg not installed)" else substr(x$scenario_digest, 1, 16), "...\n")
  cat("  seed:", if (is.null(x$seed)) "NULL" else x$seed, " (", x$seed_strategy, ")\n", sep = "")
  cat("  timestamp:", x$timestamp, "\n")
  cat("  r_version:", x$r_version, "\n")
  cat("  mixpower_version:", x$mixpower_version, "\n")
  cat("  git_sha:", if (is.na(x$git_sha)) "NA" else substr(x$git_sha, 1, 7), "\n")
  invisible(x)
}

#' Bundle results with manifest and optional labels
#'
#' Combines a single result object ([mp_power], [mp_sensitivity], or
#' [mp_power_curve]), a reproducibility manifest, and optional user labels
#' into one object. Diagnostics and result structure are retained unchanged.
#'
#' @param result An object of class `mp_power`, `mp_sensitivity`, or `mp_power_curve`.
#' @param manifest An `mp_manifest` object (from [mp_manifest()]).
#' @param study_id Optional character; study or run identifier.
#' @param analyst Optional character; analyst name or ID.
#' @param notes Optional character; free-form notes.
#' @return An object of class `mp_bundle` with components `result`, `manifest`,
#'   and `labels` (list with `study_id`, `analyst`, `notes`).
#' @export
mp_bundle_results <- function(result,
                              manifest,
                              study_id = NULL,
                              analyst = NULL,
                              notes = NULL) {
  if (!inherits(manifest, "mp_manifest")) {
    stop("`manifest` must be an `mp_manifest` object.", call. = FALSE)
  }
  ok <- inherits(result, "mp_power") ||
    inherits(result, "mp_sensitivity") ||
    inherits(result, "mp_power_curve")
  if (!ok) {
    stop("`result` must be an `mp_power`, `mp_sensitivity`, or `mp_power_curve` object.", call. = FALSE)
  }

  labels <- list(
    study_id = if (is.null(study_id)) NA_character_ else as.character(study_id),
    analyst = if (is.null(analyst)) NA_character_ else as.character(analyst),
    notes = if (is.null(notes)) NA_character_ else as.character(notes)
  )

  out <- list(
    result = result,
    manifest = manifest,
    labels = labels
  )
  class(out) <- "mp_bundle"
  out
}

#' @export
print.mp_bundle <- function(x, ...) {
  cat("<mp_bundle>\n")
  cat("  result:", class(x$result)[1], "\n")
  cat("  study_id:", x$labels$study_id, "\n")
  cat("  analyst:", x$labels$analyst, "\n")
  print(x$manifest)
  invisible(x)
}

#' @export
summary.mp_bundle <- function(object, ...) {
  list(
    result_class = class(object$result)[1],
    labels = object$labels,
    manifest = object$manifest
  )
}

#' Publication-ready summary table for power results
#'
#' Returns a flat data frame with power estimate, CI, failure/singularity rates,
#' and effective simulation counts. Works with [mp_power], [mp_sensitivity],
#' [mp_power_curve], or the result of [mp_bundle_results()] (uses the bundled result).
#'
#' @param x An object of class `mp_power`, `mp_sensitivity`, `mp_power_curve`, or from [mp_bundle_results()].
#' @param ... Unused; reserved for future arguments.
#' @return A data frame: for `mp_power` one row; for sensitivity/curve one row
#'   per grid cell with parameter column(s), `power_estimate`, `ci_low`, `ci_high`,
#'   `failure_rate`, `singular_rate`, `n_effective`, `nsim`.
#' @export
mp_report_table <- function(x, ...) {
  if (inherits(x, "mp_bundle")) {
    x <- x$result
  }

  if (inherits(x, "mp_power")) {
    n_effective <- if (x$failure_policy == "exclude") {
      sum(!is.na(x$sims$p_value))
    } else {
      x$nsim
    }
    return(data.frame(
      power_estimate = x$power,
      ci_low = x$ci[1],
      ci_high = x$ci[2],
      failure_rate = x$diagnostics$fail_rate,
      singular_rate = x$diagnostics$singular_rate,
      n_effective = n_effective,
      nsim = x$nsim,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  if (inherits(x, "mp_sensitivity") || inherits(x, "mp_power_curve")) {
    r <- x$results
    out <- data.frame(
      r[, setdiff(names(r), c("estimate", "mcse", "conf_low", "conf_high", "failure_rate", "singular_rate", "n_effective", "nsim")), drop = FALSE],
      power_estimate = r$estimate,
      ci_low = r$conf_low,
      ci_high = r$conf_high,
      failure_rate = r$failure_rate,
      singular_rate = r$singular_rate,
      n_effective = r$n_effective,
      nsim = r$nsim,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    return(out)
  }

  stop("`x` must be mp_power, mp_sensitivity, mp_power_curve, or mp_bundle.", call. = FALSE)
}

#' Write results or bundle to CSV or JSON
#'
#' Writes the report table (and for bundles, manifest/labels) to file. CSV writes
#' the publication-ready table only; JSON writes report table plus manifest and
#' labels when `x` is an `mp_bundle`.
#'
#' @param x An object from [mp_bundle_results()], or `mp_power`, `mp_sensitivity`, or `mp_power_curve`.
#' @param file Path to output file (extension need not match format).
#' @param format `"csv"` or `"json"`.
#' @param ... For CSV, arguments passed to [utils::write.csv()] (e.g. `row.names = FALSE`).
#' @return Invisibly the path `file`.
#' @export
mp_write_results <- function(x, file, format = c("csv", "json"), ...) {
  format <- match.arg(format)

  if (format == "csv") {
    tab <- mp_report_table(x)
    utils::write.csv(tab, file, ...)
    return(invisible(file))
  }

  if (format == "json") {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("Package \"jsonlite\" is required for JSON export. Install with install.packages(\"jsonlite\").", call. = FALSE)
    }
    report <- mp_report_table(x)
    payload <- list(report = report)
    if (inherits(x, "mp_bundle")) {
      payload$manifest <- list(
        scenario_digest = x$manifest$scenario_digest,
        seed = x$manifest$seed,
        seed_strategy = x$manifest$seed_strategy,
        timestamp = x$manifest$timestamp,
        r_version = x$manifest$r_version,
        mixpower_version = x$manifest$mixpower_version,
        git_sha = x$manifest$git_sha
      )
      payload$labels <- x$labels
    }
    jsonlite::write_json(payload, file, auto_unbox = TRUE, pretty = TRUE)
    return(invisible(file))
  }

  invisible(file)
}
