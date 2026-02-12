test_that("mp_manifest is deterministic for fixed inputs", {
  d <- mp_design(clusters = list(subject = 20), trials_per_cell = 2)
  a <- mp_assumptions(fixed_effects = list(condition = 0.3), residual_sd = 1)
  scn <- mp_scenario(
    y ~ condition + (1 | subject),
    design = d,
    assumptions = a,
    test = "custom",
    simulate_fun = function(s) data.frame(y = rnorm(20), condition = rep(c(0, 1), 10), subject = rep(1:10, 2)),
    fit_fun = function(dat, s) stats::lm(y ~ condition, data = dat),
    test_fun = function(fit, s) list(p_value = as.numeric(coef(summary(fit))["condition", "Pr(>|t|)"]))
  )

  m1 <- mp_manifest(scn, seed = 42, session = FALSE)
  m2 <- mp_manifest(scn, seed = 42, session = FALSE)
  expect_identical(m1$scenario_digest, m2$scenario_digest)
  expect_identical(m1$seed, m2$seed)
  expect_identical(m1$seed_strategy, m2$seed_strategy)
  expect_identical(m1$r_version, m2$r_version)
  expect_identical(m1$mixpower_version, m2$mixpower_version)
})

test_that("mp_manifest stores seed strategy correctly", {
  d <- mp_design(clusters = list(subject = 10), trials_per_cell = 2)
  a <- mp_assumptions(fixed_effects = list(condition = 0.2), residual_sd = 1)
  scn <- mp_scenario(
    y ~ condition,
    design = d,
    assumptions = a,
    test = "wald"
  )
  m_none <- mp_manifest(scn, seed = NULL, session = FALSE)
  m_fixed <- mp_manifest(scn, seed = 123, session = FALSE)
  expect_identical(m_none$seed_strategy, "none")
  expect_identical(m_fixed$seed_strategy, "fixed")
  expect_identical(m_fixed$seed, 123)
})

test_that("mp_bundle_results retains result and diagnostics untouched", {
  d <- mp_design(clusters = list(subject = 20), trials_per_cell = 2)
  a <- mp_assumptions(fixed_effects = list(condition = 0.3), residual_sd = 1)
  scn <- mp_scenario(
    y ~ condition + (1 | subject),
    design = d,
    assumptions = a,
    test = list(term = "condition"),
    simulate_fun = function(s) data.frame(y = rnorm(20), condition = rep(c(0, 1), 10), subject = rep(1:10, 2)),
    fit_fun = function(dat, s) stats::lm(y ~ condition, data = dat),
    test_fun = function(fit, s) list(p_value = as.numeric(coef(summary(fit))["condition", "Pr(>|t|)"]))
  )
  res <- mp_power(scn, nsim = 6, seed = 1)
  manifest <- mp_manifest(scn, seed = 1, session = FALSE)
  bundle <- mp_bundle_results(res, manifest, study_id = "S1", analyst = "A1", notes = "test")

  expect_identical(bundle$result$power, res$power)
  expect_identical(bundle$result$diagnostics$fail_rate, res$diagnostics$fail_rate)
  expect_identical(bundle$result$diagnostics$singular_rate, res$diagnostics$singular_rate)
  expect_identical(bundle$result$nsim, res$nsim)
  expect_identical(bundle$labels$study_id, "S1")
  expect_identical(bundle$labels$analyst, "A1")
  expect_identical(bundle$labels$notes, "test")
})

test_that("mp_report_table returns correct structure for mp_power and curve", {
  d <- mp_design(clusters = list(subject = 20), trials_per_cell = 2)
  a <- mp_assumptions(fixed_effects = list(condition = 0.3), residual_sd = 1)
  scn <- mp_scenario(
    y ~ condition + (1 | subject),
    design = d,
    assumptions = a,
    test = list(term = "condition"),
    simulate_fun = function(s) data.frame(y = rnorm(20), condition = rep(c(0, 1), 10), subject = rep(1:10, 2)),
    fit_fun = function(dat, s) stats::lm(y ~ condition, data = dat),
    test_fun = function(fit, s) list(p_value = as.numeric(coef(summary(fit))["condition", "Pr(>|t|)"]))
  )
  res <- mp_power(scn, nsim = 4, seed = 1)
  tab <- mp_report_table(res)
  expect_equal(nrow(tab), 1)
  expect_true(all(c("power_estimate", "ci_low", "ci_high", "failure_rate", "singular_rate", "n_effective", "nsim") %in% names(tab)))
  expect_equal(tab$power_estimate, res$power)
  expect_equal(tab$failure_rate, res$diagnostics$fail_rate)

  curve <- mp_power_curve(scn, vary = list(`clusters.subject` = c(10, 20)), nsim = 4, seed = 1)
  tab2 <- mp_report_table(curve)
  expect_equal(nrow(tab2), 2)
  expect_true("clusters.subject" %in% names(tab2))
})

test_that("mp_report_table works with mp_bundle", {
  d <- mp_design(clusters = list(subject = 20), trials_per_cell = 2)
  a <- mp_assumptions(fixed_effects = list(condition = 0.3), residual_sd = 1)
  scn <- mp_scenario(
    y ~ condition + (1 | subject),
    design = d,
    assumptions = a,
    test = list(term = "condition"),
    simulate_fun = function(s) data.frame(y = rnorm(20), condition = rep(c(0, 1), 10), subject = rep(1:10, 2)),
    fit_fun = function(dat, s) stats::lm(y ~ condition, data = dat),
    test_fun = function(fit, s) list(p_value = as.numeric(coef(summary(fit))["condition", "Pr(>|t|)"]))
  )
  res <- mp_power(scn, nsim = 4, seed = 1)
  manifest <- mp_manifest(scn, seed = 1, session = FALSE)
  bundle <- mp_bundle_results(res, manifest)
  tab <- mp_report_table(bundle)
  expect_equal(nrow(tab), 1)
  expect_equal(tab$power_estimate, res$power)
})

test_that("round-trip write/read for CSV and JSON", {
  d <- mp_design(clusters = list(subject = 20), trials_per_cell = 2)
  a <- mp_assumptions(fixed_effects = list(condition = 0.3), residual_sd = 1)
  scn <- mp_scenario(
    y ~ condition + (1 | subject),
    design = d,
    assumptions = a,
    test = list(term = "condition"),
    simulate_fun = function(s) data.frame(y = rnorm(20), condition = rep(c(0, 1), 10), subject = rep(1:10, 2)),
    fit_fun = function(dat, s) stats::lm(y ~ condition, data = dat),
    test_fun = function(fit, s) list(p_value = as.numeric(coef(summary(fit))["condition", "Pr(>|t|)"]))
  )
  res <- mp_power(scn, nsim = 4, seed = 1)
  manifest <- mp_manifest(scn, seed = 1, session = FALSE)
  bundle <- mp_bundle_results(res, manifest, study_id = "S1")

  csv_file <- tempfile(fileext = ".csv")
  mp_write_results(bundle, csv_file, format = "csv", row.names = FALSE)
  on.exit(unlink(csv_file, force = TRUE), add = TRUE)
  read_back <- utils::read.csv(csv_file)
  expect_equal(nrow(read_back), 1)
  expect_equal(read_back$power_estimate, res$power, tolerance = 1e-9)
  expect_equal(read_back$failure_rate, res$diagnostics$fail_rate, tolerance = 1e-9)

  skip_if_not_installed("jsonlite")
  json_file <- tempfile(fileext = ".json")
  mp_write_results(bundle, json_file, format = "json")
  on.exit(unlink(json_file, force = TRUE), add = TRUE)
  from_json <- jsonlite::read_json(json_file)
  expect_equal(length(from_json$report), 1)
  expect_equal(as.numeric(from_json$report[[1]]$power_estimate), res$power, tolerance = 1e-9)
  expect_equal(from_json$labels$study_id, "S1")
})

test_that("mp_bundle_results rejects invalid inputs", {
  d <- mp_design(clusters = list(subject = 10), trials_per_cell = 2)
  a <- mp_assumptions(fixed_effects = list(condition = 0.2), residual_sd = 1)
  scn <- mp_scenario(
    y ~ condition,
    design = d,
    assumptions = a,
    test = "custom",
    simulate_fun = function(s) data.frame(y = rnorm(10), condition = rep(c(0, 1), 5)),
    fit_fun = function(dat, s) stats::lm(y ~ condition, data = dat),
    test_fun = function(fit, s) list(p_value = as.numeric(coef(summary(fit))["condition", "Pr(>|t|)"]))
  )
  manifest <- mp_manifest(scn, session = FALSE)
  res <- mp_power(scn, nsim = 2, seed = 1)

  expect_error(mp_bundle_results(res, list()), "mp_manifest")
  expect_error(mp_bundle_results(data.frame(), manifest), "mp_power")
})
