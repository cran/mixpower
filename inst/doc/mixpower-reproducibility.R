## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(mixpower)

## -----------------------------------------------------------------------------
d <- mp_design(clusters = list(subject = 30), trials_per_cell = 4)
a <- mp_assumptions(
  fixed_effects = list(`(Intercept)` = 0, condition = 0.3),
  residual_sd = 1,
  random_effects = list(subject = list(intercept_sd = 0.1))
)
scn <- mp_scenario_lme4(
  y ~ condition + (1 | subject),
  design = d,
  assumptions = a,
  test_method = "wald"
)

seed <- 123
res <- mp_power(scn, nsim = 12, seed = seed)
manifest <- mp_manifest(scn, seed = seed, session = FALSE)
manifest

## -----------------------------------------------------------------------------
bundle <- mp_bundle_results(
  res,
  manifest,
  study_id = "power_2024_01",
  analyst = "analyst",
  notes = "Initial power run for condition effect"
)
bundle

## -----------------------------------------------------------------------------
tab <- mp_report_table(bundle)
tab

## ----eval = FALSE-------------------------------------------------------------
# mp_write_results(bundle, "power_results.csv", format = "csv", row.names = FALSE)
# mp_write_results(bundle, "power_results.json", format = "json")

## -----------------------------------------------------------------------------
res2 <- mp_power(scn, nsim = 12, seed = manifest$seed)
all.equal(res$power, res2$power)

## -----------------------------------------------------------------------------
m <- mp_manifest(scn, seed = 123, session = FALSE)
df_row <- data.frame(
  scenario_digest = m$scenario_digest,
  seed = m$seed,
  seed_strategy = m$seed_strategy,
  timestamp = m$timestamp,
  r_version = m$r_version,
  mixpower_version = m$mixpower_version,
  git_sha = m$git_sha,
  stringsAsFactors = FALSE
)
df_row

