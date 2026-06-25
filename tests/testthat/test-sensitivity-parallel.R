test_that("parallel sensitivity matches serial sensitivity", {
  skip_if_not_installed("parallel")
  can_parallel <- tryCatch({
    cl <- parallel::makeCluster(2L)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterEvalQ(cl, suppressPackageStartupMessages(library(mixpower, character.only = TRUE)))
    TRUE
  }, error = function(e) FALSE)
  skip_if_not(can_parallel)

  d <- mp_design(clusters = list(subject = 18), trials_per_cell = 2)
  a <- mp_assumptions(
    fixed_effects = list(`(Intercept)` = 0, condition = 0.25),
    residual_sd = 1
  )
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)

  vary <- list(`fixed_effects.condition` = c(0.2, 0.35))
  serial <- mp_sensitivity(scn, vary = vary, nsim = 8, seed = 99)
  paral <- mp_sensitivity_parallel(scn, vary = vary, nsim = 8, seed = 99, workers = 2L)

  expect_equal(serial$results$estimate, paral$results$estimate)
  expect_equal(serial$results$failure_rate, paral$results$failure_rate)
})

test_that("mp_sensitivity_parallel progress path returns mp_sensitivity", {
  d <- mp_design(clusters = list(subject = 12), trials_per_cell = 2)
  a <- mp_assumptions(fixed_effects = list(`(Intercept)` = 0, condition = 0.3), residual_sd = 1)
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
  out <- mp_sensitivity_parallel(
    scn,
    vary = list(`clusters.subject` = c(10, 12)),
    nsim = 5,
    seed = 3,
    progress = TRUE
  )
  expect_s3_class(out, "mp_sensitivity")
  expect_equal(nrow(out$results), 2L)
})

test_that("checkpoint_dir writes manifest and cells", {
  d <- mp_design(clusters = list(subject = 10), trials_per_cell = 2)
  a <- mp_assumptions(fixed_effects = list(`(Intercept)` = 0, condition = 0.2), residual_sd = 1)
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
  ck <- tempfile("mp_ck_")
  dir.create(ck)
  on.exit(unlink(ck, recursive = TRUE), add = TRUE)

  out <- mp_sensitivity_parallel(
    scn,
    vary = list(`clusters.subject` = c(10, 12)),
    nsim = 4,
    seed = 11,
    progress = TRUE,
    checkpoint_dir = ck,
    resume = TRUE
  )
  expect_true(file.exists(file.path(ck, "_mixpower_sensitivity_manifest.rds")))
  expect_true(file.exists(file.path(ck, "cell_00001.rds")))
  expect_equal(nrow(out$results), 2L)
})
