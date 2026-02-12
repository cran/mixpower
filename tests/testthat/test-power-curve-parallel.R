test_that("parallel curve matches serial curve with fixed seed", {
  skip_if_not_installed("parallel")
  can_parallel <- tryCatch({
    cl <- parallel::makeCluster(2L)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterEvalQ(cl, library(mixpower, character.only = TRUE))
    TRUE
  }, error = function(e) FALSE)
  skip_if_not(can_parallel, "Cannot create cluster or load mixpower on workers (e.g. sandbox or run from installed pkg)")

  d <- mp_design(clusters = list(subject = 20), trials_per_cell = 2)
  a <- mp_assumptions(fixed_effects = list(condition = 0.3), residual_sd = 1)

  scn <- mp_scenario(
    y ~ condition + (1 | subject),
    design = d,
    assumptions = a,
    test = list(term = "condition"),
    simulate_fun = function(scenario) {
      n_subj <- scenario$design$clusters$subject
      n <- n_subj * scenario$design$trials_per_cell
      data.frame(
        y = stats::rnorm(n),
        condition = rep(c(0, 1), length.out = n),
        subject = rep(seq_len(n_subj), each = scenario$design$trials_per_cell)
      )
    },
    fit_fun = function(data, scenario) stats::lm(y ~ condition, data = data),
    test_fun = function(fit, scenario) {
      list(p_value = as.numeric(coef(summary(fit))["condition", "Pr(>|t|)"]))
    }
  )

  serial <- mp_power_curve(
    scn,
    vary = list(`clusters.subject` = c(10, 20)),
    nsim = 6,
    seed = 123
  )

  parallel <- mp_power_curve_parallel(
    scn,
    vary = list(`clusters.subject` = c(10, 20)),
    nsim = 6,
    seed = 123,
    workers = 2
  )

  expect_equal(serial$results$estimate, parallel$results$estimate)
  expect_equal(serial$results$failure_rate, parallel$results$failure_rate)
  expect_equal(serial$results$singular_rate, parallel$results$singular_rate)
  expect_equal(serial$results$n_effective, parallel$results$n_effective)
})

test_that("mp_power_curve_parallel with progress = TRUE returns mp_power_curve", {
  d <- mp_design(clusters = list(subject = 20), trials_per_cell = 2)
  a <- mp_assumptions(fixed_effects = list(condition = 0.3), residual_sd = 1)
  scn <- mp_scenario(
    y ~ condition + (1 | subject),
    design = d,
    assumptions = a,
    test = list(term = "condition"),
    simulate_fun = function(s) {
      data.frame(
        y = stats::rnorm(20),
        condition = rep(c(0, 1), 10),
        subject = rep(1:10, 2)
      )
    },
    fit_fun = function(dat, s) stats::lm(y ~ condition, data = dat),
    test_fun = function(fit, s) list(p_value = as.numeric(coef(summary(fit))["condition", "Pr(>|t|)"]))
  )

  out <- mp_power_curve_parallel(
    scn,
    vary = list(`clusters.subject` = c(10, 20)),
    nsim = 4,
    seed = 1,
    progress = TRUE
  )

  expect_s3_class(out, "mp_power_curve")
  expect_true(all(c("estimate", "failure_rate", "singular_rate", "n_effective") %in% names(out$results)))
})

test_that("mp_power_curve_parallel validates scenario and vary", {
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

  expect_error(mp_power_curve_parallel(list(), vary = list(`clusters.subject` = c(10, 20)), nsim = 2),
               "mp_scenario")
  expect_error(mp_power_curve_parallel(scn, vary = list(a = 1, b = 2), nsim = 2),
               "exactly one entry")
})
