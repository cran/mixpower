test_that("mp_power_curve wraps sensitivity for a single parameter", {
  d <- mp_design(clusters = list(subject = 20), trials_per_cell = 2)
  a <- mp_assumptions(fixed_effects = list(condition = 0.3), residual_sd = 1)

  scn <- mp_scenario(
    y ~ condition + (1 | subject),
    design = d,
    assumptions = a,
    test = list(term = "condition"),
    simulate_fun = function(scenario) {
      data.frame(
        y = stats::rnorm(20),
        condition = rep(c(0, 1), 10),
        subject = rep(1:10, 2)
      )
    },
    fit_fun = function(data, scenario) stats::lm(y ~ condition, data = data),
    test_fun = function(fit, scenario) {
      list(p_value = as.numeric(coef(summary(fit))["condition", "Pr(>|t|)"]))
    }
  )

  curve <- mp_power_curve(
    scn,
    vary = list(`clusters.subject` = c(10, 20)),
    nsim = 4,
    seed = 1
  )

  expect_s3_class(curve, "mp_power_curve")
  expect_true(all(c("estimate", "failure_rate", "singular_rate", "n_effective") %in% names(curve$results)))
})

test_that("mp_power_curve rejects non-scenario or multi-parameter vary", {
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

  expect_error(mp_power_curve(list(), vary = list(`clusters.subject` = c(10, 20)), nsim = 2),
               "mp_scenario")
  expect_error(mp_power_curve(scn, vary = list(`clusters.subject` = c(10, 20), `trials_per_cell` = c(2, 4)), nsim = 2),
               "exactly one entry")
})
