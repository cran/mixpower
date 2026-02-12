test_that("mp_solve_sample_size returns a solution when power achieved", {
  d <- mp_design(clusters = list(subject = 10), trials_per_cell = 2)
  a <- mp_assumptions(fixed_effects = list(condition = 0.5), residual_sd = 1)

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

  out <- mp_solve_sample_size(
    scn,
    parameter = "clusters.subject",
    grid = c(10, 20, 30),
    target_power = 0.5,
    nsim = 4,
    seed = 1
  )

  expect_true(is.numeric(out$solution))
  expect_named(out, c("target_power", "parameter", "solution", "results"))
  expect_true(all(c("estimate", "failure_rate", "singular_rate", "n_effective") %in% names(out$results)))
})

test_that("mp_solve_sample_size returns NA when target never achieved", {
  d <- mp_design(clusters = list(subject = 10), trials_per_cell = 2)
  a <- mp_assumptions(fixed_effects = list(condition = 0.001), residual_sd = 10)

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

  out <- mp_solve_sample_size(
    scn,
    parameter = "clusters.subject",
    grid = c(10, 12),
    target_power = 0.99,
    nsim = 4,
    seed = 1
  )

  expect_identical(out$solution, NA_real_)
})
