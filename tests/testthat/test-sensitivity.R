test_that("mp_sensitivity validates vary keys and values", {
  d <- mp_design(clusters = list(subject = 20), trials_per_cell = 2)
  a <- mp_assumptions(fixed_effects = list(condition = 0.2), residual_sd = 1)

  scn <- mp_scenario(
    y ~ condition + (1 | subject),
    design = d,
    assumptions = a,
    test = "custom",
    simulate_fun = function(scenario) {
      data.frame(
        y = stats::rnorm(20),
        condition = rep(c(0, 1), 10),
        subject = rep(1:10, 2)
      )
    },
    fit_fun = function(data, scenario) stats::lm(y ~ condition, data = data),
    test_fun = function(fit, scenario) {
      p_val <- coef(summary(fit))["condition", "Pr(>|t|)"]
      list(p_value = as.numeric(p_val))
    }
  )

  expect_error(mp_sensitivity(scn, vary = list(unknown.key = c(1, 2)), nsim = 3),
               "Unsupported variation key")
  expect_error(mp_sensitivity(scn, vary = list(fixed_effects = c(0.1, 0.2)), nsim = 3),
               "must include a subfield")
  expect_error(mp_sensitivity(scn, vary = list(`clusters.subject` = c(10, NA)), nsim = 3),
               "must not contain `NA`")
})

test_that("plot.mp_sensitivity rejects multi-parameter objects", {
  d <- mp_design(clusters = list(subject = 20), trials_per_cell = 2)
  a <- mp_assumptions(fixed_effects = list(condition = 0.2), residual_sd = 1)

  scn <- mp_scenario(
    y ~ condition + (1 | subject),
    design = d,
    assumptions = a,
    test = "custom",
    simulate_fun = function(scenario) {
      data.frame(
        y = stats::rnorm(20),
        condition = rep(c(0, 1), 10),
        subject = rep(1:10, 2)
      )
    },
    fit_fun = function(data, scenario) stats::lm(y ~ condition, data = data),
    test_fun = function(fit, scenario) {
      p_val <- coef(summary(fit))["condition", "Pr(>|t|)"]
      list(p_value = as.numeric(p_val))
    }
  )

  out <- mp_sensitivity(
    scn,
    vary = list(`fixed_effects.condition` = c(0.1, 0.2), `clusters.subject` = c(20, 30)),
    nsim = 3,
    seed = 1
  )

  expect_error(plot(out), "one varying parameter")
})
