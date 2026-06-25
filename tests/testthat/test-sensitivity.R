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

test_that("plot.mp_sensitivity supports 1D line and 2D heatmap", {
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

  out_2d <- mp_sensitivity(
    scn,
    vary = list(`fixed_effects.condition` = c(0.1, 0.2), `clusters.subject` = c(20, 30)),
    nsim = 3,
    seed = 1
  )
  z <- plot(out_2d)
  expect_true(is.matrix(z))
  expect_equal(dim(z), c(2, 2))
})

test_that("plot.mp_sensitivity allows singular_rate and n_effective for 1D", {
  d <- mp_design(clusters = list(subject = 20), trials_per_cell = 2)
  a <- mp_assumptions(fixed_effects = list(condition = 0.2), residual_sd = 1)
  scn <- mp_scenario(
    y ~ condition + (1 | subject),
    design = d,
    assumptions = a,
    test = "custom",
    simulate_fun = function(s) data.frame(y = stats::rnorm(20), condition = rep(c(0, 1), 10), subject = rep(1:10, 2)),
    fit_fun = function(dat, s) stats::lm(y ~ condition, data = dat),
    test_fun = function(fit, s) list(p_value = as.numeric(coef(summary(fit))["condition", "Pr(>|t|)"]))
  )
  out <- mp_sensitivity(scn, vary = list(`clusters.subject` = c(10, 20)), nsim = 3, seed = 1)
  expect_s3_class(plot(out, y = "singular_rate"), "data.frame")
  expect_s3_class(plot(out, y = "n_effective"), "data.frame")
})

test_that("plot.mp_sensitivity errors when more than two varying parameters", {
  d <- mp_design(clusters = list(subject = 20), trials_per_cell = 2)
  a <- mp_assumptions(fixed_effects = list(condition = 0.2), residual_sd = 1)
  scn <- mp_scenario(
    y ~ condition + (1 | subject),
    design = d,
    assumptions = a,
    test = "custom",
    simulate_fun = function(s) data.frame(y = stats::rnorm(20), condition = rep(c(0, 1), 10), subject = rep(1:10, 2)),
    fit_fun = function(dat, s) stats::lm(y ~ condition, data = dat),
    test_fun = function(fit, s) list(p_value = as.numeric(coef(summary(fit))["condition", "Pr(>|t|)"]))
  )
  out <- mp_sensitivity(
    scn,
    vary = list(
      `fixed_effects.condition` = c(0.1, 0.2),
      `clusters.subject` = c(20, 30),
      trials_per_cell = c(2, 4)
    ),
    nsim = 2,
    seed = 1
  )
  expect_error(plot(out), "supports one .* or two .* not 3")
})
