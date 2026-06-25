test_that("as_tibble.mp_power works with tibble installed", {
  skip_on_cran()
  skip_if_not_installed("tibble")

  d <- mp_design(clusters = list(subject = 10), trials_per_cell = 2)
  a <- mp_assumptions(fixed_effects = list(`(Intercept)` = 0, condition = 0.2), residual_sd = 1)
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
  res <- mp_power(scn, nsim = 6, seed = 1)
  tbl <- tibble::as_tibble(res)
  expect_true(nrow(tbl) > 0L)
  expect_true("p_value" %in% names(tbl))
})

test_that("autoplot.mp_power_curve requires ggplot2", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  d <- mp_design(clusters = list(subject = 14), trials_per_cell = 2)
  a <- mp_assumptions(fixed_effects = list(`(Intercept)` = 0, condition = 0.25), residual_sd = 1)
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
  curve <- mp_power_curve(
    scn,
    vary = list(`clusters.subject` = c(12, 14)),
    nsim = 5,
    seed = 2
  )
  p <- ggplot2::autoplot(curve)
  expect_s3_class(p, "ggplot")
})
