test_that("sensitivity comparison across Wald and LRT runs with matched vary grid", {
  skip_if_not_installed("lme4")

  d <- mp_design(clusters = list(subject = 20), trials_per_cell = 4)
  a <- mp_assumptions(
    fixed_effects = list(`(Intercept)` = 0, condition = 0.4),
    residual_sd = 1,
    icc = list(subject = 0.1)
  )

  scn_wald <- mp_scenario_lme4(
    y ~ condition + (1 | subject),
    design = d,
    assumptions = a,
    test_method = "wald"
  )

  scn_lrt <- mp_scenario_lme4(
    y ~ condition + (1 | subject),
    design = d,
    assumptions = a,
    test_method = "lrt",
    null_formula = y ~ 1 + (1 | subject)
  )

  vary_spec <- list(`clusters.subject` = c(15, 25))
  s_w <- mp_sensitivity(scn_wald, vary = vary_spec, nsim = 4, seed = 1)
  s_l <- mp_sensitivity(scn_lrt,  vary = vary_spec, nsim = 4, seed = 1)

  expect_equal(s_w$results$`clusters.subject`, s_l$results$`clusters.subject`)
  expect_true(all(c("estimate", "failure_rate", "singular_rate") %in% names(s_w$results)))
  expect_true(all(c("estimate", "failure_rate", "singular_rate") %in% names(s_l$results)))
})
