make_res <- function() {
  d <- mp_design(list(subject = 30), trials_per_cell = 6)
  a <- mp_assumptions(
    fixed_effects = list("(Intercept)" = 0, condition = 0.4),
    random_effects = list(subject = list(intercept_sd = 0.5)),
    residual_sd = 1
  )
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
  suppressWarnings(mp_power(scn, nsim = 30, seed = 1))
}

test_that("mp_methods_text produces a usable paragraph", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  res <- make_res()
  txt <- mp_methods_text(res)
  expect_s3_class(txt, "mp_methods_text")
  s <- unclass(txt)
  expect_match(s, "simulation-based power analysis")
  expect_match(s, "mixpower")
  expect_match(s, "condition")
  expect_match(s, "%")
  expect_output(print(txt), "power")

  no_sw <- mp_methods_text(res, software = FALSE)
  expect_false(grepl("mixpower R package", unclass(no_sw)))
})

test_that("mp_methods_text validates input", {
  expect_error(mp_methods_text(list()), "mp_power")
})

test_that("plot.mp_power draws the p-value distribution", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  res <- make_res()
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  p <- plot(res)
  expect_true(is.numeric(p))
  expect_true(all(p >= 0 & p <= 1))
})

test_that("plot.mp_power errors when no per-replicate p-values are stored", {
  skip_if_not_installed("lme4")
  skip_on_cran()
  d <- mp_design(list(subject = 25), trials_per_cell = 6)
  a <- mp_assumptions(list("(Intercept)" = 0, condition = 0.4),
                      random_effects = list(subject = list(intercept_sd = 0.5)),
                      residual_sd = 1)
  scn <- mp_scenario_lme4(y ~ condition + (1 | subject), design = d, assumptions = a)
  streamed <- suppressWarnings(mp_power(scn, nsim = 20, seed = 1, aggregate = "streaming"))
  expect_error(plot(streamed), "streaming")
})
