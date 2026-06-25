test_that("mp_backend rejects simulate_fun with no scenario argument slot", {
  expect_error(
    mp_backend(
      simulate_fun = function() data.frame(y = 1),
      fit_fun = function(data, scenario) NULL,
      test_fun = function(fit, scenario) list(p_value = 0.5)
    ),
    "at least one formal"
  )
})

test_that("mp_backend succeeds for minimal valid backend", {
  b <- mp_backend(
    simulate_fun = function(scenario, seed = NULL) data.frame(y = 1, condition = 0),
    fit_fun = function(data, scenario) stats::lm(y ~ condition, data = data),
    test_fun = function(fit, scenario) list(p_value = 0.2),
    name = "toy"
  )
  expect_s3_class(b, "mp_backend")
  expect_equal(b$name, "toy")
})

test_that("validate_mp_backend accepts plain list", {
  lst <- list(
    simulate_fun = function(scn, seed = NULL) data.frame(y = 1, x = 1),
    fit_fun = function(data, scenario) stats::lm(y ~ x, data = data),
    test_fun = function(fit, scenario) list(p_value = 0.5)
  )
  expect_true(validate_mp_backend(lst))
})
