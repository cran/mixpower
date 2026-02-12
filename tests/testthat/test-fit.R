test_that("fit_model returns an lm object", {
  data <- data.frame(y = rnorm(10), x = rnorm(10))
  fit <- fit_model(data, y ~ x)
  expect_s3_class(fit, "lm")
})
