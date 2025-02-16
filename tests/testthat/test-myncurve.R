library(testthat)

test_that("myncurve returns correct mu", {
  result <- myncurve(0, 1, 1.5)
  expect_equal(result$mu, 0)
})

test_that("myncurve returns correct sigma", {
  result <- myncurve(0, 1, 1.5)
  expect_equal(result$sigma, 1)
})

test_that("myncurve computes correct probability", {
  result <- myncurve(0, 1, 1.5)
  expected_prob <- round(pnorm(1.5, mean=0, sd=1), 4)
  expect_equal(result$probability, expected_prob)
})

test_dir("tests/testthat")
