test_that("mysq works", {
  l <- mysq(2:4)
  expect_equal(l[1], 4)
})
