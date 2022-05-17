test_that("cHull works", {
  data(mtcars)
  x <- c(mtcars$drat)
  y <- c(mtcars$wt)
  expect_equal(cHull(x, y), 3.94787)
})
