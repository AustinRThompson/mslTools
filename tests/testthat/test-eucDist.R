test_that("multiplication works", {
  testthat::expect_equal(as.numeric(eucDist(x1 = 2,
                                 y1 = 3,
                                 x2 = 5,
                                 y2 = 8)),
                         5.8309518948453)
})
