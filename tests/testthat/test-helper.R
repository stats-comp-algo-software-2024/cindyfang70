test_that("correctly returns TRUE", {
  expect_equal(are_all_close(1, 1 + .Machine$double.eps), TRUE)
})

test_that("correctly returns FALSE because the relative error is above rel_tol", {
  expect_equal(are_all_close(1, 1 + 2*1e-6), FALSE)
})

test_that("correctly returns FALSE because the relative error is above abs_tol", {
  expect_equal(are_all_close(1, 1 + 1e-5), FALSE)
})

