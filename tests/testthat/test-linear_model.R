test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


test_that("numerical gradient and analytical gradient are similar" , {
  set.seed(410)
  n_param <- 4
  X <- matrix(rnorm(2 * n_param^2), nrow = 2 * n_param, ncol = n_param)
  Sigma_inv <- t(X) %*% X
  x <- c(3, 1, 4, 1)

  hglm <- hiper_glm(
    X, x, model = 'linear', option = list(mle_solver = 'BFGS')
  )

  betas <- coef(hglm)

  my_grad <- grad.linear(X, x, betas)
  numerical_grad <- approxgrad.linear(function(x) gaussian_logp(X, x, betas), x)

  print(my_grad)
  expect_true(are_all_close(
    my_grad, numerical_grad, abs_tol = Inf, rel_tol = 1e-3
  ))
})


