test_that("numerical gradient and analytical gradient are similar" , {
  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = 'logit', seed = 1918)
  design <- data$design; outcome <- data$outcome
  betas <- rep(0, n_pred)

  my_grad <- grad.logit(design, outcome, betas)

  numerical_grad <- approxgrad.linear(function(x){loglik.logit(design, outcome, x)}, betas)
  print(my_grad)
  print(numerical_grad)
  expect_true(are_all_close(my_grad, numerical_grad))
})
