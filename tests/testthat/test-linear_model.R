test_that("numerical gradient and analytical gradient are similar" , {

  data <- simulate_data(n_obs=4, n_pred=4, model = 'linear', seed = 1918)
  design <- data$design; outcome <- data$outcome; betas <- data$coef_true


  my_grad <- grad.linear(design, outcome, betas)
  numerical_grad <- approxgrad.linear(function(x){loglik.linear(design, outcome, x)}, betas)
  print(my_grad)
  print(numerical_grad)
  expect_true(are_all_close(my_grad, numerical_grad))
})


