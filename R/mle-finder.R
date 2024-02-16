find.mle.pseudoinv <- function(design, outcome){
  sigma_inv <- t(design) %*% design
  #print(sigma_inv)
  betas <- solve(t(design) %*% design, t(design) %*% outcome)
  return(betas)
}

find.mle.bfgs <- function(design, outcome, tol=1e-6){
  betas <- rep(0, ncol(design))

  betas_hat <- stats::optim(betas, loglik.linear, design=design, outcome=outcome)

  return(betas_hat$par)

}
