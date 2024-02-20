find.mle.pseudoinv <- function(design, outcome){
  betas <- solve(t(design) %*% design, t(design) %*% outcome)
  return(betas)
}

find.mle.bfgs <- function(design, outcome, func, grad, tol=1e-6){
  betas <- rep(0, ncol(design))

  betas_hat <- stats::optim(betas, fn=loglik.linear, gr=grad, design=design,
                            outcome=outcome, method="BFGS")

  return(betas_hat$par)

}
