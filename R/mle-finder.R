find.mle.pseudoinv <- function(design, outcome){
  betas <- solve(t(design) %*% design, t(design) %*% outcome)
  return(betas)
}

find.mle.bfgs <- function(design, outcome, func, grad, tol=1e-6, model="linear"){
  betas <- rep(0, ncol(design))
  if (model=="logit"){
    betas_hat <- stats::optim(betas, fn=func, gr=grad, design=design,
                              outcome=outcome, method="BFGS", control=list(fnscale = -1))
  }else{
    betas_hat <- stats::optim(betas, fn=func, gr=grad, design=design,
                              outcome=outcome, method="BFGS")
  }



  return(betas_hat$par)

}

find.mle.newtonraphson <- function(design, outcome, func, grad, hess, iter=10000){
  betas <- rep(0.001, ncol(design))

  n_iter <- 0
  loglik.diff <- 10000
  while(loglik.diff > 1e-10){
    hessian <- hessian.logit(design, outcome, betas)
    gradient <- grad.logit(design, outcome, betas)

    loglik.old <- func(design, outcome, betas)

    update <- solve(hessian) %*% gradient
    betas <- betas - update

    loglik.new <- func(design, outcome, betas)

    loglik.diff <- abs(loglik.old-loglik.new)

    n_iter <- n_iter + 1

    if (n_iter > iter){
      stop("Not converging")
    }
  }
  return(betas)

  # design <- as.matrix(design)
  # outcome <- as.matrix(outcome )
  # betas <- rep(0.001, ncol(design))
  # i = 1
  #
  # while(i < iter){
  #   hess <- hessian.logit(design, outcome, betas)
  #   grad <- grad.logit(design, outcome, betas)
  #   betas <- betas - solve(hess)%*%grad
  #   i= i + 1
  # }
  #
  # return(betas)
}
