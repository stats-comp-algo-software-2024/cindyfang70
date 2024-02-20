loglik.logit <- function(design, outcome, betas){
  loglik <- sum(t(outcome) %*% (design %*% betas)) - sum(log(1 + exp(design %*% betas)))
  return(loglik)
}

grad.logit <- function(design, outcome, betas){
  xbeta <- design %*% betas
  probs <- exp(xbeta)/(1 + exp(xbeta))
  grad <- t(design) %*% (outcome - probs)
}

hessian.logit <- function(design, outcome, betas){
  xbeta <- design %*% betas
  probs <- as.vector(exp(xbeta)/(1 + exp(xbeta)))

  W <- diag(probs)

  hess <- t(design) %*% W %*% design

}
