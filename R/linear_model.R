loglik.linear <- function(design, outcome, betas){
  loglik <- 0.5*t(outcome - design %*% betas) %*% (outcome - design %*% betas)
  return(loglik)
}

grad.linear <- function(design, outcome, betas){
  grad <- -t(design) %*% (outcome - design %*% betas)
  return(grad)
}


gaussian_logp <- function(X, x, betas) {
  logp <- - .5 * t(x - (X%*%betas)) %*% (x - (X%*%betas))
  return(logp)
}
