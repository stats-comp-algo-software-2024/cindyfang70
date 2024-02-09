loglik.linear <- function(design, outcome, betas){
  sigma_inv <- t(design) %*% design
  loglik <- 0.5*t(outcome - design %*% betas) %*% (outcome - design %*% betas)
  return(loglik)
}

grad.linear <- function(design, outcome, betas){
  grad <- -t(design) %*% (outcome - design %*% betas)
  return(grad)
}

approxgrad.linear <- function(func, x, dx = .Machine$double.eps^(1/3)) {
  numerical_grad <- rep(0, length(x))
  for (i in 1:length(x)){
    x_plus <- x
    x_plus[[i]] <- x_plus[[i]] + dx

    x_minus <- x
    x_minus[[i]] <- x_minus[[i]] - dx

    numerical_grad[[i]] <- (func(x_plus) - func(x_minus))/(2*dx)
  }

  #numerical_grad <- (func(x + dx) - func(x - dx))/(2*dx)
  return(numerical_grad)
}
gaussian_logp <- function(X, x, betas) {
  logp <- - .5 * t(x - (X%*%betas)) %*% (x - (X%*%betas))
  return(logp)
}