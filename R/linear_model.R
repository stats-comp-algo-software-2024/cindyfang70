loglik.linear <- function(design, outcome, betas){

  residuals <- outcome - design %*% betas
  loglik <- 0.5*t(residuals) %*% residuals
  return(loglik)
}

grad.linear <- function(design, outcome, betas){
  grad <- -t(design) %*% (outcome - design %*% betas)
  return(grad) # test if this is equal to the centred difference approx

}

