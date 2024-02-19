loglik.linear <- function(design, outcome, betas){

  mat <- outcome - design %*% betas
  loglik <- 0.5*t(mat) %*% mat
  return(loglik)
}

grad.linear <- function(design, outcome, betas){
  grad <- -t(design) %*% (outcome - design %*% betas)
  return(grad) # test if this is equal to the centred difference approx

}

