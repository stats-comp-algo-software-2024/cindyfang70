#' hiper_glm
#'
#' @param design a design matrix
#' @param outcome a vector of outcomes
#'
#' @return An object of class `hglm`
#' @export
hiper_glm <- function(design, outcome, model, options){
  hglm <- list()
  class(hglm) <- "hglm"

  if (model=="linear"){
    if(missing(options)){
      # assume pseudoinverse as default
      betas_hat <- find.mle.pseudoinv(design, outcome)
    }else if (options$mle_solver=="BFGS"){
      # do bfgs
      betas_hat <-find.mle.bfgs(design, outcome)
    }

  }
  hglm$coefficients <- betas_hat
  return(hglm)
}
