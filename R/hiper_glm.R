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

  solver_options <- c("BFGS", "pseudoinverse")

  if (model == "linear"){
    if (!(options$mle_solver %in% solver_options)){
      warning("Invalid MLE solver option, defaulting to BFGS")
      betas_hat <-find.mle.bfgs(design, outcome)
    }else if (options$mle == "pseudoinverse"){
      betas_hat <- find.mle.pseudoinv(design, outcome)
    }else if (options$mle == "BFGS"){
      betas_hat <-find.mle.bfgs(design, outcome)
    }

  }

  hglm$coefficients <- betas_hat
  return(hglm)
}
