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

  solver_options <- c("BFGS", "pseudoinverse", "newton-raphson")

  if (model == "linear"){
    if (!(options$mle_solver %in% solver_options)){
      warning("Invalid MLE solver option, defaulting to BFGS")
      betas_hat <-find.mle.bfgs(design=design, outcome=outcome, func=loglik.linear, grad=grad.linear)
    }else if ((options$mle_solver) == "pseudoinverse"){
      betas_hat <- find.mle.pseudoinv(design, outcome)
    }else if (options$mle_solver == "BFGS"){
      betas_hat <-find.mle.bfgs(design=design, outcome=outcome, func=loglik.linear, grad=grad.linear)
    }

  }else if (model == "logit"){
    if (!(options$mle_solver %in% solver_options)){
      warning("Invalid MLE solver option, defaulting to BFGS")
      betas_hat <-find.mle.bfgs(design=design, outcome=outcome,
                                func=loglik.logit, grad=grad.logit, model="logit")
    } else if (options$mle_solver=="BFGS"){
      betas_hat <-find.mle.bfgs(design=design, outcome=outcome,
                                func=loglik.logit, grad=grad.logit,
                                model="logit")
    }else if (options$mle_solver=="newton-raphson"){
      betas_hat <-find.mle.newtonraphson(design=design, outcome=outcome,
                                         func=loglik.logit, grad=grad.logit,
                                         hess=hessian.logit)
  }
  }
  hglm$coefficients <- betas_hat
  return(hglm)
}
