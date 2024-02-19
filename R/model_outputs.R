#' predict.hglm
#'
#' @param hglm an S3 object of type hglm
#'
#' @return a warning
#' @export
predict.hglm <- function(hglm){
  warning("This is a placeholder until the function is implemented")
}

#' coef.hglm
#'
#' @param hglm an S3 object of type hglm
#'
#' @return a warning
#' @export
coef.hglm <- function(hglm){
  return(hglm$coefficients)
}

#' cov.hglm
#'
#' @param hglm an S3 object of type hglm
#'
#' @return a warning
#' @export
cov.hglm <- function(hglm){
  warning("This is a placeholder until the function is implemented")
}

#' vcov.hglm
#'
#' @param hglm an S3 object of type hglm
#'
#' @return a warning
#' @export
vcov.hglm <- function(hglm){
  warning("This is a placeholder until the function is implemented")
}
