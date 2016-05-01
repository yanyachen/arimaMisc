#' @title Generating fixed parameter for an ARIMA full model
#'
#' @description
#' Performe Z test for arma model's coefficients and Return an vector of
#' fixed parameter for \code{\link{arima}} function's fixed argument.
#'
#' @param Arima Fitted ARIMA model from \code{\link{arima}}
#' @return fixed parameter for an arima function's fixed argument
#' @importFrom stats qnorm
#' @export

armaSigOrder <- function(Arima){
  z <- abs(Arima$coef) / sqrt(diag(Arima$var.coef))
  fixed <- ifelse(z>qnorm(0.975), NA, 0)
  return(fixed)
}
