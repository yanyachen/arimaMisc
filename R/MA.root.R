#' @title Computing roots of the characteristic MA polynomial
#'
#' @description
#' Compute roots of the characteristic MA polynomial
#'
#' @param Arima Fitted ARIMA model from \code{\link{arima}}
#' @return returns a three column data frame with the real, 
#'   the imaginary part and the radius of the roots.
#'   The number of rows corresponds to the coefficients.
#' @export

MA.root <- function(Arima){
  if(Arima$arma[2]==0){
    return("This model has no MA part")
  }else{
    return(fArma::armaRoots(-Arima$coef[(Arima$arma[1]+1):sum(Arima$arma[1:2])]))
  }
}