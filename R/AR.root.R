#' @title Computing roots of the characteristic AR polynomial
#'
#' @description
#' Compute roots of the characteristic AR polynomial.
#'
#' @param Arima Fitted ARIMA model from \code{\link{arima}}
#' @return returns a three column data frame with the real, 
#'   the imaginary part and the radius of the roots.
#'   The number of rows corresponds to the coefficients.
#' @export

AR.root <- function(Arima){
  if(Arima$arma[1]==0){
    return("This model has no AR part")
  }else{
    return(fArma::armaRoots(Arima$coef[1:Arima$arma[1]]))
  }
}