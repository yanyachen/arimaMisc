#' @title Corrected Ljung-Box Test for ARIMA model's residuals
#'
#' @description
#' Compute Ljung Box test statistic and return p-value for examining an ARIMA 
#' model's residuals, with correct degree of freedom. This is an efficient
#' version of \code{\link{Box.test}} for testing ARIMA model's residuals.
#'
#' @param Arima Fitted ARIMA model from \code{\link{arima}}
#' @param lag the statistic based on \emph{lag} autocorrelation coefficients
#' @return p-value of Ljung-Box Test
#' @export

Box.Test <- function(Arima, lag=12){
  boxtest <- Box.test(Arima$residuals,lag=lag,type='Ljung')
  pv <- 1 - pchisq(boxtest$statistic, 
                   boxtest$parameter-length(Arima$coef[Arima$coef!=0]))
  return(as.numeric(pv))
}