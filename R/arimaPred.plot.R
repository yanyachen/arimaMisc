#' @title Plot the prediction and confidence interval of arima model
#'
#' @description
#' Compute the prediction and standard error of the prediction and 
#' plot the prediction and confidence interval of arima model
#'
#' @param series time series data
#' @param Arima Fitted ARIMA model from \code{\link{arima}}
#' @param b number of periods for original data plotting
#' @param h number of periods for forecasting
#' @param level confidence level for prediction intervals
#' @param new.xreg optional, a vector or matrix of external regressors
#' @export

arimaPred.plot <- function(series, Arima, b, h, level=0.95, new.xreg=NULL){
  pred <- predict(Arima, n.ahead = h, newxreg=new.xreg)
  datalast.ts <- ts(c(series[(length(series)-(b-1)):length(series)], 
                      pred$pred[1],rep(NA,(h-1))), start=1)
  pred.ts <- ts(c(rep(NA,b),pred$pred), start=(b+1))
  level.d <- 1-((1-level)/2)
  pred.lower <- ts(c(rep(NA,b),pred$pred-qnorm(level.d)*pred$se), start=(b+1))
  pred.upper <- ts(c(rep(NA,b),pred$pred+qnorm(level.d)*pred$se), start=(b+1))
  ts.plot(data.frame(datalast.ts,pred.ts,pred.lower,pred.upper), 
          lty=c(1,2,3,3), col = c("BLACK", "BLUE", "RED", "RED"))
}