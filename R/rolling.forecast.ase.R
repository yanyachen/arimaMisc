#' @title Computing the out-of-sample forecasting squared error of rolling predictions
#'
#' @description
#' Calculating the average squared error of one step rolling predictions.
#'
#' @param series time series data
#' @param Arima Fitted ARIMA model from \code{\link{arima}}
#' @param range the forecasting range
#' @param start starting point for out-of-sample forecast
#' @param plot logical flag: if TRUE, then plot the observations v.s. prediction
#' @return mean square error of the prediction
#' @export

rolling.forecast.ase <- function(series, Arima, range=10, start=50, plot=FALSE){
  pred <- (1:range)*0
  se <- (1:range)*0
  for(i in start:(start+range-1)){
    series.temp <- series[1:i-1]
    order2 <- Arima$arma[c(1,6,2)]
    fixed2 <- ifelse(Arima$coef!=0, NA, 0)
    arima2 <- Arima(series.temp, order=order2, fixed=fixed2, transform.pars = FALSE)
    pred[i-start+1] <- predict(arima2,n.ahead=1)$pred
    se[i-start+1] <- (series[i]-pred[i-start+1])^2
  }
  if(plot==TRUE){
    real.pred <- cbind(series[start:(start+range)], pred)
    ts.plot(real.pred, lty=1:2, col=c("BLACK","RED"), 
            main="Observation V.S. Prediction", xlab = "Number", ylab = "Value")
    legend("topleft", legend=c("Observation", "Prediction"), 
           lty=1:2, col=c("BLACK","RED"))
  }
  return(mean(se))
}