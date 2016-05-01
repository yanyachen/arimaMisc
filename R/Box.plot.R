#' @title Plot the Ljung-Box Test's p-value vs to lag
#'
#' @description
#' Compute and Plot the Ljung-Box Test's p-value vs lag.
#'
#' @param Arima Fitted ARIMA model from \code{\link{arima}}
#' @param h maximum lag
#' @importFrom graphics plot
#' @importFrom graphics abline
#' @importFrom graphics points
#' @export

Box.plot <- function(Arima,h=48){
  plot(0,type='n', xlim=c(0,h), ylim=c(0,1),
       main='Ljung-Box Test', ylab='p-Value', xlab='lag')
  abline(h=0.05, col="RED")
  for(i in 1:h){
    coef <- Arima$coef[1:(Arima$arma[1]+Arima$arma[2])]
    Notfree <- length(coef[coef!=0])
    boxtest <- Box.test(Arima$residuals,lag=(Notfree+i),type='Ljung')
    pv <- 1 - pchisq(boxtest$statistic, boxtest$parameter-Notfree)
    pv
    points(Notfree+i,pv,pch=20)
  }
}
