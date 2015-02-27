#' @title Testing normality based on fourth central moment
#'
#' @description
#' Test normality based on asymptotic properties of fourth central moment
#'
#' @param series time series data
#' @param sign significant level
#' @return p-value
#' @export

kurtosis.test <- function(series,sign=0.05){
  s4 <- kurtosis(series)
  len <- length(series)
  t4 <- abs(s4/sqrt(24/len))
  p4 <- 2*(1-pnorm(t4))
  cat("kurtosis test", "\n", "P-value:", p4, "\n")
  if(p4>sign) cat("can't reject the null of normal tail thickness")
  else cat("Reject the null of normal tail thickness")
}