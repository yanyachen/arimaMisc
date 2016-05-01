#' @title Testing normality based on third central moment
#'
#' @description
#' Test normality based on asymptotic properties of third central moment.
#'
#' @param series time series data
#' @param sign significant level
#' @return p-value
#' @importFrom stats pnorm
#' @export

skewness.test <- function(series,sign=0.05){
  s3 <- timeDate::skewness(series)
  len <- length(series)
  t3 <- abs(s3/sqrt(6/len))
  p3 <- 2*(1-pnorm(t3))
  cat("skewness test", "\n", "P-value:", p3, "\n")
  if(p3>sign) cat("Can't reject the null of symmetry")
  else cat("Reject the null of symmetry")
}
