#' @title Select ridge regression parameter based on AIC or BIC
#'
#' @description
#' Select ridge regression parameter based on AIC or BIC.
#'
#' @param formula a formula expression as for regression models, of the form 
#'   response ~ predictors. See the documentation of formula for other details.
#' @param data an optional data frame in which to interpret the variables 
#'   occuring in formula
#' @param lambda A ridge regression parameter, May be a vector
#' @param IC character, indicating which information criterion to use, 
#'   "AIC" or "BIC"
#' @return optimal ridge regression parameter
#' @export

Ridge.IC <- function(formula, data, lambda, IC="AIC"){
  if((IC %in% c("AIC","BIC"))==FALSE){
    stop("This function only support information criterion: AIC and BIC")
  }
  model <- ridge::linearRidge(formula, data=data.frame(data), lambda=lambda, scaling="scale", all.coef=TRUE)
  n <- length(model$y)
  df <- model$df[,1]
  coefs <- coef(model)
  X1 <- as.matrix(cbind(1,data[,colnames(model$x)]))
  pred <- X1 %*% t(coefs)
  y <- model$y
  RSS.n <- colMeans((y-pred)^2)
  if(IC=="AIC"){
    IC <- AIC <- n*log(RSS.n) + 2*df
  }else if(IC=="BIC"){
    IC <- BIC <- n*log(RSS.n) + log(n)*df
  }
  opt.lambda <- lambda[which.min(IC)]
  plot(lambda,IC, type="l")
  abline(v=opt.lambda)
  text(x=opt.lambda, y=mean(IC), labels=paste("lambda=",opt.lambda))
  return(opt.lambda)
}