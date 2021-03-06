
#' @title fill in missing values using R.
#' @description fill in missing values using R
#' @param data_temp data 
#' @param sigma range of fitting, small sigma means low influence from long distance data
#' @import rrpack
#' @return an estimator of missing values\code{n}
#' @examples
#' \dontrun{
#' data <- c(1,2,3,0,3,2,1)
#' mls(data,0.4)
#' }
#' @export
mls <-  function(data_temp,sigma){
  b=0
  x0 <- which(data_temp == 0)
  index <- seq(1,length(data_temp),1)/(length(data_temp))
  x0=x0/(length(data_temp))
  models <- function(x,theta){
    matrix(theta, nrow = 1,ncol = 3, byrow = T)%*%matrix(c(1-x^2,2*x*(1-x),x^2), nrow = 3, ncol = length(x),byrow = T)
  }
  loss = function(theta, x, y){
    sum(exp(-(x-x0)^2/sigma)*(y-models(x, theta))^2)
  }
  theta0 <- c(0, 0, 0, 0, 0, 0)
  theta.L <- optim(theta0, loss, x=index, y=data_temp, method = "BFGS")
  theta.L
  models(x0,theta.L$`par`)
  b = models(x0,theta.L$`par`)
  return(b)
}
