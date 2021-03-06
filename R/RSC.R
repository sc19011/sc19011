#' @title linear regression with rank restraint using R.
#' @description linear regression with rank restraint using R
#' @import MASS
#' @param x predictor 
#' @param y output 
#' @return the rank of regression coefficients\code{n}
#' @examples
#' \dontrun{
#' Y <- t(t(X))%*%A+E
#' RSC(X,Y)
#' }
#' @export
RSC <-  function(x,y){
  p=ncol(x);nn=ncol(y) ;mm=min(nrow(x),nn)
  M=t(x)%*%x
  mpM=ginv(M) # M的MP逆
  P=x%*%mpM%*%t(x)
  V=as.matrix(eigen(t(y)%*%P%*%y)$vectors)
  hatB=mpM%*%t(x)%*%y 
  W=hatB%*%V
  G=t(V)
  k<- mm
  hatBk<-array(0,dim=c(k,p,nn))
  RSC<-rep(0,k-1)
  mu=70
  for(kk in 2:k){
    #cat("rank=",kk,"\n")
    Wk=W[,1:kk]
    Gk=G[1:kk,]
    hatBk[kk-1,,]=Wk%*%Gk
    mse<-norm(y-x%*%hatBk[kk-1,,],type='F')^2
    pen<-mu*kk
    RSC[kk-1]<-mse+pen
  }
  rank<-which(RSC==min(RSC),arr.ind=TRUE)+1
  return(rank)
}
