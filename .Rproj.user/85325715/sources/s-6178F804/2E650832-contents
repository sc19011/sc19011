
#' @title linear regression with rank restraint using R.
#' @description linear regression with rank restraint using R
#' @param x predictor 
#' @param y output 
#' @import rrpack
#' @return an estimator of regression coefficients\code{n}
#' @examples
#' \dontrun{
#' JRRS <- function(x,y)
#' }
#' @export
JRRS <- function(x,y) {
  # r is the rank of A, p is ncols of X, nn is ncols of Y
  # model is Y=XA+E
  const <- 12
  r=nrow(x);p=ncol(x);nn=ncol(y) ;mm=nrow(x)
  jrrs<-rep(0,r+6)
  ccc<-array(0,dim=c((r+6),p,nn))
  for(ii in 1:(r+6)){
    cat("rank=",ii,"\n")
    ffiitt<-srrr(Y=y,X=x,nrank=ii,method='glasso',ic.type='BIC')
    ccc[ii,,]<-ffiitt$coef
    r.max<-min(nn,p/2,mm-1)
    p0.max=min(p/2,mm/2)
    s<-norm(y-x%*%ccc[ii,,],type='F')^2/(mm*nn-(nn+p0.max-r.max)*r.max)
    s <- 1
    # JRRS1, ii is rank(B)
    mse<-norm(y-x%*%ccc[ii,,],type='F')^2
    #j<-sum(apply(ccc[ii,,],1,sum)!=0)
    j <-15
    pen<-const*s^2*ii*(2*nn+log(2*exp(1))*j+j*log(exp(1)*p/j))
    jrrs[ii]<-mse+pen
  }
  # ，idx is jrrs1 min rank
  idx<-which(jrrs==min(jrrs),arr.ind=TRUE) 
  cat("idx=",idx,"\n")
  # estimate B
  ccc[idx,,]<-srrr(Y=y,X=x,nrank=idx,method='glasso',ic.type='BIC')$coef
  Best <- srrr(Y=y,X=x,nrank=1,method='glasso',ic.type='BIC')$coef
  return(Best)
}

