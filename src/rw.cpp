//' @title A Gibbs sampler using Rcpp
//' @description A Gibbs sampler using Rcpp
//' @param N the number of samples
//' @param thin the number of between-sample random numbers
//' @return a random sample of size \code{n}
//' @examples
//' \dontrun{
//' rnC <- gibbsC(100,10)
//' par(mfrow=c(2,1));
//' plot(rnC[,1],type='l')
//' plot(rnC[,2],type='l')
//' }
//' @export
// [[Rcpp::export]]

#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
NumericVector rwMetropolisC(double sigma, double x0, int N){
  NumericVector x(N);
  x[0] = x0;
  int k = 0;
  double u=0;
  double y=0;
  for(int i=1;i<N;i++){
    u = as<double>(runif(1));
    y = as<double>(rnorm(1, x[i-1], sigma));
    if(u <= exp(-abs(y)+abs(x[i-1])))
      x[i] = y;
    else{
      x[i] = x[i-1] ;
      k=k+1;
    }
  }
  return(x);
}


