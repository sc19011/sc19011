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


