#include <Rcpp.h>
using namespace Rcpp;
//' @title A random walk Metropolis sampler using Rcpp
//' @description Implement a random walk Metropolis sampler for generating the standard Laplace distribution using Rcpp
//' @param N the number of samples
//' @param sigma parameter for produces a normal distribution random sample
//' @param x0  Initial value
//' @return a random sample of size \code{n}
//' @examples
//' \dontrun{
//' sigma <- c(.05, .5, 2, 16)
//' x0 <- 25
//' rw1c <- rwMetropolisC(sigma[1], x0, N)
//' }
//' @export
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


