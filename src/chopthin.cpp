#include <Rcpp.h>

using namespace Rcpp;

int intrand(const int n) { return floor(unif_rand()*n); }
inline double myrunif(){return unif_rand();}
#define chopthin_error(x) throw Rcpp::exception((x))
#include "chopthin_internal.h"

//' @title The Chopthin Resampler
//' @description A CPP implementation of the Chopthin resampler. Can be used as the resampling step in particle filters and in sequential Monte Carlo.
//' @param w a vector of weights
//' @param N target number of particles
//' @param eta upper bound on the ratio between the weights. Must be >=4.
//' @return A list with two elements: new weights and indices of the ancestors of the new particles. The weights are normalised to add up to N.
//'
//' @examples
//' \dontrun{
//' chopthin(runif(10),N=10)
//' chopthin(runif(10),N=20,4)
//' chopthin(runif(10),N=5)
//' chopthin(runif(10),N=1)
//' }
//' @export
// [[Rcpp::export]]
List chopthin(std::vector<double>& w, int N, double eta=5.828427){
  std::vector<double> wres(N);
  std::vector<int> ires(N);
  chopthin_internal(w,N,wres,ires,eta);
  List res;
  res["weights"]=NumericVector(wres.begin(),wres.end());
  res["indices"]=IntegerVector(ires.begin(),ires.end());
  return res;
}

