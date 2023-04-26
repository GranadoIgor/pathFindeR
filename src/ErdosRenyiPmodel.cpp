#include <Rcpp.h>

using namespace Rcpp;

//' Erdős–Rényi model for random graph creation

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
Rcpp::DataFrame ErdosRenyiPmodel(int n, double p) {

  NumericVector prob(1);
  std::vector<int> from;
  std::vector<int> to;

  for (int i = 0; i < n; i++)  {
    for (int j = 0; j < n; j++)  {

      prob = Rcpp::runif(1, 0, 1);

      if (prob[0] <= p) {
          from.push_back(i);
          to.push_back(j);
      }
    }
  }

  DataFrame out = DataFrame::create(Named("from") = from,
                                    Named("to")   = to);

  return  out;
}
