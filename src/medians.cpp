#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
double median(Rcpp::NumericVector x) {
  return(Rcpp::median(x));
}

// [[Rcpp::export]]
Rcpp::NumericVector col_medians(Rcpp::NumericMatrix x) {
  Rcpp::NumericVector m(x.cols());
  int nc = x.cols();
  
  for (int i = 0; i < nc; i++) {
    NumericVector z = x(_, i);
    m[i] = Rcpp::median(z);
  }
  return(m);
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
timesTwo(42)
*/
