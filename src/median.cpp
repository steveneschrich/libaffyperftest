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
double naive_median1(NumericVector x, bool na_rm = false)
{
  return(Rcpp::median(x,na_rm));
}
/*
 * Calculate the median of n numbers (x[0]..x[n-1]) without touching
 *  the x array.
 */
// [[Rcpp::export]]
double naive_median2(Rcpp::NumericVector x, bool na_rm = false)
{
  Rcpp::NumericVector d;
  double  M;
  int n;

  
  if ( na_rm ) {
    d = Rcpp::na_omit(x);
  } else {
    d = Rcpp::clone(x);
  }
  n = d.size();
  
  d = d.sort();
  /* Median is middle value, or mean of two middle values */
  if (n % 2 == 1)
    M = d[n / 2];
  else
    M = (d[(n / 2) - 1] + d[n / 2]) / 2.0;
  
  //*range_ptr = d[n-1] - d[0];
  
  //h_free(d);
  
  return (M);
}

// [[Rcpp::export]]
double naive_median3(Rcpp::NumericVector x, bool na_rm = false) {
  Rcpp::NumericVector d = Rcpp::clone(x);
  //d = d.sort();
  return(d[0]);
  
}