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



// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
timesTwo(42)
*/



/*--------------------------------------------------------------------
 Calculate Tukey's Biweighted Average, per the Affy docs.
 --------------------------------------------------------------------*/
// [[Rcpp::export]]
double tukey_biweight(NumericVector x, int c = 5, double epsilon = 1e-04)
{
  int     i, *mempool;
  double  M, S;
  double  Tbi_num = 0, Tbi_denom = 0;
  double  range;
  int n = x.size();
    
  /* special case for n == 1, to avoid any potential issues */
  if (n == 1)
  {
    return x[0];
  }
  
  /* special case for n == 2, to avoid potential epsilon-related errors */
  if (n == 2)
  {
    return (0.5 * (x[0] + x[1]));
  }
  
  /* Calculate median */
  M = Rcpp::median(x);
  //M = median(x, n, &range, err);
  NumericVector diffs = Rcpp::abs(x - M);
  S = Rcpp::median(diffs);
  NumericVector u = (x - M) / (c * S + epsilon);
  
  
  for (i = 0; i < n; i++)
  {
         double usquared, w;
     
         /* Function w(u) is 0 for all |u|>1 */
         if (fabs(u[i]) > 1)
           continue;
         
         usquared = u[i] * u[i];
         w = (1.0 - usquared) * (1.0 - usquared);
         Tbi_num += w * x[i];
         Tbi_denom += w;
  }
    //   
    //   /* If all points are distant, take a flat average instead */
    //   if (Tbi_denom <= DBL_EPSILON)
    //   {
    //     Tbi_denom = n;
    //     Tbi_num = 0.0;
    //     
    //     for (i = 0; i < n; i++)
    //       Tbi_num += x[i];
    //   }
    //   
    
    
  //   S = median(diffs, n, &range, err);
 return(Tbi_num / Tbi_denom);
}

// [[Rcpp::export]]
Rcpp::NumericVector col_tukey_biweights(Rcpp::NumericMatrix x, int c = 5, double epsilon=1e-04) {
  Rcpp::NumericVector m(x.cols());
  int nc = x.cols();
  
  for (int i = 0; i < nc; i++) {
    m[i] = tukey_biweight(x(_,i));
  }
  return(m);
}

//   /* zero variance, return first value */
//   if (range <= DBL_EPSILON)
//   {
//     h_free(mempool);
//     return x[0];
//   }
//   
//   /* Calculate S, median of absolute differences from M */
//   diffs = h_suballoc(mempool, n * sizeof(double));
//   if (diffs == NULL)
//   {
//     h_free(mempool);
//     AFFY_HANDLE_ERROR("malloc failed", AFFY_ERROR_OUTOFMEM, err, 0.0);
//   }
//   
//   for (i = 0; i < n; i++)
//     diffs[i] = fabs(x[i] - M);
//   
//   S = median(diffs, n, &range, err);
//   
//   if (err->type != AFFY_ERROR_NONE)
//   {
//     h_free(mempool);
//     
//     return (0.0);
//   }
//   
//   /* Calculate distance measure */
//   for (i = 0; i < n; i++)
//     u[i] = (x[i] - M) / (c * S + epsilon);
//   
//   /* Finally, calculate the result */
//   for (i = 0; i < n; i++)
//   {
//     double usquared, w;
//     
//     /* Function w(u) is 0 for all |u|>1 */
//     if (fabs(u[i]) > 1)
//       continue;
//     
//     usquared = u[i] * u[i];
//     w = (1.0 - usquared) * (1.0 - usquared);
//     Tbi_num += w * x[i];
//     Tbi_denom += w;
//   }
//   
//   /* If all points are distant, take a flat average instead */
//   if (Tbi_denom <= DBL_EPSILON)
//   {
//     Tbi_denom = n;
//     Tbi_num = 0.0;
//     
//     for (i = 0; i < n; i++)
//       Tbi_num += x[i];
//   }
//   
//   h_free(mempool);
//   
//   return (Tbi_num / Tbi_denom);
// }
