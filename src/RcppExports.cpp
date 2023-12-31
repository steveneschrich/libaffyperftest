// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// mean
double mean(Rcpp::NumericVector x);
RcppExport SEXP _libaffyperftest_mean(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(mean(x));
    return rcpp_result_gen;
END_RCPP
}
// col_means
Rcpp::NumericVector col_means(Rcpp::NumericMatrix x);
RcppExport SEXP _libaffyperftest_col_means(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(col_means(x));
    return rcpp_result_gen;
END_RCPP
}
// naive_median1
double naive_median1(NumericVector x, bool na_rm);
RcppExport SEXP _libaffyperftest_naive_median1(SEXP xSEXP, SEXP na_rmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    rcpp_result_gen = Rcpp::wrap(naive_median1(x, na_rm));
    return rcpp_result_gen;
END_RCPP
}
// naive_median2
double naive_median2(Rcpp::NumericVector x, bool na_rm);
RcppExport SEXP _libaffyperftest_naive_median2(SEXP xSEXP, SEXP na_rmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    rcpp_result_gen = Rcpp::wrap(naive_median2(x, na_rm));
    return rcpp_result_gen;
END_RCPP
}
// naive_median3
double naive_median3(Rcpp::NumericVector x, bool na_rm);
RcppExport SEXP _libaffyperftest_naive_median3(SEXP xSEXP, SEXP na_rmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    rcpp_result_gen = Rcpp::wrap(naive_median3(x, na_rm));
    return rcpp_result_gen;
END_RCPP
}
// median
double median(Rcpp::NumericVector x);
RcppExport SEXP _libaffyperftest_median(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(median(x));
    return rcpp_result_gen;
END_RCPP
}
// col_medians
Rcpp::NumericVector col_medians(Rcpp::NumericMatrix x);
RcppExport SEXP _libaffyperftest_col_medians(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(col_medians(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_hello_world
List rcpp_hello_world();
RcppExport SEXP _libaffyperftest_rcpp_hello_world() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(rcpp_hello_world());
    return rcpp_result_gen;
END_RCPP
}
// tukey_biweight
double tukey_biweight(NumericVector x, int c, double epsilon);
RcppExport SEXP _libaffyperftest_tukey_biweight(SEXP xSEXP, SEXP cSEXP, SEXP epsilonSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type c(cSEXP);
    Rcpp::traits::input_parameter< double >::type epsilon(epsilonSEXP);
    rcpp_result_gen = Rcpp::wrap(tukey_biweight(x, c, epsilon));
    return rcpp_result_gen;
END_RCPP
}
// col_tukey_biweights
Rcpp::NumericVector col_tukey_biweights(Rcpp::NumericMatrix x, int c, double epsilon);
RcppExport SEXP _libaffyperftest_col_tukey_biweights(SEXP xSEXP, SEXP cSEXP, SEXP epsilonSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type c(cSEXP);
    Rcpp::traits::input_parameter< double >::type epsilon(epsilonSEXP);
    rcpp_result_gen = Rcpp::wrap(col_tukey_biweights(x, c, epsilon));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_libaffyperftest_mean", (DL_FUNC) &_libaffyperftest_mean, 1},
    {"_libaffyperftest_col_means", (DL_FUNC) &_libaffyperftest_col_means, 1},
    {"_libaffyperftest_naive_median1", (DL_FUNC) &_libaffyperftest_naive_median1, 2},
    {"_libaffyperftest_naive_median2", (DL_FUNC) &_libaffyperftest_naive_median2, 2},
    {"_libaffyperftest_naive_median3", (DL_FUNC) &_libaffyperftest_naive_median3, 2},
    {"_libaffyperftest_median", (DL_FUNC) &_libaffyperftest_median, 1},
    {"_libaffyperftest_col_medians", (DL_FUNC) &_libaffyperftest_col_medians, 1},
    {"_libaffyperftest_rcpp_hello_world", (DL_FUNC) &_libaffyperftest_rcpp_hello_world, 0},
    {"_libaffyperftest_tukey_biweight", (DL_FUNC) &_libaffyperftest_tukey_biweight, 3},
    {"_libaffyperftest_col_tukey_biweights", (DL_FUNC) &_libaffyperftest_col_tukey_biweights, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_libaffyperftest(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
