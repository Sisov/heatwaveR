// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// mw_cpp
NumericVector mw_cpp(DataFrame d);
RcppExport SEXP _heatwaveR_mw_cpp(SEXP dSEXP) {
  BEGIN_RCPP
  Rcpp::RObject rcpp_result_gen;
  Rcpp::RNGScope rcpp_rngScope_gen;
  Rcpp::traits::input_parameter< DataFrame >::type d(dSEXP);
  rcpp_result_gen = Rcpp::wrap(mw_cpp(d));
  return rcpp_result_gen;
  END_RCPP
}
// mw_seq_cpp
NumericVector mw_seq_cpp(DataFrame d);
RcppExport SEXP _heatwaveR_mw_seq_cpp(SEXP dSEXP) {
  BEGIN_RCPP
  Rcpp::RObject rcpp_result_gen;
  Rcpp::RNGScope rcpp_rngScope_gen;
  Rcpp::traits::input_parameter< DataFrame >::type d(dSEXP);
  rcpp_result_gen = Rcpp::wrap(mw_seq_cpp(d));
  return rcpp_result_gen;
  END_RCPP
}
// sumCpp
int sumCpp(Rcpp::IntegerVector x);
RcppExport SEXP _heatwaveR_sumCpp(SEXP xSEXP) {
  BEGIN_RCPP
  Rcpp::RObject rcpp_result_gen;
  Rcpp::RNGScope rcpp_rngScope_gen;
  Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type x(xSEXP);
  rcpp_result_gen = Rcpp::wrap(sumCpp(x));
  return rcpp_result_gen;
  END_RCPP
}
// rcpparma_hello_world
arma::mat rcpparma_hello_world();
RcppExport SEXP _heatwaveR_rcpparma_hello_world() {
  BEGIN_RCPP
  Rcpp::RObject rcpp_result_gen;
  Rcpp::RNGScope rcpp_rngScope_gen;
  rcpp_result_gen = Rcpp::wrap(rcpparma_hello_world());
  return rcpp_result_gen;
  END_RCPP
}
// rcpparma_outerproduct
arma::mat rcpparma_outerproduct(const arma::colvec& x);
RcppExport SEXP _heatwaveR_rcpparma_outerproduct(SEXP xSEXP) {
  BEGIN_RCPP
  Rcpp::RObject rcpp_result_gen;
  Rcpp::RNGScope rcpp_rngScope_gen;
  Rcpp::traits::input_parameter< const arma::colvec& >::type x(xSEXP);
  rcpp_result_gen = Rcpp::wrap(rcpparma_outerproduct(x));
  return rcpp_result_gen;
  END_RCPP
}
// rcpparma_innerproduct
double rcpparma_innerproduct(const arma::colvec& x);
RcppExport SEXP _heatwaveR_rcpparma_innerproduct(SEXP xSEXP) {
  BEGIN_RCPP
  Rcpp::RObject rcpp_result_gen;
  Rcpp::RNGScope rcpp_rngScope_gen;
  Rcpp::traits::input_parameter< const arma::colvec& >::type x(xSEXP);
  rcpp_result_gen = Rcpp::wrap(rcpparma_innerproduct(x));
  return rcpp_result_gen;
  END_RCPP
}
// rcpparma_bothproducts
Rcpp::List rcpparma_bothproducts(const arma::colvec& x);
RcppExport SEXP _heatwaveR_rcpparma_bothproducts(SEXP xSEXP) {
  BEGIN_RCPP
  Rcpp::RObject rcpp_result_gen;
  Rcpp::RNGScope rcpp_rngScope_gen;
  Rcpp::traits::input_parameter< const arma::colvec& >::type x(xSEXP);
  rcpp_result_gen = Rcpp::wrap(rcpparma_bothproducts(x));
  return rcpp_result_gen;
  END_RCPP
}
// add_vecs
arma::vec add_vecs(arma::vec a, arma::vec b);
RcppExport SEXP _heatwaveR_add_vecs(SEXP aSEXP, SEXP bSEXP) {
  BEGIN_RCPP
  Rcpp::RObject rcpp_result_gen;
  Rcpp::RNGScope rcpp_rngScope_gen;
  Rcpp::traits::input_parameter< arma::vec >::type a(aSEXP);
  Rcpp::traits::input_parameter< arma::vec >::type b(bSEXP);
  rcpp_result_gen = Rcpp::wrap(add_vecs(a, b));
  return rcpp_result_gen;
  END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
  {"_heatwaveR_mw_cpp", (DL_FUNC) &_heatwaveR_mw_cpp, 1},
  {"_heatwaveR_mw_seq_cpp", (DL_FUNC) &_heatwaveR_mw_seq_cpp, 1},
  {"_heatwaveR_sumCpp", (DL_FUNC) &_heatwaveR_sumCpp, 1},
  {"_heatwaveR_rcpparma_hello_world", (DL_FUNC) &_heatwaveR_rcpparma_hello_world, 0},
  {"_heatwaveR_rcpparma_outerproduct", (DL_FUNC) &_heatwaveR_rcpparma_outerproduct, 1},
  {"_heatwaveR_rcpparma_innerproduct", (DL_FUNC) &_heatwaveR_rcpparma_innerproduct, 1},
  {"_heatwaveR_rcpparma_bothproducts", (DL_FUNC) &_heatwaveR_rcpparma_bothproducts, 1},
  {"_heatwaveR_add_vecs", (DL_FUNC) &_heatwaveR_add_vecs, 2},
  {NULL, NULL, 0}
};

RcppExport void R_init_heatwaveR(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
