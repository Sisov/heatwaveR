#include <Rcpp.h>
using namespace Rcpp;

// run length encoding
// input:
//    numeric vector
// output:
//    numeric matrix:
//      column 1: value making up the run
//      column 2: start index of the run of values in column 1
//      column 3: end index of the run of values in column 1
//      column 4: length of the run of values in column 1

// declare replication function
// found at:
// https://stackoverflow.com/questions/28442582/reproducing-r-rep-with-the-times-argument-in-c-and-rcpp
NumericVector rep_vec(const NumericVector& x, const IntegerVector& times) {

  std::size_t n = times.size();
  if (n != 1 && n != x.size())
    stop("Invalid 'times' value");

  std::size_t n_out = std::accumulate(times.begin(), times.end(), 0);
  NumericVector res = no_init(n_out);
  auto begin = res.begin();

  for (std::size_t i = 0, ind = 0; i < n; ind += times[i], ++i) {
    auto start = begin + ind;
    auto end = start + times[i];
    std::fill(start, end, x[i]);
  }

  return res;

}

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
List rle2_proto(NumericVector x) {

  int n = x.size();
  int rows = 1;

  for (int a = 1; a < n; ++a)

    if (x(a) != x(a - 1)) rows += 1;

    NumericMatrix out(rows, 5);
    double presval = x(0);
    int prespos = 0;
    int index = -1;

    for (int b = 1; b < n; ++b) {
      if (x(b) != presval) {
        index += 1;
        out(index, 1) = presval;      // equal to R rle $values
        out(index, 2) = prespos + 1;
        out(index, 3) = b;
        out(index, 4) = b - prespos;  // equal to R rle $lengths
        presval = x(b);
        prespos = b;
      }
    }

    index += 1;
    out(index, 1) = presval;
    out(index, 2) = prespos + 1;
    out(index, 3) = n;
    out(index, 4) = n - prespos;

    int n_out = out.nrow();
    NumericMatrix n_seq(n_out, 1);
    n_seq(_, 0) = seq(1, n_out);
    out(_, 0) = n_seq;

    colnames(out) = Rcpp::CharacterVector::create("number", "value", "index_start", "index_end", "duration");

    NumericVector _times_vec = out(_, 4);
    IntegerVector times_vec = as<IntegerVector>(_times_vec);
    NumericVector res(0);
    res = rep_vec(n_seq, times_vec);

    List ret;
    ret["rle"] = out;
    ret["index"] = res;
    return ret;

}
