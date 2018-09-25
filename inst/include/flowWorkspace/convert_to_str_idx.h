/*
 * vectorSubsetting.hpp
 *
 *  Created on: Feb 26, 2018
 *      Author: wjiang2
 */

#ifndef VECTORSUBSETTING_HPP_
#define VECTORSUBSETTING_HPP_

#include <RcppCommon.h>
using namespace Rcpp;

StringVector convert_to_str_idx(StringVector x, SEXP i) {
  int type = TYPEOF(i);
  if(type == LGLSXP)
    return x[as<LogicalVector>(i)];
  else if(type == REALSXP || type == INTSXP)
    return x[as<IntegerVector>(i)-1];
  else
    stop("Failed to convert to string index due to unsupported SEXP type!");
}

// [[Rcpp::export]]
vector<unsigned> convert_to_uint_idx(unsigned n, SEXP i) {
  int type = TYPEOF(i);
  vector<unsigned> res;
  if(type == LGLSXP)
  {
    vector<bool> idx = as<vector<bool>>(as<LogicalVector>(i));
    if(idx.size()!=n)
      stop("Logical index has different length from data size: " + to_string(n));
    for(unsigned i = 0; i < n; i++)
      if(idx[i])
        res.push_back(i);
  }
  else if(type == REALSXP || type == INTSXP)
  {
    IntegerVector idx = as<IntegerVector>(i)-1;
    res = as<vector<unsigned>>(idx);
  }
  else
    stop("invalid type of i");
  return res;
}


#endif /* VECTORSUBSETTING_HPP_ */
