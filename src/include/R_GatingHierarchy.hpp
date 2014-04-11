/*
 * R_GatingHierarchy.hpp
 *
 *  Created on: Apr 4, 2012
 *      Author: wjiang2
 */

#ifndef R_GATINGHIERARCHY_HPP_
#define R_GATINGHIERARCHY_HPP_

#include <Rcpp.h>
#include "GatingHierarchy.hpp"
using namespace Rcpp;

/*
 * because __SIZE_TYPE__ is long long unsigned int by gcc on win64 (mingw64)
 * we cast it to unsigned int before pass it to Rcpp::wrap to avoid error
 */
typedef unsigned int NODEID;

RcppExport SEXP R_getSample(SEXP _ghPtr);

#endif /* R_GATINGHIERARCHY_HPP_ */
