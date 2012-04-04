/*
 * R_GatingHierarchy.hpp
 *
 *  Created on: Apr 4, 2012
 *      Author: wjiang2
 */

#ifndef R_GATINGHIERARCHY_HPP_
#define R_GATINGHIERARCHY_HPP_

//#include <Rinternals.h>
//#include <Rdefines.h>
//#include <Rmath.h>
//#include "GatingSet.hpp"

#include <Rcpp.h>
//#include <cmath>
#include "GatingHierarchy.hpp"
using namespace Rcpp;
RcppExport SEXP R_plotGh(SEXP _ghPtr);
RcppExport SEXP R_getSample(SEXP _ghPtr);

#endif /* R_GATINGHIERARCHY_HPP_ */
