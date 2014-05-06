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


RcppExport SEXP R_getSample(SEXP _ghPtr);

#endif /* R_GATINGHIERARCHY_HPP_ */
