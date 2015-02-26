/*
 * R_GatingSet.hpp
 *
 *  Created on: Apr 4, 2012
 *      Author: wjiang2
 */

#ifndef R_GATINGSET_HPP_
#define R_GATINGSET_HPP_



#include <Rcpp.h>

#include "GatingSet.hpp"
using namespace Rcpp;
RcppExport SEXP R_openWorkspace(SEXP _fileName);

//RcppExport SEXP R_parseWorkspace(SEXP _gsPtr,SEXP _groupID);

RcppExport SEXP R_getGatingHierarchyS(SEXP _gsPtr,SEXP _sampleName);
RcppExport SEXP R_getGatingHierarchyI(SEXP _gsPtr,SEXP _i);

RcppExport SEXP R_getSamples(SEXP _gsPtr);

GatingSet * getGsPtr(SEXP _gsPtr);
#endif /* R_GATINGSET_HPP_ */
