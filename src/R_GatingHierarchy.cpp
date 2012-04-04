/*
 * R_GatingSet.cpp
 *
 *these are R APIs
 *
 *  Created on: Mar 30, 2012
 *      Author: wjiang2
 */
//#include <Rinternals.h>
//#include <Rdefines.h>
//#include <Rmath.h>
//#include "GatingSet.hpp"

#include <Rcpp.h>
//#include <cmath>
#include <GatingHierarchy.hpp>
using namespace Rcpp;

RcppExport SEXP R_plotGh(SEXP _ghPtr) {
BEGIN_RCPP

	XPtr<GatingHierarchy>gh(_ghPtr);
 	 string res=gh->drawGraph();
    return wrap(res);
END_RCPP
}

//RcppExport SEXP R_parseWorkspace(SEXP _gsPtr,SEXP _groupID) {
//BEGIN_RCPP
//		XPtr<GatingSet>gs(_gsPtr);
//		unsigned short groupID=as<unsigned short>(_groupID);
//
//		gs->parseWorkspace(groupID);
//		return 0;
//END_RCPP
//		return IntegerVector(-1);
//}
//
//RcppExport SEXP R_getGatingHierarchy(SEXP _gsPtr,SEXP _sampleName) {
//BEGIN_RCPP
//	XPtr<GatingSet>gs(_gsPtr);
//	string sampleName=as<string>(_sampleName);
//
//	GatingHierarchy * gh=gs->getGatingHierarchy(sampleName);
//
//	XPtr<GatingHierarchy>ptr(gh);
//	return ptr;
//
//END_RCPP
//}






