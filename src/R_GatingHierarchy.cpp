/*
 * R_GatingSet.cpp
 *
 *these are R APIs
 *
 *  Created on: Mar 30, 2012
 *      Author: wjiang2
 */

/*
 * can't use module for exposing overloaded methods and non-standard wrap/as type of the constructor
 * Also each GatingHierarchy object is created by GatingSet method within c++
 * thus it is not initialized by Rcpp module as S4 class within R. So have to use this tedious way to
 * write R API
 */
#include "include/R_GatingHierarchy.hpp"

RcppExport SEXP R_plotGh(SEXP _ghPtr) {
BEGIN_RCPP

	XPtr<GatingHierarchy>gh(_ghPtr);
 	 string res=gh->drawGraph();
    return wrap(res);
END_RCPP
}

RcppExport SEXP R_getSample(SEXP _ghPtr){
BEGIN_RCPP

	XPtr<GatingHierarchy>gh(_ghPtr);
	 return wrap(gh->getSample());
END_RCPP
}

RcppExport SEXP R_getNodes(SEXP _ghPtr,SEXP _verID){
BEGIN_RCPP

	XPtr<GatingHierarchy>gh(_ghPtr);
	return wrap(gh->getNodeList());
END_RCPP
}

RcppExport SEXP R_getParent(SEXP _ghPtr,SEXP _i){
BEGIN_RCPP

	XPtr<GatingHierarchy>gh(_ghPtr);
	int u=as<int>(_i);
	return wrap(gh->getParent(u));
END_RCPP
}
//
//RcppExport SEXP R_getParentI(SEXP _ghPtr,SEXP _verID){
//BEGIN_RCPP
//
//	XPtr<GatingHierarchy>gh(_ghPtr);
//	return wrap(gh->getSample());
//END_RCPP
//}






