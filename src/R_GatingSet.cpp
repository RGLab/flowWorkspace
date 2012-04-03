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
#include "GatingSet.hpp"
using namespace Rcpp;

//RCPP_MODULE(cpp_GatingSet){
//
//	class_<GatingSet>(".GatingSet")
//	.method("",&GatingSet::parseWorkspace)
//}

RcppExport SEXP R_openWorkspace(SEXP _fileName) {
BEGIN_RCPP

	string fileName=as<string>(_fileName);
	XPtr<GatingSet>ptr(new GatingSet(fileName));

    return ptr;
END_RCPP
}

RcppExport SEXP R_parseWorkspace(SEXP _gsPtr,SEXP _groupID) {
BEGIN_RCPP
		XPtr<GatingSet>gs(_gsPtr);
		unsigned short groupID=as<unsigned short>(_groupID);

		gs->parseWorkspace(groupID);
		return wrap(0);
END_RCPP
		return wrap(-1);
}

RcppExport SEXP R_getGatingHierarchyS(SEXP _gsPtr,SEXP _sampleName) {
BEGIN_RCPP
	XPtr<GatingSet>gs(_gsPtr);
	string sampleName=as<string>(_sampleName);

	GatingHierarchy * gh=gs->getGatingHierarchy(sampleName);

	XPtr<GatingHierarchy>ptr(gh);
	return ptr;

END_RCPP
}
RcppExport SEXP R_getGatingHierarchyI(SEXP _gsPtr,SEXP _i) {
BEGIN_RCPP
	XPtr<GatingSet>gs(_gsPtr);
	unsigned i=as<unsigned>(_i);

	GatingHierarchy * gh=gs->getGatingHierarchy(i);

	XPtr<GatingHierarchy>ptr(gh);
	return ptr;

END_RCPP
}
//since delete is not working with xptr, make sure it is released by R
//RcppExport SEXP R_closeWorkspace(SEXP _gsPtr) {
//BEGIN_RCPP
//	XPtr<GatingSet>gs(_gsPtr);
//	delete gs;
//	cout<<"Gating Set is released!"<<endl;
//	return 0;
//END_RCPP
//	return IntegerVector(-1);
//}
//static void cooked_goose(SEXP foo)
//{
//    if (TYPEOF(foo) != EXTPTRSXP)
//        error("argument not external pointer");
//    GatingSet *x = (GatingSet *) R_ExternalPtrAddr(foo);
////    int blather = x[0];
//    Free(x);
//    cout<<"finalizer ran!"<<endl;
//}
//




