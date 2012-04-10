/*
 * R_GatingSet.cpp
 *
 *these are R APIs
 *
 *  Created on: Mar 30, 2012
 *      Author: wjiang2
 */
#include "include/R_GatingSet.hpp"

/*
 * can't use module for exposing overloaded methods
 */

//RcppExport SEXP R_openWorkspace(SEXP _fileName,SEXP _dMode) {
//BEGIN_RCPP
//
//	string fileName=as<string>(_fileName);
//	unsigned short dMode=as<unsigned short>(_dMode);
//	XPtr<GatingSet>ptr(new GatingSet(fileName,dMode));
//
//    return ptr;
//END_RCPP
//}

RcppExport SEXP R_parseWorkspace(SEXP _fileName,SEXP _groupID,SEXP _execute,SEXP _dMode) {
BEGIN_RCPP
		string fileName=as<string>(_fileName);
		unsigned short dMode=as<unsigned short>(_dMode);
		unsigned short groupID=as<unsigned short>(_groupID);
		bool isGating=as<bool>(_execute);
//		XPtr<GatingSet>gs(_gsPtr);
		GatingSet * gs=new GatingSet(fileName,dMode);
		gs->parseWorkspace(groupID,isGating);
		return XPtr<GatingSet>(gs);
//		return R_NilValue;
END_RCPP
}

//RcppExport SEXP R_getGatingHierarchyS(SEXP _gsPtr,SEXP _sampleName) {
//BEGIN_RCPP
//	XPtr<GatingSet>gs(_gsPtr);
//	string sampleName=as<string>(_sampleName);
//
//	GatingHierarchy gh=gs->getGatingHierarchy(sampleName);
//
//	XPtr<GatingHierarchy>ptr(gh);
//	return ptr;
//
//END_RCPP
//}
//RcppExport SEXP R_getGatingHierarchyI(SEXP _gsPtr,SEXP _i) {
//BEGIN_RCPP
//	XPtr<GatingSet>gs(_gsPtr);
//	unsigned i=as<unsigned>(_i);
//	GatingHierarchy * gh=gs->getGatingHierarchy(i);
//
//	XPtr<GatingHierarchy>ptr(gh);
//	return ptr;
//
//END_RCPP
//}
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

RcppExport SEXP R_getSamples(SEXP _gsPtr) {
BEGIN_RCPP
	XPtr<GatingSet>gs(_gsPtr);
	return wrap(gs->getSamples());

END_RCPP
}


