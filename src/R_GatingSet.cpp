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
typedef vector<string> StringVec;
RcppExport SEXP R_parseWorkspace(SEXP _fileName,SEXP _sampleIDs,SEXP _execute,SEXP _dMode) {
BEGIN_RCPP
		string fileName=as<string>(_fileName);
		unsigned short dMode=as<unsigned short>(_dMode);
		StringVec sampleIDs=as<StringVec>(_sampleIDs);
		bool isParseGate=as<bool>(_execute);
//		XPtr<GatingSet>gs(_gsPtr);
		GatingSet * gs=new GatingSet(fileName,dMode);
		gs->parseWorkspace(sampleIDs,isParseGate);
		return XPtr<GatingSet>(gs);
//		return R_NilValue;
END_RCPP
}

/*
 * associate nc file as the raw data to GatingSet
 */
RcppExport SEXP R_setData(SEXP _gsPtr,SEXP _fileName,SEXP _sampleNames,SEXP _params) {
BEGIN_RCPP
		string fileName=as<string>(_fileName);
		vector<string> params=as<vector<string> >(_params);
		vector<string> sampleNames=as<vector<string> >(_sampleNames);
		XPtr<GatingSet>gs(_gsPtr);
		gs->attachData(fileName,sampleNames,params);

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


