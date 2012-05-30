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

typedef vector<string> StringVec;
RcppExport SEXP R_parseWorkspace(SEXP _fileName,SEXP _sampleIDs,SEXP _execute,SEXP _dMode) {
BEGIN_RCPP
		string fileName=as<string>(_fileName);
		unsigned short dMode=as<unsigned short>(_dMode);
		StringVec sampleIDs=as<StringVec>(_sampleIDs);
		bool isParseGate=as<bool>(_execute);
		GatingSet * gs=new GatingSet(fileName,isParseGate,dMode);
		gs->parseWorkspace(sampleIDs,isParseGate);
		/*
		 * using default finalizer to delete gs,which is triggered by gc() when
		 * xptr is out of scope
		 */

		return XPtr<GatingSet>(gs);

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

RcppExport SEXP R_getSamples(SEXP _gsPtr) {
BEGIN_RCPP
	XPtr<GatingSet>gs(_gsPtr);
	return wrap(gs->getSamples());

END_RCPP
}


