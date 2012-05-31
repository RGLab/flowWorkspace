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
/*
 * constructing GatingSet from xml file
 */
RcppExport SEXP R_parseWorkspace(SEXP _fileName,SEXP _sampleIDs,SEXP _isParseGate,SEXP _dMode) {
BEGIN_RCPP
		string fileName=as<string>(_fileName);
		unsigned short dMode=as<unsigned short>(_dMode);
		StringVec sampleIDs=as<StringVec>(_sampleIDs);
		bool isParseGate=as<bool>(_isParseGate);
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

/*
 * constructing GatingSet from existing gating hierarchy and new data
 */
RcppExport SEXP R_NewGatingSet(SEXP _gsPtr,SEXP _sampleName,SEXP _newSampleNames,SEXP _dMode) {
BEGIN_RCPP

		/*
		 * get pointer of existing gating hierarchy
		 *
		 */
		XPtr<GatingSet>gs(_gsPtr);
		string sampleName=as<string>(_sampleName);
		GatingHierarchy* gh=gs->getGatingHierarchy(sampleName);

		unsigned short dMode=as<unsigned short>(_dMode);

		/*
		 * used gh as the template to clone multiple ghs in the new gs
		 */
		StringVec newSampleNames=as<StringVec>(_newSampleNames);
		GatingSet * newGS=new GatingSet(*gh,newSampleNames,dMode);

		/*
		 * using default finalizer to delete gs,which is triggered by gc() when
		 * xptr is out of scope
		 */

		return XPtr<GatingSet>(newGS);

END_RCPP
}



