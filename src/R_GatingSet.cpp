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
RcppExport SEXP R_parseWorkspace(SEXP _fileName,SEXP _sampleIDs,SEXP _isParseGate,SEXP _sampNloc,SEXP _dMode) {
BEGIN_RCPP
		string fileName=as<string>(_fileName);
		unsigned short dMode=as<unsigned short>(_dMode);
		StringVec sampleIDs=as<StringVec>(_sampleIDs);
		bool isParseGate=as<bool>(_isParseGate);
		unsigned short sampNloc=as<unsigned short>(_sampNloc);
		GatingSet * gs=new GatingSet(fileName,isParseGate,sampNloc,dMode);
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
 * Deprecated: we don't want to keep a separate view of ncdfFlowSet in c++
 */
//RcppExport SEXP R_setData(SEXP _gsPtr,SEXP _fileName,SEXP _sampleNames,SEXP _params) {
//BEGIN_RCPP
//
//		string fileName=as<string>(_fileName);
//		vector<string> params=as<vector<string> >(_params);
//		vector<string> sampleNames=as<vector<string> >(_sampleNames);
//		XPtr<GatingSet>gs(_gsPtr);
//
//		gs->attachData(fileName,sampleNames,params);
//
//END_RCPP
//}

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
		GatingSet * newGS=new GatingSet(gh,newSampleNames,dMode);

		/*
		 * using default finalizer to delete gs,which is triggered by gc() when
		 * xptr is out of scope
		 */

		return XPtr<GatingSet>(newGS);

END_RCPP
}

/*
 * constructing GatingSet with only root node for each sample
 */
RcppExport SEXP R_NewGatingSet_rootOnly(SEXP _sampleNames,SEXP _dMode) {
BEGIN_RCPP


		StringVec sampleNames=as<StringVec>(_sampleNames);


		unsigned short dMode=as<unsigned short>(_dMode);


		GatingSet * newGS=new GatingSet(sampleNames,dMode);

		return XPtr<GatingSet>(newGS);

END_RCPP
}

/*
 * save/load GatingSet
 */
RcppExport SEXP R_saveGatingSet(SEXP _gsPtr,SEXP _fileName) {
BEGIN_RCPP



		XPtr<GatingSet>gs(_gsPtr);

		string fileName=as<string>(_fileName);
		save_gs(*gs,fileName);


END_RCPP
}


RcppExport SEXP R_loadGatingSet(SEXP _fileName) {
BEGIN_RCPP


		GatingSet * gs=new GatingSet();
		string fileName=as<string>(_fileName);
		restore_gs(*gs,fileName);

		return XPtr<GatingSet>(gs);


END_RCPP
}


RcppExport SEXP R_CloneGatingSet(SEXP _gsPtr,SEXP _samples) {
BEGIN_RCPP


		XPtr<GatingSet>gs(_gsPtr);
		StringVec samples=as<StringVec>(_samples);

		GatingSet * gs_new=gs->clone_treeOnly(samples);

		return XPtr<GatingSet>(gs_new);


END_RCPP
}

RcppExport SEXP R_combineGatingSet(SEXP _gsPtrs,SEXP _samples) {
BEGIN_RCPP

		Rcpp::List gsList(_gsPtrs);
		Rcpp::List sampleList(_samples);

		GatingSet * newGS=new GatingSet();

		for(unsigned i=0;i<gsList.size();i++)
		{
			XPtr<GatingSet>gs((SEXP)gsList[i]);
			StringVec samples=as<StringVec>(sampleList[i]);
			newGS->add(*gs,samples);
		}


		return XPtr<GatingSet>(newGS);


END_RCPP
}

RcppExport SEXP R_addGate(SEXP _gsPtr,SEXP _filterList,SEXP _parent,SEXP _popName) {
BEGIN_RCPP


		XPtr<GatingSet>gs(_gsPtr);
		string parent=as<string>(_parent);
		string popName=as<string>(_popName);
		List flist(_filterList);
		gate * g;//TODO::convert R filter to gate class
		gs->add(g,parent,popName);




END_RCPP
}

