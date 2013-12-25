/*
 * R_GatingSet.cpp
 *
 *these are R APIs
 *
 *  Created on: Mar 30, 2012
 *      Author: wjiang2
 */
#include "include/R_GatingSet.hpp"

GatingSet * getGsPtr(SEXP _gsPtr){

	if(R_ExternalPtrAddr(_gsPtr)==0)
			throw(domain_error("Null GatingSet pointer!"));
	XPtr<GatingSet>gs(_gsPtr);

	return gs;
}
/*
 * can't use module for exposing overloaded methods
 */


/*
 * constructing GatingSet from xml file
 */
RcppExport SEXP R_parseWorkspace(SEXP _fileName,SEXP _sampleIDs,SEXP _isParseGate,SEXP _sampNloc,SEXP _xmlParserOption,SEXP _dMode) {
BEGIN_RCPP
		string fileName=as<string>(_fileName);
		unsigned short dMode=as<unsigned short>(_dMode);
		StringVec sampleIDs=as<StringVec>(_sampleIDs);
		bool isParseGate=as<bool>(_isParseGate);
		unsigned short sampNloc=as<unsigned short>(_sampNloc);
		int xmlParserOption = as<int>(_xmlParserOption);

		GatingSet * gs=new GatingSet(fileName,isParseGate,sampNloc,xmlParserOption,dMode);
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



	GatingSet *	gs=getGsPtr(_gsPtr);

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


		GatingSet *	gs=getGsPtr(_gsPtr);

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
RcppExport SEXP R_saveGatingSet(SEXP _gsPtr,SEXP _fileName, SEXP _typeID) {
BEGIN_RCPP



		GatingSet *	gs=getGsPtr(_gsPtr);

		string fileName=as<string>(_fileName);
		unsigned short format =as<unsigned short>(_typeID);
		save_gs(*gs,fileName, format);


END_RCPP
}


RcppExport SEXP R_loadGatingSet(SEXP _fileName, SEXP _typeID) {
BEGIN_RCPP


		GatingSet * gs=new GatingSet();
		string fileName=as<string>(_fileName);
		unsigned short format =as<unsigned short>(_typeID);
		restore_gs(*gs,fileName, format);

		return XPtr<GatingSet>(gs);


END_RCPP
}


RcppExport SEXP R_CloneGatingSet(SEXP _gsPtr,SEXP _samples) {
BEGIN_RCPP


		GatingSet *	gs=getGsPtr(_gsPtr);
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
			GatingSet *	gs=getGsPtr((SEXP)gsList[i]);
			StringVec samples=as<StringVec>(sampleList[i]);
			newGS->add(*gs,samples);
		}


		return XPtr<GatingSet>(newGS);


END_RCPP
}

RcppExport SEXP R_setSample(SEXP _gsPtrs,SEXP _oldName, SEXP _newName) {
BEGIN_RCPP

		string oldName=as<string>(_oldName);
		string newName=as<string>(_newName);

		GatingSet *	gs=getGsPtr(_gsPtrs);

		gs->setSample(oldName,newName);

END_RCPP
}

