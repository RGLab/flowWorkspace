/*
 * R_GatingSet.cpp
 *
 *these are R APIs
 *
 *  Created on: Mar 30, 2012
 *      Author: wjiang2
 */

#include "include/ws2gs.hpp"
#include <Rcpp.h>
using namespace Rcpp;
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
 * _sampleNames should be provided since the additional keys besides sample name may be necessary to uniquely tag each sample
 */
//[[Rcpp::export(name=".cpp_parseWorkspace")]]
XPtr<GatingSet> parseWorkspace(string fileName,StringVec sampleIDs
                            ,StringVec sampleNames,bool isParseGate
                            ,unsigned short sampNloc,int xmlParserOption
                            , unsigned short wsType) 
{
		workspace * ws = openWorkspace(fileName, sampNloc,xmlParserOption, wsType);
		GatingSet * gs = ws2gs(ws, sampleIDs,isParseGate,sampleNames);
		delete ws;
		return XPtr<GatingSet>(gs);


}


//[[Rcpp::export(name=".cpp_getSamples")]]
StringVec getSamples(XPtr<GatingSet> gsPtr) {

	return gsPtr->getSamples();

}

/*
 * constructing GatingSet from existing gating hierarchy and new data
 */
//[[Rcpp::export(name=".cpp_NewGatingSet")]]
XPtr<GatingSet> NewGatingSet(XPtr<GatingSet> gsPtr
               ,string sampleName
               ,StringVec newSampleNames) 
  {

		GatingHierarchy* gh=gsPtr->getGatingHierarchy(sampleName);

		/*
		 * used gh as the template to clone multiple ghs in the new gs
		 */
		GatingSet * newGS=new GatingSet(gh,newSampleNames);

		/*
		 * using default finalizer to delete gs,which is triggered by gc() when
		 * xptr is out of scope
		 */

		return XPtr<GatingSet>(newGS);

}

/*
 * constructing GatingSet with only root node for each sample
 */
//[[Rcpp::export(name=".cpp_NewGatingSet_rootOnly")]]
XPtr<GatingSet> NewGatingSet_rootOnly(StringVec sampleNames) {

		GatingSet * newGS=new GatingSet(sampleNames);

		return XPtr<GatingSet>(newGS);

}

/*
 * save/load GatingSet
 */
//[[Rcpp::export(name=".cpp_saveGatingSet")]]
void saveGatingSet(XPtr<GatingSet> gs, string fileName) {
			gs->serialize_pb(fileName);
}


//[[Rcpp::export(name=".cpp_loadGatingSet")]]
XPtr<GatingSet> loadGatingSet(string fileName) {
		GatingSet * gs=new GatingSet(fileName);
		return XPtr<GatingSet>(gs);

}


//[[Rcpp::export(name=".cpp_CloneGatingSet")]]
XPtr<GatingSet> CloneGatingSet(XPtr<GatingSet> gs,StringVec samples) {

		GatingSet * gs_new=gs->clone(samples);

		return XPtr<GatingSet>(gs_new);

}

//[[Rcpp::export(name=".cpp_combineGatingSet")]]
XPtr<GatingSet> combineGatingSet(Rcpp::List gsList,Rcpp::List sampleList) {

		GatingSet * newGS=new GatingSet();

		for(int i=0;i<gsList.size();i++)
		{
			GatingSet *	gs=getGsPtr((SEXP)gsList[i]);
			StringVec samples=as<StringVec>(sampleList[i]);
			newGS->add(*gs,samples);
		}


		return XPtr<GatingSet>(newGS);

}

/**
 * change sample name
 */
//[[Rcpp::export(name=".cpp_setSample")]]
void setSample(XPtr<GatingSet> gs,string oldName, string newName) {
	
		gs->setSample(oldName,newName);

}

//[[Rcpp::export(name=".cpp_getLogLevel")]]
unsigned short getLogLevel() {

		return(g_loglevel);

}

//[[Rcpp::export(name=".cpp_setLogLevel")]]
void setLogLevel(unsigned short loglevel) {

		g_loglevel = loglevel;

}

//[[Rcpp::export(name=".cpp_togleErrorFlag")]]
void toggleErrorFlag(){
	my_throw_on_error = !my_throw_on_error;
}
