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
#include "include/R_GatingSet.hpp"
/*
 * only expose gating set pointer to R to avoid gc() by R
 */
RcppExport SEXP R_plotGh(SEXP _gsPtr,SEXP _sampleName,SEXP _output) {
BEGIN_RCPP


//	XPtr<GatingHierarchy>gh(_ghPtr);
	XPtr<GatingSet>gs(_gsPtr);
	string sampleName=as<string>(_sampleName);
	GatingHierarchy gh=gs->getGatingHierarchy(sampleName);

	string output=as<string>(_output);
 	gh.drawGraph(output);
//    return wrap(res);
END_RCPP
}

//RcppExport SEXP R_getSample(SEXP _gsPtr,SEXP _sampleName){
//BEGIN_RCPP
//
////	XPtr<GatingHierarchy>gh(_ghPtr);
//	XPtr<GatingSet>gs(_gsPtr);
//	string sampleName=as<string>(_sampleName);
//
//	 return wrap(gs->getGatingHierarchy(sampleName).getSample());
//END_RCPP
//}

/*
 * return node names as a character vector
 */
RcppExport SEXP R_getNodes(SEXP _gsPtr,SEXP _sampleName,SEXP _tsort,SEXP _isPath){
BEGIN_RCPP

//	XPtr<GatingHierarchy>gh(_ghPtr);
	XPtr<GatingSet>gs(_gsPtr);
	string sampleName=as<string>(_sampleName);
	GatingHierarchy gh=gs->getGatingHierarchy(sampleName);
	bool tsort=as<bool>(_tsort);
	bool isPath=as<bool>(_isPath);

	return wrap(gh.getPopNames(tsort,isPath));
END_RCPP
}

RcppExport SEXP R_getParent(SEXP _gsPtr,SEXP _sampleName,SEXP _i){
BEGIN_RCPP

//	XPtr<GatingHierarchy>gh(_ghPtr);
	XPtr<GatingSet>gs(_gsPtr);
	string sampleName=as<string>(_sampleName);
	GatingHierarchy gh=gs->getGatingHierarchy(sampleName);
	int u=as<int>(_i);
	return wrap(gh.getParent(u));
END_RCPP
}

RcppExport SEXP R_getChildren(SEXP _gsPtr,SEXP _sampleName,SEXP _i){
BEGIN_RCPP

//	XPtr<GatingHierarchy>gh(_ghPtr);
	XPtr<GatingSet>gs(_gsPtr);
	string sampleName=as<string>(_sampleName);
	GatingHierarchy gh=gs->getGatingHierarchy(sampleName);
	int u=as<int>(_i);
	return wrap(gh.getChildren(u));
END_RCPP
}

RcppExport SEXP R_getPopStats(SEXP _gsPtr,SEXP _sampleName,SEXP _i){
BEGIN_RCPP

//	XPtr<GatingHierarchy>gh(_ghPtr);
	XPtr<GatingSet>gs(_gsPtr);
	string sampleName=as<string>(_sampleName);
	GatingHierarchy gh=gs->getGatingHierarchy(sampleName);
	int u=as<int>(_i);
	populationNode node=gh.vertexIDToNode(u);

	return List::create(Named("FlowCore",node.getStats(true))
						,Named("FlowJo",node.getStats(false))
						);

END_RCPP
}




