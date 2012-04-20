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
#include <stdexcept>
#include "include/gate.hpp"
using namespace std;

/*
 * only expose gating set pointer to R to avoid gc() by R
 */
RcppExport SEXP R_plotGh(SEXP _gsPtr,SEXP _sampleName,SEXP _output) {
BEGIN_RCPP


//	XPtr<GatingHierarchy>gh(_ghPtr);
	XPtr<GatingSet>gs(_gsPtr);
	string sampleName=as<string>(_sampleName);
	GatingHierarchy *gh=gs->getGatingHierarchy(sampleName);

	string output=as<string>(_output);
 	gh->drawGraph(output);
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
	GatingHierarchy* gh=gs->getGatingHierarchy(sampleName);
	bool tsort=as<bool>(_tsort);
	bool isPath=as<bool>(_isPath);

	return wrap(gh->getPopNames(tsort,isPath));
END_RCPP
}

RcppExport SEXP R_getParent(SEXP _gsPtr,SEXP _sampleName,SEXP _i){
BEGIN_RCPP

//	XPtr<GatingHierarchy>gh(_ghPtr);
	XPtr<GatingSet>gs(_gsPtr);
	string sampleName=as<string>(_sampleName);
	GatingHierarchy *gh=gs->getGatingHierarchy(sampleName);
	int u=as<int>(_i);
	return wrap(gh->getParent(u));
END_RCPP
}

RcppExport SEXP R_getChildren(SEXP _gsPtr,SEXP _sampleName,SEXP _i){
BEGIN_RCPP

//	XPtr<GatingHierarchy>gh(_ghPtr);
	XPtr<GatingSet>gs(_gsPtr);
	string sampleName=as<string>(_sampleName);
	GatingHierarchy* gh=gs->getGatingHierarchy(sampleName);
	int u=as<int>(_i);
	return wrap(gh->getChildren(u));
END_RCPP
}

RcppExport SEXP R_getPopStats(SEXP _gsPtr,SEXP _sampleName,SEXP _i){
BEGIN_RCPP

//	XPtr<GatingHierarchy>gh(_ghPtr);
	XPtr<GatingSet>gs(_gsPtr);
	string sampleName=as<string>(_sampleName);
	GatingHierarchy *gh=gs->getGatingHierarchy(sampleName);
	int u=as<int>(_i);
	nodeProperties *node=gh->getNodeProperty(u);

	return List::create(Named("FlowCore",node->getStats(true))
						,Named("FlowJo",node->getStats(false))
						);

END_RCPP
}

RcppExport SEXP R_gating(SEXP _gsPtr,SEXP _sampleName){
BEGIN_RCPP

//	XPtr<GatingHierarchy>gh(_ghPtr);
	XPtr<GatingSet>gs(_gsPtr);
	string sampleName=as<string>(_sampleName);

	GatingHierarchy* gh=gs->getGatingHierarchy(sampleName);
	gh->gating();

END_RCPP
}

RcppExport SEXP R_getGate(SEXP _gsPtr,SEXP _sampleName,SEXP _i){
BEGIN_RCPP

//	XPtr<GatingHierarchy>gh(_ghPtr);
	XPtr<GatingSet>gs(_gsPtr);
	string sampleName=as<string>(_sampleName);
	int u=as<int>(_i);

	GatingHierarchy* gh=gs->getGatingHierarchy(sampleName);
	if(u<0)
		throw(domain_error("not valid vertexID!"));
	if(u==0)
		throw(domain_error("no gate associated with root node."));
	gate *g=gh->getNodeProperty(u)->getGate();
//	gate *g1;

	switch(g->getType()>0)
	{
		case POLYGONGATE:
			{
				vector<coordinate> vert=g->getVertices();
				vector<double> x,y;
				for(vector<coordinate>::iterator it=vert.begin();it!=vert.end();it++)
				{
					x.push_back(it->x);
					y.push_back(it->y);
				}

				 List ret=List::create(Named("parameters",g->getParam())
						 	 	 	 	 ,Named("x",x),Named("y",y)
						 	 	 	 	 ,Named("type",POLYGONGATE)
						 	 	 	 	 );
				return ret;
			}
//		case RECTGATE:
//

		case ELLIPSEGATE:
			{


				 List ret=List::create(Named("parameters",g->getParam())
									 ,Named("radius",NumericVector::create(Named("a")=g->getMajorAxis()
																			,Named("b")=g->getMinorAxis())
											)

						 	 	 	 ,Named("centriod",NumericVector::create(Named("x")=g->getCentroid().x
																			,Named("y")=g->getCentroid().y
																			)
											)

									 ,Named("type",ELLIPSEGATE)
									 );
				return ret;
			}
		case RANGEGATE:
			{
				vector<coordinate> vert=g->getVertices();
				vector<double> x;
				for(vector<coordinate>::iterator it=vert.begin();it!=vert.end();it++)
					x.push_back(it->x);

				List ret=List::create(Named("parameters",g->getParam())
									 ,Named("range",x)
									 ,Named("type",POLYGONGATE)
									 );
				return ret;
			}
		default:
			throw(domain_error("unknown gate!"));

	}
//		g1=g->toPolygon();
//	else
//		g1=g;




//	if(g->getType()>0)
//		delete g1;

END_RCPP
}

RcppExport SEXP R_getIndices(SEXP _gsPtr,SEXP _sampleName,SEXP _i){
BEGIN_RCPP

	XPtr<GatingSet>gs(_gsPtr);
	string sampleName=as<string>(_sampleName);
	int u=as<int>(_i);
	if(u<0)throw(domain_error("not valid vertexID!"));

	GatingHierarchy* gh=gs->getGatingHierarchy(sampleName);
	return wrap(gh->getNodeProperty(u)->indices);

END_RCPP
}
