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
#include "include/transformation.hpp"
using namespace std;

/*
 * only expose gating set pointer to R to avoid gc() by R
 */
RcppExport SEXP R_plotGh(SEXP _gsPtr,SEXP _sampleName,SEXP _output) {
BEGIN_RCPP


	XPtr<GatingSet>gs(_gsPtr);
	string sampleName=as<string>(_sampleName);
	GatingHierarchy *gh=gs->getGatingHierarchy(sampleName);

	string output=as<string>(_output);
 	gh->drawGraph(output);

END_RCPP
}


/*
 * return node names as a character vector
 */
RcppExport SEXP R_getNodes(SEXP _gsPtr,SEXP _sampleName,SEXP _tsort,SEXP _isPath){
BEGIN_RCPP

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

	XPtr<GatingSet>gs(_gsPtr);
	string sampleName=as<string>(_sampleName);
	GatingHierarchy *gh=gs->getGatingHierarchy(sampleName);
	int u=as<int>(_i);
	return wrap(gh->getParent(u));
END_RCPP
}

RcppExport SEXP R_getChildren(SEXP _gsPtr,SEXP _sampleName,SEXP _i){
BEGIN_RCPP


	XPtr<GatingSet>gs(_gsPtr);
	string sampleName=as<string>(_sampleName);
	GatingHierarchy* gh=gs->getGatingHierarchy(sampleName);
	int u=as<int>(_i);
	return wrap(gh->getChildren(u));
END_RCPP
}

RcppExport SEXP R_getPopStats(SEXP _gsPtr,SEXP _sampleName,SEXP _i){
BEGIN_RCPP


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



RcppExport SEXP R_getCompensation(SEXP _gsPtr,SEXP _sampleName){
BEGIN_RCPP


	XPtr<GatingSet>gs(_gsPtr);
	string sampleName=as<string>(_sampleName);

	GatingHierarchy* gh=gs->getGatingHierarchy(sampleName);
	compensation comp=gh->getCompensation();
	return(List::create(Named("cid",comp.cid)
						,Named("prefix",comp.prefix)
						,Named("suffix",comp.suffix)
						,Named("comment",comp.comment)
						,Named("parameters",comp.marker)
						,Named("spillOver",comp.spillOver))
			);

END_RCPP
}


RcppExport SEXP R_getTransformations(SEXP _gsPtr,SEXP _sampleName){
BEGIN_RCPP

	XPtr<GatingSet>gs(_gsPtr);
	string sampleName=as<string>(_sampleName);

	GatingHierarchy* gh=gs->getGatingHierarchy(sampleName);
	Trans_map trans=gh->trans;
	List res;

	for (Trans_map::iterator it=trans.begin();it!=trans.end();it++)
	{
		transformation * curTrans=it->second;

		switch(curTrans->type)
			{
				case LOGICLE:
					{
						throw(domain_error("logicle transformation is not supported yet in R_getTransformation!"));
		//
		////				Vector args=Vector::create();
		////
		////				return(List::create(Named("type",LOGICAL)
		////									,Named("arguments",args)
		////									)
		////							);
					}
				case CALTBL:
				{
					Spline_Coefs obj=curTrans->getCalTbl();

					res.push_back(List::create(Named("z",obj.coefs)
												,Named("method",obj.method)
												,Named("type",obj.type)
												)
									,curTrans->name.append(curTrans->channel)

									);
					break;
				}
				default:
					throw(domain_error("unknown transformation in R_getTransformations!"));
			}

	}
	return (res);



END_RCPP
}

RcppExport SEXP R_gating(SEXP _gsPtr,SEXP _sampleName){
BEGIN_RCPP


	XPtr<GatingSet>gs(_gsPtr);
	string sampleName=as<string>(_sampleName);

	GatingHierarchy* gh=gs->getGatingHierarchy(sampleName);
	gh->loadData();
	//gh->compensating();
	gh->transforming(true);
	gh->gating();
	gh->unloadData();

END_RCPP
}

RcppExport SEXP R_getGate(SEXP _gsPtr,SEXP _sampleName,SEXP _i){
BEGIN_RCPP


	XPtr<GatingSet>gs(_gsPtr);
	string sampleName=as<string>(_sampleName);
	int u=as<int>(_i);

	GatingHierarchy* gh=gs->getGatingHierarchy(sampleName);
	if(u<0)
		throw(domain_error("not valid vertexID!"));
	if(u==0)
		throw(domain_error("no gate associated with root node."));
	gate *g=gh->getNodeProperty(u)->getGate();
	switch(g->getType())
	{
		case POLYGONGATE:
			{
				vertices_vector vert=g->getVertices().toVector();

				 List ret=List::create(Named("parameters",g->getParam())
						 	 	 	 	 ,Named("x",vert.x),Named("y",vert.y)
						 	 	 	 	 ,Named("type",POLYGONGATE)
						 	 	 	 	 );
				return ret;
			}

		case RANGEGATE:
			{
				vertices_vector vert=g->getVertices().toVector();

				List ret=List::create(Named("parameters",g->getParam())
									 ,Named("range",vert.x)
									 ,Named("type",RANGEGATE)
									 );
				return ret;
			}
		default:
		{
//			cout<<g->getType()<<endl;
			throw(domain_error("unknown gate thrown by R_getGate!"));
		}

	}

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
