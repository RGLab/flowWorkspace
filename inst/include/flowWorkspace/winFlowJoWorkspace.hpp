/*
 * winFlowJoWorkspace.hpp
 *
 *  Created on: May 15, 2012
 *      Author: wjiang2
 */

#ifndef WINFLOWJOWORKSPACE_HPP_
#define WINFLOWJOWORKSPACE_HPP_
#include "flowJoWorkspace.hpp"

class winFlowJoWorkspace:public flowJoWorkspace{
public:
	winFlowJoWorkspace(xmlDoc *);
	compensation getCompensation(wsSampleNode sampleNode);
//	isTransMap getTransFlag(wsSampleNode sampleNode);
	trans_global_vec getGlobalTrans();
	trans_local getTransformation(wsRootNode,const compensation & comp,PARAM_VEC & transFlag,trans_global_vec *,biexpTrans * _globalBiExpTrans,linTrans * _globalLinTrans, bool prefixed);
	string xPathSample(string sampleID);
	  gate * getGate(wsPopNode &);
	  polygonGate * getGate(wsPolyGateNode &, string vertexPath = "*[local-name()='vertex']");
	  gate * getGate(wsRectGateNode &);
	  ellipsoidGate * getGate(wsEllipseGateNode &);
	  rangeGate * getGate(wsRangeGateNode &);
	  CurlyGuadGate* getGate(wsCurlyQuadGateNode &);
};

class xFlowJoWorkspace:public winFlowJoWorkspace{
public:
	xFlowJoWorkspace(xmlDoc * _doc);
	trans_global_vec getGlobalTrans(){trans_global_vec res;
										return res;
										};
	trans_local getTransformation(wsRootNode,const compensation & comp,PARAM_VEC & transFlag,trans_global_vec *,biexpTrans * _globalBiExpTrans,linTrans * _globalLinTrans, bool prefixed);
};

#endif /* WINFLOWJOWORKSPACE_HPP_ */
