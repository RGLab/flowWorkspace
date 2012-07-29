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
	trans_local getTransformation(wsRootNode,const compensation & comp,PARAM_VEC & transFlag,trans_global_vec *);
	string xPathSample(string sampleID);
	  gate * getGate(wsPopNode &);
	  polygonGate * getGate(wsPolyGateNode &);
	  polygonGate * getGate(wsRectGateNode &);
	  polygonGate * getGate(wsEllipseGateNode &);
	  rangegate * getGate(wsRangeGateNode &);
};



#endif /* WINFLOWJOWORKSPACE_HPP_ */
