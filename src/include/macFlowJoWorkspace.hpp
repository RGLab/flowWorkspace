/*
 * macFlowJoWorkspace.hpp
 *
 *  Created on: May 15, 2012
 *      Author: wjiang2
 */

#ifndef MACFLOWJOWORKSPACE_HPP_
#define MACFLOWJOWORKSPACE_HPP_
#include "flowJoWorkspace.hpp"

class macFlowJoWorkspace:public flowJoWorkspace{
public:
	macFlowJoWorkspace(xmlDoc *);
	compensation getCompensation(wsSampleNode sampleNode);
	trans_local getTransformation(wsRootNode,string cid,trans_vec*);
	trans_global_vec getGlobalTrans();
	string xPathSample(string sampleID);
	gate * getGate(wsPopNode &);
	polygonGate * getGate(wsPolyGateNode &);
//	polygonGate * getGate(wsRectGateNode &);
	polygonGate * getGate(wsEllipseGateNode &);
	rangegate * getGate(wsRangeGateNode &);
};





#endif /* MACFLOWJOWORKSPACE_HPP_ */
