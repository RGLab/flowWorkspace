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
	vector<BOOL_GATE_OP> parseBooleanSpec(string specs,vector<string> gPaths);
public:
	macFlowJoWorkspace(xmlDoc *);
	compensation getCompensation(wsSampleNode sampleNode);

	trans_local getTransformation(wsRootNode,const compensation & comp,const isTransMap & transFlag,trans_global_vec*);
	trans_global_vec getGlobalTrans();
	string xPathSample(string sampleID);
	gate * getGate(wsPopNode &);
	polygonGate * getGate(wsPolyGateNode &);
	boolGate * getGate(wsBooleanGateNode &);
	polygonGate * getGate(wsEllipseGateNode &);
	rangegate * getGate(wsRangeGateNode &);
};





#endif /* MACFLOWJOWORKSPACE_HPP_ */
