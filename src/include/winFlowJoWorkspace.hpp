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
	trans_vec getGlobalTrans();
	trans_map getTransformation(wsSampleNode,string cid,trans_vec *);
	string xPathSample(string sampleID);
	  gate * getGate(wsPopNode &);
	  polygonGate * getGate(wsPolyGateNode &);
	  polygonGate * getGate(wsRectGateNode &);
	  polygonGate * getGate(wsEllipseGateNode &);
	  rangegate * getGate(wsRangeGateNode &);
};



#endif /* WINFLOWJOWORKSPACE_HPP_ */
