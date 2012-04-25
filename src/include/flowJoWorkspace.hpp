/*
 * flowJoWorkspace.hpp
 *
 *  Created on: Mar 15, 2012
 *      Author: wjiang2
 */

#ifndef FLOWJOWORKSPACE_HPP_
#define FLOWJOWORKSPACE_HPP_

#include "workspace.hpp"




class flowJoWorkspace:public workspace{

public:
//	~flowJoWorkspace();
	 vector <string> getSampleID(unsigned short);

     wsRootNode getRoot(wsSampleNode sampleNode);
     wsPopNodeSet getSubPop(wsNode * node);
     nodeProperties * to_popNode(wsRootNode &);
     nodeProperties * to_popNode(wsPopNode &,bool isParseGate);
     string getSampleName(wsSampleNode &);
//     virtual string xPathSample(string sampleID)=0;

};

class winFlowJoWorkspace:public flowJoWorkspace{
public:
	winFlowJoWorkspace(xmlDoc *);
	compensation getCompensation(wsSampleNode sampleNode);
	  void getTransformation(){};
	  string xPathSample(string sampleID);
	  gate * getGate(wsPopNode &);
	  polygonGate * getGate(wsPolyGateNode &);
	  polygonGate * getGate(wsRectGateNode &);
	  polygonGate * getGate(wsEllipseGateNode &);
	  rangegate * getGate(wsRangeGateNode &);
};


class macFlowJoWorkspace:public flowJoWorkspace{
public:
	macFlowJoWorkspace(xmlDoc *);
	compensation getCompensation(wsSampleNode sampleNode);
	void getTransformation(){};
	string xPathSample(string sampleID);
	gate * getGate(wsPopNode &);
	polygonGate * getGate(wsPolyGateNode &);
//	polygonGate * getGate(wsRectGateNode &);
	polygonGate * getGate(wsEllipseGateNode &);
	rangegate * getGate(wsRangeGateNode &);
};



#endif /* FLOWJOWORKSPACE_HPP_ */
