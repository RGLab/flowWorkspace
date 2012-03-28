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
	 wsSampleNode getSampleNode(string sampleID);
     wsRootNode getRoot(wsSampleNode sampleNode);
     wsPopNodeSet getSubPop(wsNode * node);
     populationNode to_popNode(wsRootNode const *node);
     populationNode to_popNode(wsPopNode const * node);
     string getName(wsNode *);

};

class winFlowJoWorkspace:public flowJoWorkspace{
public:
	winFlowJoWorkspace(xmlDoc *);
	  void getCompensation(){};
	  void getTransformation(){};
//	  void getPopulation(){};
};


class macFlowJoWorkspace:public flowJoWorkspace{
public:
	macFlowJoWorkspace(xmlDoc *);
	void getCompensation(){};
	void getTransformation(){};
//	void getPopulation(){};
};



#endif /* FLOWJOWORKSPACE_HPP_ */
