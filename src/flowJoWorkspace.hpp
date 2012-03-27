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
	 vector <xmlChar *> getSampleID(unsigned short);
	 wsSampleNode getSampleNode(xmlChar *sampleID);
     wsRootNode getRoot(wsSampleNode sampleNode);
     wsNodeSet getSubPop(wsNode const * node);

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
