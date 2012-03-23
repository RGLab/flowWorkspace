/*
 * flowJoWorkspace.hpp
 *
 *  Created on: Mar 15, 2012
 *      Author: wjiang2
 */

#ifndef FLOWJOWORKSPACE_HPP_
#define FLOWJOWORKSPACE_HPP_

#include "workspace.hpp"

/*TODO: so far I will see the differenc between wind and max workspace in terms of xpath(like xpath of sample node)
 * if this is the case eventually we can try to use one template class (eliminate two derived classes )
 * with T structure that stores different versions of xpaths for win/mac,for example:
 *
 * struct winWorkspace{
 * xpath_sample=xxx
 * ....
 * }
 *
 * struct macWorkspace{
 * xpath_sample=xxx
 * ....
 * }
 *
 * this may potentially reduce the amount of code
 *
 */
struct xpath{
	string group;
	string sampleRef;
	string sample;
	string sampleNode;

};

class flowJoWorkspace:public workspace{

protected:
	 xpath nodePath;

public:
	 vector <xmlChar *> getSampleID(unsigned short);
     xmlNodePtr getSampleNode(xmlChar *sampleID);
     wsRootNode getRoot(xmlNodePtr sampleNode);
	 wsPopNode getSubPop(wsNode *);
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
