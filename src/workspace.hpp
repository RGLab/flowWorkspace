/*
 * workspace.hpp
 *
 *  Created on: Mar 22, 2012
 *      Author: wjiang2
 */

#ifndef WORKSPACE_HPP_
#define WORKSPACE_HPP_
#include <vector>
#include <string>
#include <libxml/xpath.h>
#include "wsNode.hpp"
using namespace std;



class workspace{
protected:
	 xmlDoc * doc;
public:
	~workspace();
	 void print_element_names(xmlNode *);
	 virtual void getCompensation()=0;
	 virtual void getTransformation()=0;
	 xmlXPathObjectPtr xpathInNode(string xpath,xmlNodePtr curNode);
	 virtual vector <xmlChar *> getSampleID(unsigned short)=0;
	 virtual xmlNodePtr getSampleNode(xmlChar *sampleID)=0;
	 virtual wsRootNode getRoot(xmlNodePtr sampleNode)=0;
	 virtual wsPopNode getSubPop(wsNode*)=0;
};


#endif /* WORKSPACE_HPP_ */

