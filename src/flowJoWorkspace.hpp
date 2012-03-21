/*
 * flowJoWorkspace.hpp
 *
 *  Created on: Mar 15, 2012
 *      Author: wjiang2
 */

#ifndef FLOWJOWORKSPACE_HPP_
#define FLOWJOWORKSPACE_HPP_
#include <vector>
#include <string>
#include <libxml/tree.h>
#include <libxml/xpath.h>
//#include <libxml2/libxml/parser.h>
//#include <iostream>
using namespace std;

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

class flowJoWorkspace{
protected:
	 xmlDoc * doc;
	 string xpath_sample;
public:
	~flowJoWorkspace();
	 void print_element_names(xmlNode *);
	 virtual void getCompensation()=0;
	 virtual void getTransformation()=0;
	 virtual void getPopulation()=0;
	 xmlXPathObjectPtr xpathInNode(string xpath,xmlNodePtr curNode);
	 xmlNodePtr getSample(xmlChar* sampleID);
	 xmlNodePtr getSampleNode(xmlNodePtr sampleNode);

	 vector <xmlChar *> getSampleID(unsigned short);
};

class winFlowJoWorkspace:public flowJoWorkspace{
public:
	winFlowJoWorkspace(xmlDoc *);
	  void getCompensation(){};
	  void getTransformation(){};
	  void getPopulation(){};
};


class macFlowJoWorkspace:public flowJoWorkspace{
public:
	macFlowJoWorkspace(xmlDoc *);
	void getCompensation(){};
	void getTransformation(){};
	void getPopulation(){};
};



#endif /* FLOWJOWORKSPACE_HPP_ */
