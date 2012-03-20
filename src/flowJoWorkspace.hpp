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
//#include <libxml2/libxml/parser.h>
//#include <iostream>
using namespace std;


class flowJoWorkspace{
protected:
	 xmlDoc * doc;
public:
//	flowJoWorkspace(xmlDoc *);
	~flowJoWorkspace();
	 void print_element_names(xmlNode *);
	 virtual void getCompensation()=0;
	 virtual void getTransformation()=0;
	 virtual void getPopulation()=0;
	 virtual xmlNodePtr getSampleNode(xmlChar*)=0;
	 vector <xmlChar *> getSampleID(unsigned short);
};

class winFlowJoWorkspace:public flowJoWorkspace{
public:
	winFlowJoWorkspace(xmlDoc *);
	  void getCompensation(){};
	  void getTransformation(){};
	  void getPopulation(){};
	  xmlNodePtr getSampleNode(xmlChar*);
};


class macFlowJoWorkspace:public flowJoWorkspace{
public:
	macFlowJoWorkspace(xmlDoc *);
	void getCompensation(){};
	void getTransformation(){};
	void getPopulation(){};
	xmlNodePtr getSampleNode(xmlChar*);
};



#endif /* FLOWJOWORKSPACE_HPP_ */
