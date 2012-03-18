/*
 * flowJoWorkspace.hpp
 *
 *  Created on: Mar 15, 2012
 *      Author: wjiang2
 */

#ifndef FLOWJOWORKSPACE_HPP_
#define FLOWJOWORKSPACE_HPP_

#include <string>
#include <libxml/tree.h>
//#include <libxml2/libxml/parser.h>
//#include <iostream>
//using namespace std;


class flowJoWorkspace{

	 xmlDoc *doc;
public:
	flowJoWorkspace(const char *);
	~flowJoWorkspace();
	 void openWorkspace(const char * );
	void print_element_names(xmlNode *);
};








#endif /* FLOWJOWORKSPACE_HPP_ */
