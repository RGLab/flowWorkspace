/*
 * GatingSet.cpp
 *
 *  Created on: Mar 19, 2012
 *      Author: wjiang2
 */


#include "GatingSet.hpp"
#include <string>
#include <libxml/tree.h>
#include <libxml/parser.h>
#include <iostream>
using namespace std;

//read xml file and create the appropriate flowJoWorkspace object
void GatingSet::openWorkspace(const char * sFileName)
{

		LIBXML_TEST_VERSION

		/*parse the file and get the DOM */
		xmlDocPtr doc = xmlReadFile(sFileName, NULL, 0);
		if (doc == NULL ) {
				fprintf(stderr,"Document not parsed successfully. \n");
				return;
			}
		//validity check
		xmlNodePtr cur = xmlDocGetRootElement(doc);
		if (cur == NULL) {
				fprintf(stderr,"empty document\n");
//				xmlFreeDoc(doc);
				return;
			}
		 if (xmlStrcmp(cur->name, (const xmlChar *) "Workspace")) {
				fprintf(stderr,"document of the wrong type, root node != Workspace");
//				xmlFreeDoc(doc);
				return;
			}

		//get version info
		 xmlChar * wsVersion=xmlGetProp(cur,(const xmlChar*)"version");

		 if (xmlStrcmp(wsVersion,(const xmlChar *)"1.6"))
			 this->ws=new winFlowJoWorkspace(doc);
		 else if (xmlStrcmp(wsVersion,(const xmlChar *)"2.0"))
			 this->ws=new macFlowJoWorkspace(doc);
		 else
			 throw(1);

}

void GatingSet::parseWorkspace(unsigned short groupID)
{
	flowJoWorkspace * ws=this->ws;
	vector<xmlChar *> sampleID=ws->getSampleID(groupID);
	vector<xmlChar *>::iterator it;
	for(it=sampleID.begin();it!=sampleID.end();it++)
	{
		ws->getSampleNode(*it);
	}

}
