/*
 * flowJoWorkspace.cpp
 *
 *  Created on: Mar 15, 2012
 *      Author: wjiang2
 */
#include "flowJoWorkspace.hpp"
#include <string>
//#include <libxml/tree.h>
#include <libxml/parser.h>

#include <iostream>
using namespace std;

//flowJoWorkspace::flowJoWorkspace(xmlDoc * doc){
//	this->doc=doc;
//
//}
macFlowJoWorkspace::macFlowJoWorkspace(xmlDoc * doc){
	cout<<"mac version of flowJo workspace recognized."<<endl;

	xpath_sample="/Workspace/SampleList/Sample";

	this->doc=doc;

}
winFlowJoWorkspace::winFlowJoWorkspace(xmlDoc * doc){
	cout<<"windows version of flowJo workspace recognized."<<endl;
	xpath_sample="/Workspace/SampleList/Sample/DataSet";

	this->doc=doc;

}
flowJoWorkspace::~flowJoWorkspace(){

	 /*free the document */
			xmlFreeDoc(doc);

			/*
			 *Free the global variables that may
			 *have been allocated by the parser.
			 */
			xmlCleanupParser();
			cout<<"xml freed!"<<endl;
}


vector<xmlChar *> flowJoWorkspace::getSampleID(unsigned short groupID)
{

		xmlXPathContextPtr context = xmlXPathNewContext(doc);
		xmlXPathObjectPtr result = xmlXPathEval((xmlChar *)"/Workspace/Groups/GroupNode", context);
		if(xmlXPathNodeSetIsEmpty(result->nodesetval)){
			xmlXPathFreeObject(result);
	                cout<<"No Groups"<<endl;;
	                throw(2);
		}


		xmlNodePtr cur=result->nodesetval->nodeTab[groupID];
		context->node=cur;
		xmlXPathObjectPtr sids=xmlXPathEval((xmlChar *)".//SampleRef",context);
		vector<xmlChar *> sampleID;
		xmlNodeSetPtr nodeSet=sids->nodesetval;
		int size=nodeSet->nodeNr;

		for(unsigned i=0;i<size;i++)
		{
			xmlNodePtr curNode=nodeSet->nodeTab[i];
			xmlChar * curSampleID= xmlGetProp(curNode,(xmlChar *)"sampleID");

			sampleID.push_back(curSampleID);
		}
//			;

		xmlXPathFreeObject (result);
		xmlXPathFreeContext(context);
		xmlXPathFreeObject (sids);
		return(sampleID);
}



xmlXPathObjectPtr flowJoWorkspace::getSampleNode(xmlChar *sampleID){
		string xpath=xpath_sample;
		xpath.append("[@sampleID='");
		xpath.append((const char *)sampleID);
		xpath.append("']");
		xmlXPathContextPtr context=xmlXPathNewContext(doc);

		xmlXPathObjectPtr res=xmlXPathEval((xmlChar *)xpath.c_str(),context);

		xmlXPathFreeContext(context);

		return res;
}
void flowJoWorkspace::print_element_names(xmlNode *a_node)
{



    xmlNode *cur_node = NULL;

    for (cur_node = a_node; cur_node; cur_node = cur_node->next) {
        if (cur_node->type == XML_ELEMENT_NODE) {
            printf("node type: Element, name: %s\n", cur_node->name);
        }

        print_element_names(cur_node->children);
    }
}
