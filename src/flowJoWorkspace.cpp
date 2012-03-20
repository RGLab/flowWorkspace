/*
 * flowJoWorkspace.cpp
 *
 *  Created on: Mar 15, 2012
 *      Author: wjiang2
 */
#include "flowJoWorkspace.hpp"
#include <string>
#include <libxml/tree.h>
#include <libxml/parser.h>
#include <libxml/xpath.h>
#include <iostream>
using namespace std;

//flowJoWorkspace::flowJoWorkspace(xmlDoc * doc){
//	this->doc=doc;
//
//}
macFlowJoWorkspace::macFlowJoWorkspace(xmlDoc * doc){
	cout<<"mac version of flowJo workspace recognized."<<endl;
	this->doc=doc;

}
winFlowJoWorkspace::winFlowJoWorkspace(xmlDoc * doc){
	cout<<"windows version of flowJo workspace recognized."<<endl;
	this->doc=doc;

}
flowJoWorkspace::~flowJoWorkspace(){

	 /*free the document */
			xmlFreeDoc(this->doc);

			/*
			 *Free the global variables that may
			 *have been allocated by the parser.
			 */
			xmlCleanupParser();
			cout<<"xml freed!"<<endl;
}


vector<xmlChar *> flowJoWorkspace::getSampleID(unsigned short groupID)
{

		xmlXPathContextPtr context;
		xmlXPathObjectPtr result;

		context = xmlXPathNewContext(doc);
		result = xmlXPathEval((xmlChar *)"/Workspace/Groups/GroupNode", context);
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
			sampleID.push_back(xmlGetProp(curNode,(xmlChar *)"sampleID"));
		}
//			;

		xmlXPathFreeObject (result);
		xmlXPathFreeObject (sids);
		return(sampleID);
}


xmlNodePtr winFlowJoWorkspace::getSampleNode(xmlChar * sampleID){
	string xpath="/Workspace/SampleList/Sample/DataSet[@sampleID='";
	xmlXPathContextPtr context=xmlXPathNewContext(doc);

	xmlXPathEval((xmlChar *)xpath,context);
	//				xpathApply(x,paste("/Workspace/SampleList/Sample/DataSet[@sampleID='",i,"']",sep=""))[[1]]
	return (NULL);
}

xmlNodePtr macFlowJoWorkspace::getSampleNode(xmlChar *sampleID){
	//	if(wsversion=="2.0"){
	//			l<-sapply(sg[sg$groupName==groups[result],]$sampleID,function(i){
	//				xpathApply(x,paste("/Workspace/SampleList/Sample[@sampleID='",i,"']",sep=""))[[1]]
	//				})

	return(NULL);
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
