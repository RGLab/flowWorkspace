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

/*constructors of flowJoWorkspace for mac and win derived classes
 *
 */
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

/*get a vector of sampleID by the given groupID
 * keep the returned results in char * format in case the non-numeric sampleID is stored
 * make sure to free the memory of xmlChar * outside of the call
 */
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


//make sure to free the memory of xmlXPathObject outside of the call
xmlNodePtr flowJoWorkspace::getSample(xmlChar *sampleID){
		string xpath=xpath_sample;
		xpath.append("[@sampleID='");
		xpath.append((const char *)sampleID);
		xpath.append("']");
		xmlXPathContextPtr context=xmlXPathNewContext(doc);

		xmlXPathObjectPtr res=xmlXPathEval((xmlChar *)xpath.c_str(),context);
		xmlNodePtr sample=res->nodesetval->nodeTab[0];

		xmlXPathFreeContext(context);
		xmlXPathFreeObject(res);
		return sample;
}

//xquery the "SampleNode" within "sample" context
xmlNodePtr flowJoWorkspace::getSampleNode(xmlNodePtr sample)
{
	string xpath="./SampleNode";


	xmlXPathObjectPtr res=xpathInNode(xpath,sample);
	xmlNodePtr sampleNode=res->nodesetval->nodeTab[0];
	xmlXPathFreeObject(res);
	return sampleNode;
}

/*Oftentimes we need to do the xquery based on the context of the current node instead of doc
* it is strange that I haven't found this commonly used API in libxml2
* so have to write my own here
*/
xmlXPathObjectPtr flowJoWorkspace::xpathInNode(string xpath,xmlNodePtr curNode)
{
	xmlXPathContextPtr ctxt=xmlXPathNewContext(this->doc);
	ctxt->node=curNode;
	xmlXPathObjectPtr res=xmlXPathEval((xmlChar *)xpath.c_str(),ctxt);
	xmlXPathFreeContext(ctxt);
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
