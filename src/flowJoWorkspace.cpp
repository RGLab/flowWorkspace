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

	nodePath.group="/Workspace/Groups/GroupNode";
	nodePath.sampleRef=".//SampleRef";
	nodePath.sample="/Workspace/SampleList/Sample";
	nodePath.sampleNode="./SampleNode";
	nodePath.popNode="./Population";
	this->doc=doc;

}
winFlowJoWorkspace::winFlowJoWorkspace(xmlDoc * doc){
	cout<<"windows version of flowJo workspace recognized."<<endl;
//	xpath_sample="/Workspace/SampleList/Sample/DataSet";
	nodePath.group="/Workspace/Groups/GroupNode";// abs path
	nodePath.sampleRef=".//SampleRef";//relative GroupNode
	nodePath.sample="/Workspace/SampleList/Sample";//abs path
	nodePath.sampleNode="./SampleNode";//relative to sample
	nodePath.popNode="./*/Population";//relative to sampleNode
	this->doc=doc;

}


/*get a vector of sampleID by the given groupID
 * keep the returned results in char * format in case the non-numeric sampleID is stored
 * make sure to free the memory of xmlChar * outside of the call
 */
vector<string> flowJoWorkspace::getSampleID(unsigned short groupID)
{

		xmlXPathContextPtr context = xmlXPathNewContext(doc);
		xmlXPathObjectPtr result = xmlXPathEval((xmlChar *)nodePath.group.c_str(), context);
		if(xmlXPathNodeSetIsEmpty(result->nodesetval)){
			xmlXPathFreeObject(result);
			xmlXPathFreeContext(context);
	                cout<<"No Groups"<<endl;;
	                throw(2);
		}


		xmlNodePtr cur=result->nodesetval->nodeTab[groupID];
		context->node=cur;
		xmlXPathObjectPtr sids=xmlXPathEval((xmlChar *)nodePath.sampleRef.c_str(),context);
		vector<string> sampleID;
		xmlNodeSetPtr nodeSet=sids->nodesetval;
		int size=nodeSet->nodeNr;

		for(unsigned i=0;i<size;i++)
		{
			xmlNodePtr curNode=nodeSet->nodeTab[i];
			xmlChar * curSampleID= xmlGetProp(curNode,(xmlChar *)"sampleID");
			//to avoid memory leaking,store a copy of returned characters in string vector so that the dynamically allocated memory
			//can be freed right away in stead of later on.
			string sSampleID=(const char *)curSampleID;
			sampleID.push_back(sSampleID.c_str());
			xmlFree(curSampleID);
		}
//			;

		xmlXPathFreeObject (result);
		xmlXPathFreeContext(context);
		xmlXPathFreeObject (sids);
		return(sampleID);
}

string macFlowJoWorkspace::xPathSample(string sampleID){
			string xpath=nodePath.sample;
			xpath.append("[@sampleID='");
			xpath.append(sampleID);
			xpath.append("']");
			return xpath;

}

string winFlowJoWorkspace::xPathSample(string sampleID){
			string xpath=nodePath.sample;
			xpath.append("/DataSet[@sampleID='");
			xpath.append(sampleID);
			xpath.append("']/..");
			return xpath;

}


////make sure to free the memory of xmlXPathObject outside of the call
//wsSampleNode macFlowJoWorkspace::getSample(string sampleID){
//		string xpath=nodePath.sample;
//		xpath.append("[@sampleID='");
//		xpath.append(sampleID);
//		xpath.append("']");
//
//		wsNode docRoot(xmlDocGetRootElement(doc));
//
//		xmlXPathObjectPtr res=docRoot.xpathInNode(xpath);
//		if(res->nodesetval->nodeNr>1)
//		{
//			cout<<sampleID<<" is not unique within this group!"<<endl;
//			xmlXPathFreeObject(res);
//			throw(3);
//		}
//
//		wsSampleNode sample(res->nodesetval->nodeTab[0]);
//		xmlXPathFreeObject(res);
//		return sample;
//}
//wsSampleNode winFlowJoWorkspace::getSample(string sampleID){
//
//
//		wsNode docRoot(xmlDocGetRootElement(doc));
//
//		xmlXPathObjectPtr res=docRoot.xpathInNode(xpath);
//		if(res->nodesetval->nodeNr>1)
//		{
//			cout<<sampleID<<" is not unique within this group!"<<endl;
//			xmlXPathFreeObject(res);
//			throw(3);
//		}
//
//		wsSampleNode sample(res->nodesetval->nodeTab[0]);
//		xmlXPathFreeObject(res);
//		return sample;
//}
//need to explicitly release the memory after this call
string flowJoWorkspace::getSampleName(wsSampleNode & node){

	xmlXPathObjectPtr res=node.xpathInNode("SampleNode");//get sampleNode
	wsNode sampleNode(res->nodesetval->nodeTab[0]);
	xmlXPathFreeObject(res);

	return sampleNode.getProperty("name");//get property name from sampleNode

}

//xquery the "SampleNode" within "sample" context
wsRootNode flowJoWorkspace::getRoot(wsSampleNode sample)
{


	xmlXPathObjectPtr res=sample.xpathInNode(nodePath.sampleNode);
	wsRootNode node(res->nodesetval->nodeTab[0]);
	xmlXPathFreeObject(res);
	return node;
}


wsPopNodeSet flowJoWorkspace::getSubPop(wsNode * node)
{

	xmlXPathObjectPtr res=node->xpathInNode(nodePath.popNode);
	int nChildren=res->nodesetval->nodeNr;
//	wsPopNodeSet childenNodes(res->nodesetval->nodeTab,nChildren);
	wsPopNodeSet childenNodes;
	for(int i=0;i<nChildren;i++)
	{
		childenNodes.push_back(wsPopNode(res->nodesetval->nodeTab[i]));
	}

	xmlXPathFreeObject(res);

	return childenNodes;

}

populationNode flowJoWorkspace::to_popNode(wsRootNode & node){

	populationNode pNode;

	pNode.setName(node.getProperty("name").c_str());

	pNode.fjStats["count"]=atoi(node.getProperty("count").c_str());

	return pNode;
}

populationNode flowJoWorkspace::to_popNode(wsPopNode &node){

	populationNode pNode;
	pNode.setName(node.getProperty("name").c_str());

	pNode.fjStats["count"]=atoi(node.getProperty("count").c_str());

	return pNode;
}
