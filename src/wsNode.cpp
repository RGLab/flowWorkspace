/*
 * wsNode.cpp
 *
 *  Created on: Mar 27, 2012
 *      Author: wjiang2
 */



#include "wsNode.hpp"

/*Oftentimes we need to do the xquery based on the context of the current node instead of doc
* it is strange that I haven't found this commonly used API in libxml2
* so have to write my own here
*/
xmlXPathObjectPtr wsNode::xpathInNode(string xpath)
{
	xmlXPathContextPtr ctxt=xmlXPathNewContext(thisNode->doc);
	ctxt->node=thisNode;
	xmlXPathObjectPtr res=xmlXPathEval((xmlChar *)xpath.c_str(),ctxt);
	xmlXPathFreeContext(ctxt);
	return res;
}

populationNode wsRootNode::to_popNode(){
	xmlChar * popName=xmlGetProp(thisNode,(const xmlChar*)"name");

	populationNode pNode;
	pNode.setName((const char *)popName);

	xmlFree(popName);

	return pNode;
}

populationNode wsPopNode::to_popNode(){
	xmlChar * popName=xmlGetProp(thisNode,(const xmlChar*)"name");

	populationNode pNode;
	pNode.setName((const char *)popName);

	xmlFree(popName);

	return pNode;
}
