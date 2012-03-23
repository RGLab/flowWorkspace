/*
 * workspaceNode.cpp
 *
 *  Created on: Mar 22, 2012
 *      Author: wjiang2
 */

#include "wsNode.hpp"



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
