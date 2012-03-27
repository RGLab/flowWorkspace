/*
 * wsNode.hpp
 *
 *  Created on: Mar 22, 2012
 *      Author: wjiang2
 */

#ifndef WSNODE_HPP_
#define WSNODE_HPP_
#include "populationNode.hpp"
#include <libxml/tree.h>
#include <libxml/xpath.h>


class wsNode{
public:
	xmlNodePtr thisNode;
	xmlXPathObjectPtr xpathInNode(string xpath);


};



class wsSampleNode:public wsNode{
public:
	wsSampleNode(xmlNodePtr node){thisNode=node;};

};

class wsRootNode:public wsNode{
public:
	wsRootNode(xmlNodePtr node){thisNode=node;};
};

class wsPopNode:public wsNode{
public:
	wsPopNode(xmlNodePtr node){thisNode=node;};
	wsPopNode(){};
};

typedef vector<wsPopNode> wsPopNodeSet;
//class wsPopNodeSet{
//public:
//	wsPopNode * nodes;
//	int number;
//	wsPopNodeSet(xmlNodePtr *,int nSize){
//		childenNodes.nodes=new wsPopNode [nChildren];
//			for(int i=0;i<nChildren;i++)
//				{
//					childenNodes.nodes[i]=wsPopNode(res->nodesetval->nodeTab[i]);
//				}
//
//	}
//};

#endif /* WSNODE_HPP_ */

