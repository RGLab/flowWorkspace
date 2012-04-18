/*
 * wsNode.hpp
 *
 *  Created on: Mar 22, 2012
 *      Author: wjiang2
 */

#ifndef WSNODE_HPP_
#define WSNODE_HPP_
#include "nodeProperties.hpp"
#include <libxml/tree.h>
#include <libxml/xpath.h>


class wsNode{
public:
	xmlNodePtr thisNode;
	string getProperty(string propName);
	xmlXPathObjectPtr xpathInNode(string xpath);
	wsNode(xmlNodePtr node){thisNode=node;};
	wsNode(){};
};



class wsSampleNode:public wsNode{
public:
//	wsSampleNode();
	wsSampleNode(xmlNodePtr node){thisNode=node;};

};

class wsRootNode:public wsNode{
public:
	wsRootNode(xmlNodePtr node){thisNode=node;};
};

class wsPopNode:public wsNode{
public:
	wsPopNode(xmlNodePtr node){thisNode=node;};
//	wsPopNode(){};
};

class wsRectGateNode:public wsNode{
public:
	wsRectGateNode(xmlNodePtr node){thisNode=node;};
//	wsPopNode(){};
};

class wsPolyGateNode:public wsNode{
public:
	wsPolyGateNode(xmlNodePtr node){thisNode=node;};
//	wsPopNode(){};
};

class wsEllipseGateNode:public wsNode{
public:
	wsEllipseGateNode(xmlNodePtr node){thisNode=node;};
//	wsPopNode(){};
};

//typedef vector<wsNode> wsNodeSet;
typedef vector<wsPopNode> wsPopNodeSet;


#endif /* WSNODE_HPP_ */

