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

	xmlNodePtr thisNode;
public:
	string getProperty(string propName);
	string getNsProperty(string propName,string ns);
	string getContent();
	xmlXPathObjectPtr xpathInNode(string xpath);
	xmlXPathObjectPtr xpath(string xpath);
	xmlNodePtr getNodePtr(){return thisNode;};
	void setNodePtr(xmlNodePtr n){thisNode=n;};
	wsNode(xmlNodePtr node){thisNode=node;};
	wsNode(){};
};



class wsSampleNode:public wsNode{
public:
//	wsSampleNode();
	wsSampleNode(xmlNodePtr node){setNodePtr(node);};


};

class wsRootNode:public wsNode{
public:
	wsRootNode(xmlNodePtr node){setNodePtr(node);};
};

class wsPopNode:public wsNode{
public:
	wsPopNode(xmlNodePtr node){setNodePtr(node);};
//	wsPopNode(){};
};

class wsRectGateNode:public wsNode{
public:
	wsRectGateNode(xmlNodePtr node){setNodePtr(node);};
//	wsPopNode(){};
};

class wsPolyGateNode:public wsNode{
public:
	wsPolyGateNode(xmlNodePtr node){setNodePtr(node);};

};

class wsEllipseGateNode:public wsNode{
public:
	wsEllipseGateNode(xmlNodePtr node){setNodePtr(node);};
};

class wsRangeGateNode:public wsNode{
public:
	wsRangeGateNode(xmlNodePtr node){setNodePtr(node);};
};

class wsBooleanGateNode:public wsNode{
public:
	wsBooleanGateNode(xmlNodePtr node){setNodePtr(node);};

};

typedef vector<wsPopNode> wsPopNodeSet;


#endif /* WSNODE_HPP_ */

