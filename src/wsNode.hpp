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



class wsNode{
public:
	xmlNodePtr thisNode;
	xmlXPathObjectPtr xpathInNode(string xpath);


};

class wsNodeSet{
public:
	wsNode * nodes;
	int number;
};

class wsSampleNode:public wsNode{
public:
	wsSampleNode(xmlNodePtr node){thisNode=node;};
	populationNode to_popNode(){
		cout<<"SampeNode can't be converted to population Node!"<<endl;
		populationNode empty;
		return(empty);
	};
};

class wsRootNode:public wsNode{
public:
	wsRootNode(xmlNodePtr node){thisNode=node;};
};

class wsPopNode:public wsNode{
public:
	wsPopNode(xmlNodePtr node){thisNode=node;};
};


#endif /* WSNODE_HPP_ */

