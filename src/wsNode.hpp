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
	virtual populationNode to_popNode()=0;
};



class wsRootNode:public wsNode{
public:
	populationNode to_popNode();
};

class wsPopNode:public wsNode{
public:
	populationNode to_popNode();
};

#endif /* WSNODE_HPP_ */
