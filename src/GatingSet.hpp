/*
 * GatingSet.hpp
 *
 *  Created on: Mar 15, 2012
 *      Author: wjiang2
 */



#ifndef GATINGSET_HPP_
#define GATINGSET_HPP_
#include "GatingHierarchy.hpp"
#include <string>

/*GatingSet is multiple GatingHierarchies that has the flow data associated and gated*/
class GatingSet{
	GatingHierarchy *ghs;
public:
	GatingHierarchy getGatingHierarchy(string sampleName);
};
#endif /* GATINGSET_HPP_ */
