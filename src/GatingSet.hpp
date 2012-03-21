/*
 * GatingSet.hpp
 *
 *  Created on: Mar 15, 2012
 *      Author: wjiang2
 */



#ifndef GATINGSET_HPP_
#define GATINGSET_HPP_
#include "GatingHierarchy.hpp"
#include "flowJoWorkspace.hpp"
#include <string>
#include <map>

using namespace std;

/*GatingSet is multiple GatingHierarchies that has the flow data associated and gated*/
class GatingSet{
	flowJoWorkspace * ws;
	map<string,GatingHierarchy> ghs;
public:
	~GatingSet();

	GatingHierarchy getGatingHierarchy(string sampleName);
	void openWorkspace(const char * );
	 void parseWorkspace(unsigned short);

};
#endif /* GATINGSET_HPP_ */
