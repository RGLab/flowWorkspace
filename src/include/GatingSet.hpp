/*
 * GatingSet.hpp
 *
 *  Created on: Mar 15, 2012
 *      Author: wjiang2
 */



#ifndef GATINGSET_HPP_
#define GATINGSET_HPP_
#include "GatingHierarchy.hpp"
#include "macFlowJoWorkspace.hpp"
#include "winFlowJoWorkspace.hpp"
#include <string>
#include <map>
#include <boost/foreach.hpp>

using namespace std;
/*
 * have to use pointer GatingHierarchy * here,
 * because GatingHierarchy's destructor is responsible for clearing dynamically allocated memory
 * within GatingHierarchy,like nodeProperties * that further contains the gate *.
 * if don't use GatingHierarchy *,then these children pointers have to be taken care of ouside of
 * GatingHierarchy class,which could be problematic.
 *
 */
typedef map<string,GatingHierarchy*> gh_map;

/*GatingSet is multiple GatingHierarchies that has the flow data associated and gated*/
class GatingSet{

//	CALTBS calTbls;
	trans_global_vec gTrans;
	gh_map ghs;
	ncdfFlow nc;
	/*
	 * we assume the GatingSet is mainly for read-access
	 * so we have a copy of samplenames here as a vector
	 * for the convienient access samplenames
	 * If there is need to avoid data-inconsistency,we can loop through the map
	 * and return the sampleList
	 */
//	vector<string> sampleList;
	unsigned short dMode;//debug level to control print out
public:
	workspace * ws;
public:
	~GatingSet();
	GatingSet(string,bool,unsigned short);
	GatingHierarchy * getGatingHierarchy(string );
	GatingHierarchy * getGatingHierarchy(unsigned int);
	void gating();
	void parseWorkspace(unsigned short,bool);
	void parseWorkspace(vector<string>,bool);
	vector<string> getSamples(void);
	void attachData(string,vector<string>,vector<string>);
	ncdfFlow getNcObj(){return nc;}
};
#endif /* GATINGSET_HPP_ */
