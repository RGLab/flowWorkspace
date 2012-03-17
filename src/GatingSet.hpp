/*
 * GatingSet.hpp
 *
 *  Created on: Mar 15, 2012
 *      Author: wjiang2
 */



#ifndef GATINGSET_HPP_
#define GATINGSET_HPP_

#include <string>
#include <vector>
using namespace std;



class transformation{

};

class compensation{

};

class gate{

};
/*GatingHierarchy is a tree that holds the gate definitions hierarchically,
 along with the transformation functions and compensation matrix,
 Once the one FCS file is associated,the tree can also hold indices that subset the events
 It can serves as a gating template when data is empty
 */

class GatingHierarchy{
//	transformation trans;
//	compensation comp;
//	double ** data;
//	string sampleName;
//	tree:
public:
	/*retrieve the gate definition from a particular node*/
//	gate getGate(unsigned short gateid);
//	gate getGate(string popName);

	/*remove the gate from */
//	void removeGate(unsigned short popId);
//	void removeGate(string popName);

	/*append the gate to the tree*/
//	void addGate(gate& g,string popName);

	/*associate the tree with data matrix*/
//	void addData();

	/**/
//	vector<bool> * getIndice(string popName);
//	vector<bool> * getData(string popName);

//	void gating(gate& g,string popName);
public:
	void gating(){cout <<"test gating"<<endl;};
};

/*GatingSet is multiple GatingHierarchies that has the flow data associated and gated*/
class GatingSet{
	GatingHierarchy *ghs;
public:
	GatingHierarchy getGatingHierarchy(string sampleName);
};
#endif /* GATINGSET_HPP_ */
