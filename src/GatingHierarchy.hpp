/*
 * GatingHierarchy.hpp
 *
 *  Created on: Mar 17, 2012
 *      Author: mike
 */

#ifndef GATINGHIERARCHY_HPP_
#define GATINGHIERARCHY_HPP_

#include <string>
#include <vector>
#include "populationTree.hpp"

using namespace std;



class transformation{

};

class compensation{

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
	populationTree tree;
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



#endif /* GATINGHIERARCHY_HPP_ */
