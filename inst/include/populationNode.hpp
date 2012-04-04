/*
 * populationNode.hpp
 *
 *  Created on: Mar 16, 2012
 *      Author: wjiang2
 */

#ifndef POPULATIONNODE_HPP_
#define POPULATIONNODE_HPP_

#include <map>
#include <string>
#include <vector>
#include "gate.hpp"

using namespace std;

class populationNode{
public:
	string thisName;
	gate * thisGate;
	vector<bool> thisIndice;

	map<string,double> fjStats;
	map<string,double> fcStats;
public:
	populationNode(){thisGate=NULL;};
//	~populationNode(){delete thisGate;};//since gate is dynamically created,needs to be freed here in destroy method
	vector<bool> getIndice(){return(this->thisIndice);};

	map<string,double> getStats(bool isFlowCore=1){
		return(isFlowCore?this->fcStats:this->fjStats);
		};

	gate * getGate(){return(this->thisGate);};
	string getName(){return(this->thisName);};
	void setName(const char * popName){thisName=popName;};
	void setGate(gate *gate){thisGate=gate;};
	void setIndice(vector<bool> indice){thisIndice=indice;};


};


#endif /* POPULATIONNODE_HPP_ */
