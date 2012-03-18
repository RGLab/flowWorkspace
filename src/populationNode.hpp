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
#include "gate.hpp"

using namespace std;

class populationNode{
	map<string,double> fjStats;
	map<string,double> fcStats;
	gate thisGate;
	vector<bool> indice;
public:
	vector<bool> getIndice(){return(this->indice);};

	map<string,double> getStats(bool isFlowCore=1){
		return(isFlowCore?this->fcStats:this->fjStats);
		};

	gate getGate(){return(this->thisGate);};
};


#endif /* POPULATIONNODE_HPP_ */
