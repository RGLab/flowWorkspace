/*
 * populationNode.hpp
 *
 *  Created on: Mar 16, 2012
 *      Author: wjiang2
 */

#ifndef NODEPROPERTIES_HPP_
#define NODEPROPERTIES_HPP_

#include <map>
#include <string>
#include <vector>
#include "gate.hpp"
using namespace std;

typedef map<string,double> POPSTATS;
/*
 *TODO: this class should exist apart from populationTree object
 *so all its constructor and desctuctor functions should be private
 */
class nodeProperties{
public:
	string thisName;
	gate * thisGate;
	POPINDICES indices;

	POPSTATS fjStats,fcStats;
public:
	nodeProperties();
	~nodeProperties();//since gate is dynamically created,needs to be freed here in destroy method
//	valarray<bool> * getIndice(){return(this->thisIndice);};

	POPSTATS getStats(bool);

	gate * getGate();
	string getName();
	void setName(const char * popName);
	void setGate(gate *gate);
//	void setIndice(vector<bool> indice){thisIndice=indice;};


};


#endif /* NODEPROPERTIES_HPP_ */
