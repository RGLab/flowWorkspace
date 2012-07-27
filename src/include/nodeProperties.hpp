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



typedef map<string,float> POPSTATS;
/*
 *TODO: this class should exist apart from populationTree object
 *so all its constructor and desctuctor functions should be private
 */
/* gate is polymorphic memeber,so has to to be pointer
 * can't be reference either, because this member should belong to nodeProperties
 * and destroyed by nodeProperties's destructor ,reference member means refer to the object
 * outside, So it is not possible to instantiate it since we may not need to parse gate if only
 * stats from flowJo are needed
 *
 */
class nodeProperties{

public:
	string thisName;
	gate * thisGate;
	POPINDICES indices;
	POPSTATS fjStats,fcStats;
	bool dMode;

public:
	nodeProperties();

	~nodeProperties();


	POPSTATS getStats(bool);
	unsigned getCounts();
	bool isGated(){return !indices.empty();};
	gate * getGate();
	string getName();
	void setName(const char * popName);
	void setGate(gate *gate);
	void computeStats();
	nodeProperties * clone();

};


#endif /* NODEPROPERTIES_HPP_ */
