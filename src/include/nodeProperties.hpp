/*
 * populationNode.hpp
 *
 *  Created on: Mar 16, 2012
 *      Author: wjiang2
 */

#ifndef NODEPROPERTIES_HPP_
#define NODEPROPERTIES_HPP_

#include "POPINDICES.hpp"

using namespace std;


/*! population stats */
typedef map<string,float> POPSTATS;
/*
 *TODO: this class should exist apart from populationTree object
 *so all its constructor and desctuctor functions should be private
 */
/* gate is polymorphic member,so has to to be pointer
 * can't be reference either, because this member should belong to nodeProperties
 * and destroyed by nodeProperties's destructor ,reference member means refer to the object
 * outside, So it is not possible to instantiate it since we may not need to parse gate if only
 * stats from flowJo are needed.
 * Also reference member's life cycle is different from its host object, which could be problematic.
 *
 */

typedef boost::scoped_ptr<POPINDICES> popIndPtr;/*! the pointer to the event indices*/
/**
 * \class nodeProperties
 * \brief The container that holds gate and population information
 *
 * It has the population name, the pointer to the base \link<gate> object, gate indices and population stats.
 */
class nodeProperties{
private:
	string thisName;
	gate * thisGate; /**< pointer to the abstract gate object */
	popIndPtr indices;/**< scoped_ptr to the POPINDICES */
	POPSTATS fjStats,fcStats;
	bool hidden;


public:
	nodeProperties();
	nodeProperties(const pb::nodeProperties & np_pb);
	nodeProperties(const nodeProperties& np);
	nodeProperties & operator=(nodeProperties np);
	~nodeProperties();


	POPSTATS getStats(bool isFlowCore=false);
	void setStats(POPSTATS s,bool isFlowCore=false);
	unsigned getCounts();
	bool isGated(){return indices.get()!=NULL;};
	gate * getGate();
	string getName();
	void setName(const char * popName);
	void setHiddenFlag(const bool);
	bool getHiddenFlag();

	vector<bool> getIndices();
	void setIndices(vector<bool> _ind);
	void setIndices(unsigned _nEvent);
	void setGate(gate *gate);
	void computeStats();
	void convertToPb(pb::nodeProperties & np_pb, bool isRoot);

};

#endif /* NODEPROPERTIES_HPP_ */
