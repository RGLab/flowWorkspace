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
//	friend std::ostream & operator<<(std::ostream &os, const nodeProperties &gh);
	friend class boost::serialization::access;
private:
	string thisName;
	gate * thisGate;
	boost::scoped_ptr<POPINDICES> indices;
	POPSTATS fjStats,fcStats;
public:
	unsigned short dMode;
private:
	template<class Archive>
			    void serialize(Archive &ar, const unsigned int version)
			    {

					ar & BOOST_SERIALIZATION_NVP(thisName);

					ar.register_type(static_cast<polygonGate *>(NULL));
					ar.register_type(static_cast<ellipseGate *>(NULL));
					ar.register_type(static_cast<boolGate *>(NULL));
					ar.register_type(static_cast<rangegate *>(NULL));
					ar.register_type(static_cast<rectgate *>(NULL));
					ar & BOOST_SERIALIZATION_NVP(thisGate);

					ar.register_type(static_cast<BOOLINDICES *>(NULL));
					ar.register_type(static_cast<INTINDICES *>(NULL));
					ar.register_type(static_cast<ROOTINDICES *>(NULL));
					ar & BOOST_SERIALIZATION_NVP(indices);
			        ar & BOOST_SERIALIZATION_NVP(fjStats);
			        ar & BOOST_SERIALIZATION_NVP(fcStats);
			        if(version==0){
			        	bool _dMode;
			        	ar & BOOST_SERIALIZATION_NVP(_dMode);
			        }else
			        {
			        	ar & BOOST_SERIALIZATION_NVP(dMode);
			        }

			    }
public:
	nodeProperties();

	~nodeProperties();


	POPSTATS getStats(bool isFlowCore=false);
	void setStats(POPSTATS s,bool isFlowCore=false);
	unsigned getCounts();
	bool isGated(){return indices.get()!=NULL;};
	gate * getGate();
	string getName();
	void setName(const char * popName);
	vector<bool> getIndices();
	void setIndices(vector<bool> _ind);
	void setIndices(unsigned _nEvent);
	void setGate(gate *gate);
	void computeStats();
	nodeProperties * clone(bool gateResult=false);

};
BOOST_CLASS_VERSION(nodeProperties,1)

#endif /* NODEPROPERTIES_HPP_ */
