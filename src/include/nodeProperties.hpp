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
/* gate is polymorphic member,so has to to be pointer
 * can't be reference either, because this member should belong to nodeProperties
 * and destroyed by nodeProperties's destructor ,reference member means refer to the object
 * outside, So it is not possible to instantiate it since we may not need to parse gate if only
 * stats from flowJo are needed.
 * Also reference member's life cycle is different from its host object, which could be problematic.
 *
 */
class nodeProperties{

	friend class boost::serialization::access;
private:
	string thisName;
	gate * thisGate;
	boost::scoped_ptr<POPINDICES> indices;
	POPSTATS fjStats,fcStats;
	bool hidden;


private:
	template<class Archive>
			    void save(Archive &ar, const unsigned int version) const
			    {

					ar & BOOST_SERIALIZATION_NVP(thisName);

					ar.register_type(static_cast<polygonGate *>(NULL));
					ar.register_type(static_cast<ellipseGate *>(NULL));
					ar.register_type(static_cast<boolGate *>(NULL));
					ar.register_type(static_cast<rangeGate *>(NULL));
					ar.register_type(static_cast<rectGate *>(NULL));
					ar.register_type(static_cast<logicalGate *>(NULL));
//					if(version>=3)
						ar.register_type(static_cast<ellipsoidGate *>(NULL));

					ar & BOOST_SERIALIZATION_NVP(thisGate);

					ar.register_type(static_cast<BOOLINDICES *>(NULL));
					ar.register_type(static_cast<INTINDICES *>(NULL));
					ar.register_type(static_cast<ROOTINDICES *>(NULL));
					ar & BOOST_SERIALIZATION_NVP(indices);
			        ar & BOOST_SERIALIZATION_NVP(fjStats);
			        ar & BOOST_SERIALIZATION_NVP(fcStats);

			        if(version<2){
						bool _hidden=false;
						ar & BOOST_SERIALIZATION_NVP(_hidden);
					}else
					{
						ar & BOOST_SERIALIZATION_NVP(hidden);
					}



			    }
	template<class Archive>
			void load(Archive &ar, const unsigned int version)
			{

				ar & BOOST_SERIALIZATION_NVP(thisName);

				ar.register_type(static_cast<polygonGate *>(NULL));
				ar.register_type(static_cast<ellipseGate *>(NULL));
				ar.register_type(static_cast<boolGate *>(NULL));
				ar.register_type(static_cast<rangeGate *>(NULL));
				ar.register_type(static_cast<rectGate *>(NULL));

				if(version>=3)
					ar.register_type(static_cast<ellipsoidGate *>(NULL));

				if(version>=5)
					ar.register_type(static_cast<logicalGate *>(NULL));
				ar & BOOST_SERIALIZATION_NVP(thisGate);

				ar.register_type(static_cast<BOOLINDICES *>(NULL));
				ar.register_type(static_cast<INTINDICES *>(NULL));
				ar.register_type(static_cast<ROOTINDICES *>(NULL));
				ar & BOOST_SERIALIZATION_NVP(indices);
				ar & BOOST_SERIALIZATION_NVP(fjStats);
				ar & BOOST_SERIALIZATION_NVP(fcStats);
				if(version>=1 && version< 4)
				{
					unsigned short dMode;
					ar & BOOST_SERIALIZATION_NVP(dMode);
				}

				if(version<2){
					hidden=false;
				}else
				{
					ar & BOOST_SERIALIZATION_NVP(hidden);
				}

			}
    BOOST_SERIALIZATION_SPLIT_MEMBER()
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
BOOST_CLASS_VERSION(nodeProperties,5)

#endif /* NODEPROPERTIES_HPP_ */
