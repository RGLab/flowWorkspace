/*
 * POPINDICES.hpp
 *
 *  Created on: Oct 8, 2012
 *      Author: wjiang2
 */


#ifndef POPINDICES_HPP_
#define POPINDICES_HPP_

#include <map>
#include <string>
#include <vector>
#include "gate.hpp"
#include <boost/scoped_ptr.hpp>

class POPINDICES{

	friend class boost::serialization::access;
protected:
	unsigned nEvents;
private:
	template<class Archive>
						void serialize(Archive &ar, const unsigned int version)
						{

							ar & BOOST_SERIALIZATION_NVP(nEvents);

						}


public:
	POPINDICES(){nEvents=0;};
	virtual vector<bool> getIndices()=0;
	virtual unsigned getCount()=0;
	unsigned getTotal(){return nEvents;}
	virtual POPINDICES * clone()=0;

};

/*
 * bool vector
 */
class BOOLINDICES:public POPINDICES{
	friend class boost::serialization::access;
private:
	vector <bool> x;
	template<class Archive>
							void serialize(Archive &ar, const unsigned int version)
							{
								ar & boost::serialization::make_nvp("POPINDICES",boost::serialization::base_object<POPINDICES>(*this));
								ar & BOOST_SERIALIZATION_NVP(x);

							}


public:
	BOOLINDICES(){nEvents=0;};
	BOOLINDICES(vector <bool> _ind);
	vector<bool> getIndices();
	unsigned getCount();

	POPINDICES * clone();

};

/*
 * int vector
 */
class INTINDICES:public POPINDICES{
	friend class boost::serialization::access;
private:
	vector <unsigned> x;
	template<class Archive>
							void serialize(Archive &ar, const unsigned int version)
							{
								ar & boost::serialization::make_nvp("POPINDICES",boost::serialization::base_object<POPINDICES>(*this));
								ar & BOOST_SERIALIZATION_NVP(x);

							}


public:
	INTINDICES(){nEvents=0;};
	INTINDICES(vector <bool> _ind);
	vector<bool> getIndices();
	unsigned getCount();
	POPINDICES * clone();
};

/*
 * root node
 */
class ROOTINDICES:public POPINDICES{
	friend class boost::serialization::access;
private:

	template<class Archive>
							void serialize(Archive &ar, const unsigned int version)
							{
								ar & boost::serialization::make_nvp("POPINDICES",boost::serialization::base_object<POPINDICES>(*this));
							}


public:
	ROOTINDICES(){nEvents=0;};
	ROOTINDICES(unsigned _nEvents){nEvents=_nEvents;};
	vector<bool> getIndices();
	unsigned getCount();
	POPINDICES * clone();

};


#endif /* POPINDICES_HPP_ */
