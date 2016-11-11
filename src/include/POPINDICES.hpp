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

/**
 * \class POPINDICES
 * \brief the event indices for the subpopulation
 *
 *  It is an abstract class that hides the details of implementation of the actual indices type.
 */
class POPINDICES{
protected:
	unsigned nEvents;
public:
	POPINDICES():nEvents(0){};
	POPINDICES(unsigned _nEvents):nEvents(_nEvents){};
	virtual ~POPINDICES(){};
	/**
	 * convert the POPINDICES to bool vector
	 *
	 */
	virtual vector<bool> getIndices()=0;
	/**
	 * compute the event count from the event indices
	 */
	virtual unsigned getCount()=0;
	/**
	 * retrieve the total number of events for the original ungated events data
	 */
	unsigned getTotal(){return nEvents;}
	virtual POPINDICES * clone()=0;
	virtual void convertToPb(pb::POPINDICES & ind_pb) = 0;

};

void packToBytes(const vector <bool> & x, vector<unsigned char> &);
void unpackFromBytes(vector <bool> &, const vector<unsigned char>& x_bytes);
/*
 * bool vector
 */
class BOOLINDICES:public POPINDICES{
private:
	vector <bool> x;
public:
	BOOLINDICES():POPINDICES(){};
	BOOLINDICES(vector <bool> _ind);
	vector<bool> getIndices();
	unsigned getCount();

	POPINDICES * clone();
	void convertToPb(pb::POPINDICES & ind_pb);
	BOOLINDICES(const pb::POPINDICES & ind_pb);

};

/*
 * int vector
 */
class INTINDICES:public POPINDICES{
private:
	vector <unsigned> x;
public:
	INTINDICES():POPINDICES(){};
	INTINDICES(vector <bool> _ind);
	INTINDICES(const pb::POPINDICES & ind_pb);
	vector<bool> getIndices();
	unsigned getCount();
	POPINDICES * clone();
	void convertToPb(pb::POPINDICES & ind_pb);
};

/*
 * root node
 */
class ROOTINDICES:public POPINDICES{

public:
	ROOTINDICES():POPINDICES(){};
	ROOTINDICES(unsigned _nEvents):POPINDICES(_nEvents){};
	ROOTINDICES(const pb::POPINDICES & ind_pb);
	vector<bool> getIndices();
	unsigned getCount();
	POPINDICES * clone();
	void convertToPb(pb::POPINDICES & ind_pb);
};


#endif /* POPINDICES_HPP_ */
