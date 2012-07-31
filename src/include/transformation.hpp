/*
 * transformation.hpp
 *
 *  Created on: Apr 24, 2012
 *      Author: wjiang2
 */

#ifndef TRANSFORMATION_HPP_
#define TRANSFORMATION_HPP_

//#define CALTBL 0
//#define LOGICLE 1
//#define BIEXP 2

#include <map>
#include <string>
#include <vector>
#include <stdexcept>
#include "calibrationTable.hpp"

using namespace std;


//typedef map<string,double> ARGLIST;


class transformation{
public:
//	unsigned short type;
	string name;
	string channel;
	calibrationTable calTbl;
	bool isComputed;//this flag allow lazy computCalTbl/interpolation

public:
	transformation();
	virtual valarray<double> transforming(valarray<double> & input);
	virtual void computCalTbl(){};//dummy routine that does nothing
	virtual transformation * clone(){return new transformation(*this);};
};

typedef map<string,transformation *> trans_map;
typedef struct {
		string param;
		bool log;
		unsigned range;
		} PARAM;
typedef vector<PARAM> PARAM_VEC;

PARAM_VEC::iterator findTransFlag(PARAM_VEC & pVec, string name);

class trans_local{
public:
	trans_map transformations;
	transformation * getTran(string);
	trans_map cloneTransMap();
};

class trans_global{
public:
	string groupName;
	trans_map trans;
	vector<int> sampleIDs;

};

typedef vector<trans_global> trans_global_vec;

class biexpTrans:public transformation{
public:
	int channelRange;
	double pos, neg, widthBasis, maxValue;

public:
	biexpTrans();
	void computCalTbl();
	biexpTrans * clone();

};


class logicleTrans:public transformation{
private:
	int channelRange;
	double pos, neg, widthBasis, maxValue;


public:
};

/*
 * TODO:right now set two flags to TRUE in the contructor to avoid doing cal table stuff,
 * we should consider redesign the classes so that logTrans does not share this extra feature from parent class
 */
class logTrans:public transformation{
public:
	logTrans(){isComputed=true;calTbl.isInterpolated=true;};
	valarray<double> transforming(valarray<double> & input);
};


#endif /* TRANSFORMATION_HPP_ */
