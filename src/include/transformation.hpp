/*
 * transformation.hpp
 *
 *  Created on: Apr 24, 2012
 *      Author: wjiang2
 */

#ifndef TRANSFORMATION_HPP_
#define TRANSFORMATION_HPP_

#define CALTBL 0
#define LOG 1
#define LIN 2
//#define LOGICLE 1
//#define BIEXP 2

#include <map>
#include <string>
#include <vector>
#include <stdexcept>
#include "calibrationTable.hpp"

using namespace std;

struct coordinate
{
	double x,y;
	coordinate(double _x,double _y){x=_x;y=_y;};
	coordinate(){};
};
bool compare_x(coordinate i, coordinate j);
bool compare_y(coordinate i, coordinate j);
//typedef map<string,double> ARGLIST;


class transformation{

protected:
	calibrationTable calTbl;
	bool isGateOnly;
	unsigned short type;
	string name;
	string channel;
	bool isComputed;//this flag allow lazy computCalTbl/interpolation

public:
	transformation();
	virtual void transforming(valarray<double> & input);
	virtual void computCalTbl(){};//dummy routine that does nothing
	virtual Spline_Coefs getSplineCoefs(){return calTbl.getSplineCoefs();};
	virtual void setCalTbl(calibrationTable _tbl);
	virtual calibrationTable getCalTbl(){return calTbl;};
	virtual void interpolate(){calTbl.interpolate();};
	virtual bool isInterpolated(){return calTbl.isInterpolated();}
	virtual bool gateOnly(){return isGateOnly;};
	virtual void setGateOnlyFlag(bool _flag){isGateOnly=_flag;};
	virtual bool computed(){return isComputed;};
	virtual void setComputeFlag(bool _flag){isComputed=_flag;};
	virtual string getName(){return name;};
	virtual void setName(string _name){name=_name;};
	virtual string getChannel(){return channel;};
	virtual void setChannel(string _channel){channel=_channel;};
	virtual unsigned short getType(){return type;};
	virtual void setType(unsigned short _type){type=_type;};
	virtual transformation * clone(){return new transformation(*this);};
};

typedef map<string,transformation *> trans_map;
typedef struct {
		string param;
		bool log;
		unsigned range;
		unsigned highValue;
		unsigned calibrationIndex;
		} PARAM;
typedef vector<PARAM> PARAM_VEC;

PARAM_VEC::iterator findTransFlag(PARAM_VEC & pVec, string name);

class trans_local{
	trans_map tp;
public:
	trans_map getTransMap(){return tp;};
	void setTransMap(trans_map _tp){tp=_tp;};
	transformation * getTran(string);
	trans_map cloneTransMap();
	void addTrans(string tName,transformation* trans){tp[tName]=trans;};
};

class trans_global:public trans_local{

	string groupName;
	vector<int> sampleIDs;
public:
	void setSampleIDs(vector<int> _sampleIDs){sampleIDs=_sampleIDs;}
	vector<int> getSampleIDs(){return sampleIDs;}
	string getGroupName(){return groupName;}
	void setGroupName(string _groupName){groupName=_groupName;};
};

typedef vector<trans_global> trans_global_vec;

class biexpTrans:public transformation{
public:
	int channelRange;
	double pos, neg, widthBasis, maxValue;

public:
	biexpTrans();
	void computCalTbl();
	biexpTrans * clone(){return new biexpTrans(*this);};

};


class logicleTrans:public transformation{

	int channelRange;
	double pos, neg, widthBasis, maxValue;


public:
	logicleTrans * clone(){return new logicleTrans(*this);};
};

/*
 * TODO:right now set two flags to TRUE in the contructor to avoid doing cal table stuff,
 * we should consider redesign the classes so that logTrans does not share this extra feature from parent class
 */
class logTrans:public transformation{
public:
	logTrans(){type=LOG;isGateOnly=false;isComputed=true;calTbl.setInterpolated(true);};
	void transforming(valarray<double> & input);
	logTrans * clone(){return new logTrans(*this);};
};

class linTrans:public transformation{

public:
	linTrans(){type=LIN;isGateOnly=true;isComputed=true;calTbl.setInterpolated(true);};
	void transforming(valarray<double> & input);
	linTrans * clone(){return new linTrans(*this);};
};



#endif /* TRANSFORMATION_HPP_ */
