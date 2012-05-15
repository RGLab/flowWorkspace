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
	calibrationTable * calTbl;
public:
	~transformation();
//	virtual ARGLIST getArgs()=0;
	valarray<double> transforming(valarray<double> & input);
	void computCalTbl();
};

typedef map<string,transformation *> trans_map;
typedef vector<transformation*> trans_vec;

class biexpTrans:public transformation{
public:
	int channelRange;
	double pos, neg, widthBasis, maxValue;

public:
//	ARGLIST args;
public:
//	ARGLIST getArgs(){return args;}
//	vector<ARG> getArgs(){
//							vector<ARG> res;
//							res.push_back(ARG("m",m));
//							res.push_back(ARG("t",t));
//							res.push_back(ARG("w",w));
//							return res;
//							};
	biexpTrans();
	void computCalTbl();
//	valarray<double> transforming(valarray<double> &){throw(domain_error("not implemented yet!"));};
};


class logicleTrans:public transformation{
private:
	int channelRange;
	double pos, neg, widthBasis, maxValue;

public:
//	ARGLIST args;
public:
//	ARGLIST getArgs(){return args;}
//	vector<ARG> getArgs(){
//							vector<ARG> res;
//							res.push_back(ARG("m",m));
//							res.push_back(ARG("t",t));
//							res.push_back(ARG("w",w));
//							return res;
//							};
//	logicleTrans(){type=LOGICLE;};
//	valarray<double> transforming(valarray<double> &){throw(domain_error("not implemented yet!"));};
};


#endif /* TRANSFORMATION_HPP_ */
