/*
 * transformation.hpp
 *
 *  Created on: Apr 24, 2012
 *      Author: wjiang2
 */

#ifndef TRANSFORMATION_HPP_
#define TRANSFORMATION_HPP_

#define CALTBL 0
#define LOGICLE 1
#include <map>
#include <string>
#include <stdexcept>
#include "spline.hpp"
using namespace std;

//typedef pair<string,double> ARG;
typedef map<string,double> ARGLIST;


class transformation{
public:
	unsigned short type;
	string name;
	string channel;
public:
	virtual ARGLIST getArgs()=0;
//	virtual string getCalTbl()=0;
	virtual void interpolate()=0;
	virtual valarray<double> transforming(valarray<double> &)=0;
};

typedef map<string,transformation *> Trans_map;

class logicalTrans:public transformation{
//	string getCalTbl(){throw(domain_error("not valid operation for logicalTrans class"));}
	void interpolate(){throw(domain_error("not valid operation for logicalTrans class"));}
public:
	ARGLIST args;
public:
	ARGLIST getArgs(){return args;}
//	vector<ARG> getArgs(){
//							vector<ARG> res;
//							res.push_back(ARG("m",m));
//							res.push_back(ARG("t",t));
//							res.push_back(ARG("w",w));
//							return res;
//							};
	logicalTrans(){type=LOGICLE;};
	valarray<double> transforming(valarray<double> &){throw(domain_error("not implemented yet!"));};
};

class calibrationTable:public transformation{
	ARGLIST getArgs(){throw(domain_error("not valid operation for calibration class"));}
public:
	/*
	 * TODO:try boost::tokenizer/boost::split and boost::lexical_cast
	 * or stream and getline to parse it into double array
	 */
	double biExpDecades,biExpNegDecades,w;
	valarray<double> x,y,b,c,d;
	double * x1,*y1,*b1,*c1,*d1;
//	int n;

public:
	calibrationTable(){type=CALTBL;};
	void interpolate();
	void init(unsigned);
	valarray<double> transforming(valarray<double> &);
//	string getCalTbl(){return tbl;}
};
#endif /* TRANSFORMATION_HPP_ */
