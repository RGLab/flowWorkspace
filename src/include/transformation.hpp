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
#include <vector>
#include <stdexcept>
#include "spline.hpp"
using namespace std;

struct Spline_Coefs{
	map<string,vector<double> > coefs;
	int method;
	string type;
};
typedef map<string,double> ARGLIST;


class transformation{
public:
	unsigned short type;
	string name;
	string channel;
public:
	virtual ARGLIST getArgs()=0;
	virtual Spline_Coefs getCalTbl()=0;
	virtual void interpolate()=0;
	virtual valarray<double> transforming(valarray<double> &)=0;
};

typedef map<string,transformation *> Trans_map;

class logicalTrans:public transformation{
	Spline_Coefs getCalTbl(){throw(domain_error("not valid operation for logicalTrans class"));}
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
	int spline_method;
	string caltype;
	ARGLIST getArgs(){throw(domain_error("not valid operation for calibration class"));}
public:
	/*
	 * TODO:try boost::tokenizer/boost::split and boost::lexical_cast
	 * or stream and getline to parse it into double array
	 */
	double biExpDecades,biExpNegDecades,w;
	valarray<double> x,y,b,c,d;
//	double * x1,*y1,*b1,*c1,*d1;
//	int n;

public:
	calibrationTable(string _caltype,int _spline_method){
								type=CALTBL;
								caltype=_caltype;
								spline_method=_spline_method;
							};
	void interpolate();
	void init(unsigned);
	valarray<double> transforming(valarray<double> &);
	Spline_Coefs getCalTbl();
};
#endif /* TRANSFORMATION_HPP_ */
