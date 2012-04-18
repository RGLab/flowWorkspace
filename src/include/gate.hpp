/*
 * gate.hpp
 *
 *  Created on: Mar 16, 2012
 *      Author: wjiang2
 */

#ifndef GATE_HPP_
#define GATE_HPP_
//#include <Rinternals.h>
//#include <Rdefines.h>
//#include <Rmath.h>
#include <iostream>
#include <string>
#include <limits>
#include <vector>
#include "flowData.hpp"
using namespace std;
#define POLYGONGATE 0;
#define RANGEGATE 1;
#define RECTGATE 2;
#define ELLIPSEGATE 3;

typedef pair<float,float> coordinate;//first is X, second is Y
struct pRange
{
	string name;
	float min, max;
};

class gate {
public:
	bool isNegate;
//	virtual S4SXP to_flowCore()=0;
	virtual POPINDICES gating(const flowData &)=0;
	virtual const char * getName()=0;
	virtual unsigned char getType()=0;
	virtual gate * toPolygon()=0;
	virtual vector<string> getParam()=0;
	virtual vector<coordinate> getVertices()=0;

};

/*
 * TODO:using #include <boost/multi_array.hpp> instead to make it easier to convert to R data structure hopefully.
 *
 */
class polygonGate:public gate {
public:
	vector<string> params;
	vector<coordinate> vertices;
	const char * getName(){return "polygonGate";}
	unsigned char getType(){return POLYGONGATE;}
	polygonGate* toPolygon(){throw(domain_error("this is already polygonGate!"));};
	POPINDICES gating(const flowData &);
	vector<string> getParam(){return params;};
	vector<coordinate> getVertices(){return vertices;};
};

class rangegate:public gate {
public:
	pRange param;
	const char * getName(){return "rangegate";}
	unsigned char getType(){return RANGEGATE;}
	polygonGate* toPolygon();
	POPINDICES gating(const flowData &);
	vector<string> getParam(){throw(domain_error("need to convert to polygon gate before using this function!"));};
	vector<coordinate> getVertices(){throw(domain_error("need to convert to polygon gate before using this function!"));};

};

class rectGate:public gate {
public:
	vector<pRange> params;
	const char * getName(){return "rectGate";}
	polygonGate* toPolygon();
	POPINDICES gating(const flowData &);
	unsigned char getType(){return RECTGATE;}
	vector<string> getParam(){throw(domain_error("need to convert to polygon gate before using this function!"));};
	vector<coordinate> getVertices(){throw(domain_error("need to convert to polygon gate before using this function!"));};
};
class ellipseGate:public gate {
public:
//	vector<rangegate> params;
	const char * getName(){return "ellipseGate";}
	polygonGate* toPolygon();
	POPINDICES gating(const flowData &);
	unsigned char getType(){return ELLIPSEGATE;}
	vector<string> getParam(){throw(domain_error("need to convert to polygon gate before using this function!"));};
	vector<coordinate> getVertices(){throw(domain_error("need to convert to polygon gate before using this function!"));};

};
#endif /* GATE_HPP_ */
