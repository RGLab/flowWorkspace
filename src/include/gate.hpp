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

typedef pair<double,double> coordinate;
class gate {
public:
	bool isNegate;
//	virtual S4SXP to_flowCore()=0;
	virtual POPINDICES gating(flowData)=0;

};

/*
 * TODO:using #include <boost/multi_array.hpp> instead to make it easier to convert to R data structure hopefully.
 *
 */
class polygonGate:public gate {
public:
	vector<string> params;
	vector<coordinate> vertices;
	POPINDICES gating(flowData);
};

class rangegate:public gate {
public:
	string pName;
	double min;
	double max;
	POPINDICES gating(flowData);
};

class rectGate:public gate {
public:
	vector<rangegate> params;
	POPINDICES gating(flowData);
};
class ellipseGate:public gate {
public:
//	vector<rangegate> params;
	POPINDICES gating(flowData);
};
#endif /* GATE_HPP_ */
