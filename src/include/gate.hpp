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

typedef pair<float,float> coordinate;//first is X, second is Y
class gate {
public:
	bool isNegate;
//	virtual S4SXP to_flowCore()=0;
	virtual void gating(const flowData &,POPINDICES *)=0;
};

/*
 * TODO:using #include <boost/multi_array.hpp> instead to make it easier to convert to R data structure hopefully.
 *
 */
class polygonGate:public gate {
public:
	vector<string> params;
	vector<coordinate> vertices;

	void gating(const flowData &,POPINDICES *);
};

class rangegate:public gate {
public:
	string pName;
	double min;
	double max;
	void gating(const flowData &,POPINDICES *);
};

class rectGate:public gate {
public:
	vector<rangegate> params;
	void gating(const flowData &,POPINDICES *);
};
class ellipseGate:public gate {
public:
//	vector<rangegate> params;
	void gating(const flowData &,POPINDICES *);
};
#endif /* GATE_HPP_ */
