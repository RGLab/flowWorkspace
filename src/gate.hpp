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
using namespace std;

typedef pair<double,double> coordinate;

class gate {
public:
	bool isNegate;
//	virtual S4SXP to_flowCore()=0;
};

class polygonGate:public gate {
public:
	vector<string> params;
	vector<coordinate> vertices;

};

class rangegate:public gate {
public:
	string pName;
	double min;
	double max;
};

class rectGate:public gate {
public:
	vector<rangegate> params;
};
#endif /* GATE_HPP_ */
