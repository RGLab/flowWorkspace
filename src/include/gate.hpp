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
#define GATING_SET_LEVEL 1
#define GATING_HIERARCHY_LEVEL 2
#define POPULATION_LEVEL 3
#define GATE_LEVEL 4

#define POLYGONGATE 1
#define RANGEGATE 2
#define RECTGATE 3
#define ELLIPSEGATE 4

struct coordinate
{
	float x,y;
};
struct pRange
{
	string name;
	float min, max;
};

/*
 * TODO:possibly implement getCentroid,getMajorAxis,getMinorAxis for all gate types
 */
class gate {
public:
	bool isNegate;
	virtual POPINDICES gating(const flowData &)=0;
	virtual const char * getName()=0;
	virtual unsigned char getType()=0;
	virtual gate * toPolygon()=0;
	virtual vector<string> getParam()=0;
	virtual vector<coordinate> getVertices()=0;
	virtual gate * toEllipseGate()=0;
	virtual gate * toRangeGate()=0;
	virtual coordinate  getCentroid()=0;
	virtual unsigned  getMajorAxis()=0;
	virtual unsigned  getMinorAxis()=0;
};
/*
 * TODO:using #include <boost/multi_array.hpp> instead to make it easier to convert to R data structure hopefully.
 *
 */
class polygonGate:public gate {
	polygonGate* toPolygon(){throw(domain_error("this is already polygonGate!"));};
	coordinate  getCentroid(){throw(domain_error("not valid operation!"));};
	unsigned  getMajorAxis(){throw(domain_error("not valid operation!"));};
	unsigned  getMinorAxis(){throw(domain_error("not valid operation!"));};
public:
	vector<string> params;
	vector<coordinate> vertices;
public:
	const char * getName(){return "polygonGate";}
	unsigned char getType(){return POLYGONGATE;}

	gate * toEllipseGate();
	gate * toRangeGate();
	POPINDICES gating(const flowData &);
	vector<string> getParam(){return params;};
	vector<coordinate> getVertices(){return vertices;};
};

class rangegate:public gate {
	gate * toEllipseGate(){throw(domain_error("not valid operation!"));};
	gate * toRangeGate(){throw(domain_error("not valid operation!"));};
	gate * toPolygon(){throw(domain_error("not valid operation!"));};
	coordinate  getCentroid(){throw(domain_error("not valid operation!"));};
	unsigned  getMajorAxis(){throw(domain_error("not valid operation!"));};
	unsigned  getMinorAxis(){throw(domain_error("not valid operation!"));};
public:
	pRange param;
public:
	const char * getName(){return "rangeGate";}
	unsigned char getType(){return RANGEGATE;}
	POPINDICES gating(const flowData &);
	vector<string> getParam(){
		vector<string> res;
		res.push_back(param.name);
		return res;
	};
	vector<coordinate> getVertices(){
		vector<coordinate> res;
		res.push_back(coordinate{param.min,0});
		res.push_back(coordinate{param.max,0});
		return res;
	};

};

class rectGate:public gate {
	gate * toEllipseGate(){throw(domain_error("not valid operation!"));};
	gate * toRangeGate(){throw(domain_error("not valid operation!"));};
	coordinate  getCentroid(){throw(domain_error("not valid operation!"));};
	unsigned  getMajorAxis(){throw(domain_error("not valid operation!"));};
	unsigned  getMinorAxis(){throw(domain_error("not valid operation!"));};
public:
	vector<pRange> params;

public:
	const char * getName(){return "rectGate";}
	polygonGate* toPolygon();
	POPINDICES gating(const flowData &);
	unsigned char getType(){return RECTGATE;}
	vector<string> getParam(){throw(domain_error("need to convert to polygon gate before using this function!"));};
	vector<coordinate> getVertices(){throw(domain_error("need to convert to polygon gate before using this function!"));};
};

/*
 * using (x-x0)^2/a^2+(y-y0)^2/b^2=1 to represent ellipse
 */
class ellipseGate:public gate {
	gate * toRangeGate(){throw(domain_error("not valid operation!"));};
	gate * toEllipseGate(){throw(domain_error("not valid operation!"));};
	gate * toPolygon(){throw(domain_error("not valid operation!"));};
public:
	vector<string> params;
	unsigned int a,b;
	coordinate center;
public:
	const char * getName(){return "ellipseGate";}
	POPINDICES gating(const flowData &);
	unsigned char getType(){return ELLIPSEGATE;}
	vector<string> getParam(){return params;};
	vector<coordinate> getVertices(){throw(domain_error("not valid operation!"));};
	coordinate  getCentroid(){return center;};
	unsigned  getMajorAxis(){return a;};
	unsigned  getMinorAxis(){return b;};

};
#endif /* GATE_HPP_ */
