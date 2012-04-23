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
//#define RECTGATE 3
//#define ELLIPSEGATE 4

struct coordinate
{
	double x,y;
	coordinate(double _x,double _y){x=_x;y=_y;};
	coordinate(){};
};
struct pRange
{
	string name;
	double min, max;
	pRange(double _min,double _max,string _name){min=_min;max=_max;name=_name;};
	pRange(){};
};

/*
 * TODO:possibly implement getCentroid,getMajorAxis,getMinorAxis for all gate types
 */
class gate {
public:
	bool isNegate;
	virtual POPINDICES gating(const flowData &)=0;
	virtual const char * getName()=0;
	virtual unsigned short getType()=0;
//	virtual gate * toPolygon()=0;
	virtual vector<string> getParam()=0;
	virtual vector<coordinate> getVertices()=0;
//	virtual gate * toEllipseGate()=0;
//	virtual gate * toRangeGate()=0;
//	virtual coordinate  getCentroid()=0;
//	virtual unsigned  getMajorAxis()=0;
//	virtual unsigned  getMinorAxis()=0;
};

/*
 * rangeGate and rectGate are currently converted to polygon gate for gating
 * so they may be merged to polygonGate class in the future
 * if it is decided that there is no need to keep them as separate classes
 */
class rangegate:public gate {
//	gate * toEllipseGate(){throw(domain_error("not valid operation!"));};
//	gate * toRangeGate(){throw(domain_error("not valid operation!"));};
//	gate * toPolygon(){throw(domain_error("not valid operation!"));};
//	coordinate  getCentroid(){throw(domain_error("not valid operation of getCentroid for rangegate!"));};
//	unsigned  getMajorAxis(){throw(domain_error("not valid operation of getMajorAxis for rangegate!"));};
//	unsigned  getMinorAxis(){throw(domain_error("not valid operation of getMinorAxis for rangegate!"));};
public:
	pRange param;
public:
	const char * getName(){return "rangeGate";}
	unsigned short getType(){return RANGEGATE;}
	POPINDICES gating(const flowData &);
	vector<string> getParam(){
		vector<string> res;
		res.push_back(param.name);
		return res;
	};
	vector<coordinate> getVertices(){
		vector<coordinate> res;
		res.push_back(coordinate(param.min,0));
		res.push_back(coordinate(param.max,0));
		return res;
	};

};
/*
 * TODO:using #include <boost/multi_array.hpp> instead to make it easier to convert to R data structure hopefully.
 *
 */
class polygonGate:public gate {
//	polygonGate* toPolygon(){throw(domain_error("this is already polygonGate!"));};
//	coordinate  getCentroid(){throw(domain_error("not valid operation of getCentroid for polygonGate!"));};
//	unsigned  getMajorAxis(){throw(domain_error("not valid operation of getMajorAxis for polygonGate!"));};
//	unsigned  getMinorAxis(){throw(domain_error("not valid operation of getMinorAxis for polygonGate!"));};
public:
	vector<string> params;
	vector<coordinate> vertices;
public:
	const char * getName(){return "polygonGate";}
	unsigned short getType(){return POLYGONGATE;}

//	gate * toEllipseGate();
//	rangegate * toRangeGate();
	POPINDICES gating(const flowData &);
	vector<string> getParam(){return params;};
	vector<coordinate> getVertices(){return vertices;};
};



//class rectGate:public gate {
////	gate * toEllipseGate(){throw(domain_error("not valid operation!"));};
//	gate * toRangeGate(){throw(domain_error("not valid operation!"));};
//	coordinate  getCentroid(){throw(domain_error("not valid operation of getCentroid for rectGate!"));};
//	unsigned  getMajorAxis(){throw(domain_error("not valid operation of getMajorAxis for rectGate!"));};
//	unsigned  getMinorAxis(){throw(domain_error("not valid operation of getMinorAxis for rectGate!"));};
//	vector<string> getParam(){throw(domain_error("need to convert to polygon gate before using this function!"));};
//	vector<coordinate> getVertices(){throw(domain_error("need to convert to polygon gate before using this function!"));};public:
//public:
//	vector<pRange> params;
//
//public:
//	const char * getName(){return "rectGate";}
//	polygonGate* toPolygon();
//	POPINDICES gating(const flowData &);
//	unsigned short getType(){return RECTGATE;}
//
//};

/*
 * using .ellipseFit to convert to polygon
 */
//class ellipseGate:public polygonGate {
//	gate * toRangeGate(){throw(domain_error("not valid operation!"));};
//	gate * toEllipseGate(){throw(domain_error("not valid operation!"));};

//	vector<coordinate> getVertices(){throw(domain_error("not valid operation of getVertices for ellipseGate!"));};
//public:
//	polygonGate * toPolygon();
//	vector<string> params;
//	unsigned int a,b;
//	coordinate center;
//public:
//	const char * getName(){return "ellipseGate";}
//	POPINDICES gating(const flowData &);
//	unsigned short getType(){return ELLIPSEGATE;}
//	vector<string> getParam(){return params;};
//	coordinate  getCentroid(){return center;};
//	unsigned  getMajorAxis(){return a;};
//	unsigned  getMinorAxis(){return b;};

//};
#endif /* GATE_HPP_ */
