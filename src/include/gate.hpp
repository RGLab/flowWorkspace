/*
 * gate.hpp
 *
 *  Created on: Mar 16, 2012
 *      Author: wjiang2
 */

#ifndef GATE_HPP_
#define GATE_HPP_
#include <iostream>
#include <string>
#include <limits>
#include <vector>
#include "flowData.hpp"
#include "transformation.hpp"

//#include "transformation.hpp"
using namespace std;



#define GATING_SET_LEVEL 1
#define GATING_HIERARCHY_LEVEL 2
#define POPULATION_LEVEL 3
#define GATE_LEVEL 4

#define POLYGONGATE 1
#define RANGEGATE 2

struct vertices_vector{
	vector<double> x;
	vector<double> y;
};

class vertices_valarray{
public:
	valarray<double> x;
	valarray<double> y;
public:
	void resize(unsigned nSize){
		x.resize(nSize);
		y.resize(nSize);
	}

	vertices_vector toVector(){
		vertices_vector res;
		for(unsigned i=0;i<x.size();i++)
			res.x.push_back(x[i]);
		for(unsigned i=0;i<y.size();i++)
			res.y.push_back(y[i]);

		return res;
	}
	void print(){
		cout<<"x:";
		for(unsigned i=0;i<x.size();i++)
				cout<<x[i]<<",";
//		cout<<"x:";
//		for(unsigned i=0;i<x.size();i++)
//				cout<<x[i]<<",";

	}
};

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
	/*
	 * exact string returned by std::type_info::name() is compiler-dependent
	 * so we can't rely on RTTI
	 */
	virtual unsigned short getType()=0;
	virtual POPINDICES gating(flowData &)=0;
	virtual void extend(flowData &,unsigned short)=0;
	virtual vector<string> getParam()=0;
	virtual vertices_valarray getVertices()=0;
	virtual void transforming(trans_local &,unsigned short dMode)=0;
//	virtual gate * create()=0;
	virtual gate * clone()=0;
};

/*
 * rangeGate and rectGate are currently converted to polygon gate for gating
 * so they may be merged to polygonGate class in the future
 * if it is decided that there is no need to keep them as separate classes
 */
class rangegate:public gate {
public:
	pRange param;
public:
	unsigned short getType(){return RANGEGATE;}
	POPINDICES gating(flowData &);
	void extend(flowData &,unsigned short);
	void transforming(trans_local &,unsigned short dMode);
	vector<string> getParam(){
		vector<string> res;
		res.push_back(param.name);
		return res;
	};
	vertices_valarray getVertices();

	gate * clone(){return new rangegate(*this);};

};
/*
 * TODO:using #include <boost/multi_array.hpp> instead to make it easier to convert to R data structure hopefully.
 *
 */
class polygonGate:public gate {
public:
	vector<string> params;//params.at(0) is x, params.at(1) is y axis
	vector<coordinate> vertices;
public:
	unsigned short getType(){return POLYGONGATE;}
	void extend(flowData &,unsigned short);
	POPINDICES gating(flowData &);
	void transforming(trans_local &,unsigned short dMode);
	vector<string> getParam(){return params;};
	vertices_valarray getVertices();
	gate * clone(){return new polygonGate(*this);};
};


#endif /* GATE_HPP_ */
