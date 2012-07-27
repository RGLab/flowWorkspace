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


using namespace std;


typedef struct{
	vector<string> fullpath;
	char op;
	bool isNot;
	} BOOL_GATE_OP;


#define GATING_SET_LEVEL 1
#define GATING_HIERARCHY_LEVEL 2
#define POPULATION_LEVEL 3
#define GATE_LEVEL 4

#define POLYGONGATE 1
#define RANGEGATE 2
#define BOOLGATE 3

#define AND 1
#define OR 2
#define ANDNOT 3
#define ORNOT 4

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
	virtual vector<BOOL_GATE_OP> getBoolSpec(){throw(domain_error("undefined getBoolSpec function!"));};
	virtual POPINDICES gating(flowData &){throw(domain_error("undefined gating function!"));};
	virtual void extend(flowData &,unsigned short){throw(domain_error("undefined extend function!"));};
	virtual vector<string> getParam(){throw(domain_error("undefined getParam function!"));};
	virtual vertices_valarray getVertices(){throw(domain_error("undefined getVertices function!"));};
	virtual void transforming(trans_local &,unsigned short dMode){throw(domain_error("undefined transforming function!"));};
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

	rangegate * clone(){return new rangegate(*this);};

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
	polygonGate * clone(){return new polygonGate(*this);};
};


/*
 * instead of defining the gating function here in boolGate
 * we put the gating logic in GatingHierarchy gating function
 * because it is needs to access indices from reference nodes
 * which actually belong to GatingHierarchy object.
 * And gate classes are sits in more abstract level than GatingHierarchy in the C++ class tree,
 * thus GatingHierarchy data structure should be invisible to gate.
 */
class boolGate:public gate {
public:
	vector<BOOL_GATE_OP> boolOpSpec;//the gatePaths with the their logical operators
public:
	vector<BOOL_GATE_OP> getBoolSpec(){return boolOpSpec;};
	unsigned short getType(){return BOOLGATE;}
	boolGate * clone(){return new boolGate(*this);};
};


#endif /* GATE_HPP_ */
