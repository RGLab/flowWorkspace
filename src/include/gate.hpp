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
//#include <R_ext/Constants.h>
#include "compensation.hpp"
#include "ellipse2points.hpp"
using namespace std;


struct BOOL_GATE_OP{
	vector<string> path;
	char op;
	bool isNot;
	void convertToPb(pb::BOOL_GATE_OP & BOOL_GATE_OP_pb){
		BOOL_GATE_OP_pb.set_isnot(isNot);
		BOOL_GATE_OP_pb.set_op(op);
		for(unsigned i = 0; i < path.size(); i++){
			 BOOL_GATE_OP_pb.add_path(path.at(i));
		}
	};
	BOOL_GATE_OP(){};
	BOOL_GATE_OP(const pb::BOOL_GATE_OP & BOOL_GATE_OP_pb){
		op = BOOL_GATE_OP_pb.op();
		isNot = BOOL_GATE_OP_pb.isnot();
		for(int i = 0; i < BOOL_GATE_OP_pb.path_size(); i++)
			path.push_back(BOOL_GATE_OP_pb.path(i));
	};
	template<class Archive>
				    void serialize(Archive &ar, const unsigned int version)
				    {

						ar & BOOST_SERIALIZATION_NVP(path);
						ar & BOOST_SERIALIZATION_NVP(op);
						ar & BOOST_SERIALIZATION_NVP(isNot);
				    }

} ;

const double pi = 3.1415926535897;



#define POLYGONGATE 1
#define RANGEGATE 2
#define BOOLGATE 3
#define ELLIPSEGATE 4
#define RECTGATE 5
#define LOGICALGATE 6
#define CURLYQUADGATE 7

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
	vertices_valarray(){};
	vertices_valarray(vector<coordinate> vertices){

			unsigned nSize=vertices.size();
			resize(nSize);
			for(unsigned i=0;i<nSize;i++)
			{
				x[i]=vertices.at(i).x;
				y[i]=vertices.at(i).y;
			}

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
		COUT<<"x:";
		for(unsigned i=0;i<x.size();i++)
				COUT<<x[i]<<",";
//		COUT<<"x:";
//		for(unsigned i=0;i<x.size();i++)
//				COUT<<x[i]<<",";

	}
};


class paramRange
{

private:

	string name;
	double min, max;
public:
	paramRange(double _min,double _max,string _name){min=_min;max=_max;name=_name;};
	paramRange(){};
	vertices_valarray toValarray();
	void setName(string _n){name=_n;};
	void updateChannels(const CHANNEL_MAP & chnl_map){

			CHANNEL_MAP::const_iterator itChnl = chnl_map.find(name);
			if(itChnl!=chnl_map.end())
				name = itChnl->second;
	};
	string getName(){return name;}
	vector<string> getNameArray(){
			vector<string> res;
			res.push_back(name);
			return res;
		};
	double getMin(){return min;};
	void setMin(double _v){min=_v;};
	double getMax(){return max;};
	void setMax(double _v){max=_v;};
	void convertToPb(pb::paramRange & paramRange_pb){paramRange_pb.set_name(name);paramRange_pb.set_max(max);paramRange_pb.set_min(min);};
	paramRange(const pb::paramRange & paramRange_pb):name(paramRange_pb.name()),min(paramRange_pb.min()),max(paramRange_pb.max()){};
};
class paramPoly
{
private:


	vector<string> params;//params.at(0) is x, params.at(1) is y axis
	vector<coordinate> vertices;
public:
	vector<coordinate> getVertices(){return vertices;};
	void setVertices(vector<coordinate> _v){vertices=_v;};
	vector<string>  getNameArray(){return params;};
	void setName(vector<string> _params){params=_params;};
	void updateChannels(const CHANNEL_MAP & chnl_map){

			for(vector<string>::iterator it = params.begin(); it != params.end(); it++)
			{
				string curName = *it;

				CHANNEL_MAP::const_iterator itChnl = chnl_map.find(curName);
				if(itChnl!=chnl_map.end())
					*it = itChnl->second;
			}
		};
	vertices_valarray toValarray();
	string xName(){return params.at(0);};
	string yName(){return params.at(1);};
	paramPoly(){};
	void convertToPb(pb::paramPoly & paramPoly_pb){
		BOOST_FOREACH(vector<string>::value_type & it, params){
			paramPoly_pb.add_params(it);
		}
		BOOST_FOREACH(vector<coordinate>::value_type & it, vertices){
			pb::coordinate * coor_pb = paramPoly_pb.add_vertices();
			it.convertToPb(*coor_pb);
		}
	};
	paramPoly(const pb::paramPoly & paramPoly_pb){
		for(int i = 0; i < paramPoly_pb.params_size(); i++){
			params.push_back(paramPoly_pb.params(i));
		}
		for(int i = 0; i < paramPoly_pb.vertices_size(); i++){
			vertices.push_back(coordinate(paramPoly_pb.vertices(i)));
		}
	};
};


/*
 * TODO:possibly implement getCentroid,getMajorAxis,getMinorAxis for all gate types
 */
/*
 * Important:
 *
 * now that nodePorperties class has customized copy constructor that uses clone member function
 * form gate class. Thus it is necessary to define clone function for each derived gate class
 * in order to avoid the dispatching to parent method and thus degraded to the parent gate object
 */
/**
 * \class gate
 * \brief the base gate class
 *
 * It is an abstract class that is inherited by other concrete gate types.
 */
class gate {
protected:
	bool neg;
	bool isTransformed;
	bool isGained;

public:
	/*
	 * exact string returned by std::type_info::name() is compiler-dependent
	 * so we can't rely on RTTI. instead we return the gate type by API
	 * However it is against the motivation for nodeProperty to use base gate pointer
	 * the very reason of this gate abstraction was to make gatingheirarhcy being agnostic
	 * about the gate type. The reason we are doing it is a compromise to the needs of R API getGate
	 */
	gate ();
	gate(const pb::gate & gate_pb);
	virtual void convertToPb(pb::gate & gate_pb);
	virtual ~gate(){};
	virtual unsigned short getType()=0;
	virtual vector<BOOL_GATE_OP> getBoolSpec(){throw(domain_error("undefined getBoolSpec function!"));};
	virtual vector<bool> gating(flowData &){throw(domain_error("undefined gating function!"));};
	virtual void extend(flowData &,float){throw(domain_error("undefined extend function!"));};
	virtual void extend(float,float){throw(domain_error("undefined extend function!"));};
	virtual void gain(map<string,float> &){throw(domain_error("undefined gain function!"));};
	virtual vector<string> getParamNames(){throw(domain_error("undefined getParam function!"));};
	virtual vertices_valarray getVertices(){throw(domain_error("undefined getVertices function!"));};
	virtual void transforming(trans_local &){throw(domain_error("undefined transforming function!"));};
	virtual void updateChannels(const CHANNEL_MAP & chnl_map){throw(domain_error("undefined updateChannels function!"));};
	virtual gate * clone()=0;
	virtual bool isNegate(){return neg;};
	virtual bool gained(){return isGained;};
	virtual void setNegate(bool _neg){neg=_neg;};
	virtual bool Transformed(){return isTransformed;};
	virtual void setTransformed(bool _isTransformed){isTransformed=_isTransformed;};
};


class rangeGate:public gate {
private:
	paramRange param;
public:
	rangeGate();
	unsigned short getType(){return RANGEGATE;}
	vector<bool> gating(flowData &);
	void extend(flowData &,float);
	void extend(float,float);
	void gain(map<string,float> &);
	void transforming(trans_local &);
	paramRange getParam(){return param;};
	vector<string> getParamNames(){return param.getNameArray();};
	void setParam(paramRange _param){param=_param;};
	void updateChannels(const CHANNEL_MAP & chnl_map){param.updateChannels(chnl_map);};
	vertices_valarray getVertices(){return param.toValarray();};
	rangeGate * clone(){return new rangeGate(*this);};
	void convertToPb(pb::gate & gate_pb);
	rangeGate(const pb::gate & gate_pb);
};

/*
 * TODO:using #include <boost/multi_array.hpp> instead to make it easier to convert to R data structure hopefully.
 *
 */
/**
 * \class polygonGate
 * \brief polygon shaped gate
 *
 * It is the most common gate type used in gating.
 */
class polygonGate:public gate {
protected:
	paramPoly param;
public:
	polygonGate();
	virtual unsigned short getType(){return POLYGONGATE;}
	virtual void extend(flowData &,float);
	void extend(float,float);
	virtual void gain(map<string,float> &);
	virtual vector<bool> gating(flowData &);
	virtual void transforming(trans_local &);
	virtual void transforming(transformation * trans_x, transformation * trans_y);
	virtual vertices_valarray getVertices(){return param.toValarray();};
	void setParam(paramPoly _param){param=_param;};
	void updateChannels(const CHANNEL_MAP & chnl_map){param.updateChannels(chnl_map);};
	virtual paramPoly getParam(){return param;};
	virtual vector<string> getParamNames(){return param.getNameArray();};
	virtual polygonGate * clone(){return new polygonGate(*this);};
	void convertToPb(pb::gate & gate_pb);
	polygonGate(const pb::gate & gate_pb);
};
/*
 * rectgate is a special polygon requires simpler gating routine
 * it doesn't overload getType member function, which means it is exposed to R
 * as a regular polygonGate
 */
/**
 * \class rectGate
 * \brief rectangle gate
 *
 * It is a special polygonGate and has the simpler(faster) gating calculation.
 */
class rectGate:public polygonGate {
public:
	vector<bool> gating(flowData &);
	unsigned short getType(){return RECTGATE;}
	rectGate * clone(){return new rectGate(*this);};
	void convertToPb(pb::gate & gate_pb);
	rectGate(const pb::gate & gate_pb);
	rectGate():polygonGate(){};
};

/**
 * \class ellipseGate
 * \brief ellipse gate
 *
 * It actually no longer needs to inherit polygonGate since we are now doing the gating
 * without interpolating it into polygon. But for backward compatibility (the legacy archive), we preserve this class definition.
 */
class ellipseGate:public polygonGate {
protected:
	vector<coordinate> antipodal_vertices; //four antipodal points of ellipse (to be deprecated)
	coordinate mu;// center point
	vector<coordinate> cov;//covariance matrix
	double dist; //size of ellipse
public:
	ellipseGate(){dist = 1;};
	ellipseGate(coordinate _mu, vector<coordinate> _cov, double _dist);
	ellipseGate(vector<coordinate> _antipodal, vector<string> _params);
	vector<bool> gating(flowData &);
	vector<coordinate> getCovarianceMat(){
		if(!Transformed())
			throw(domain_error("EllipseGate has not been transformed so covariance matrix is unavailable!"));
		return cov;};
	coordinate getMu(){
		if(!Transformed())
				throw(domain_error("EllipseGate has not been transformed so mu is unavailable!"));
		return mu;};
	double getDist(){
		if(!Transformed())
			throw(domain_error("EllipseGate has not been transformed so dist is unavailable!"));
		return dist;};
	void computeCov();
	virtual unsigned short getType(){return ELLIPSEGATE;}
	void extend(flowData &,float);
	void extend(float,float);
	void gain(map<string,float> &);
	virtual void transforming(trans_local &);
	ellipseGate * clone(){return new ellipseGate(*this);};
	void convertToPb(pb::gate & gate_pb);
	ellipseGate(const pb::gate & gate_pb);
	void toPolygon(unsigned nVertices);//ellipseGate doesn't need it , but ellipsoidGate will need it to handle the special scale (256)
};

/*
 * the purpose of having this class is to do the special scaling to the gate coordinates
 * due to the historical FlowJo's implementation (win/vX) of the ellipsoid gate that the foci, distance, and edge points are expressed in 256 x 256 display coordinates
 * to scale back to data space , for linear channel, the scaling factor is max_val/256
 * for non-linear channel, we need to
 * 1. Interpolate it to polygon
 * 2. inverse transform polygon back to raw scale
 * 3. then transform it to data scale
 * Thus we still need to preserve the inheritance to the polygonGate
 */
class ellipsoidGate:public ellipseGate {
public:
	ellipsoidGate():ellipseGate(){};
	ellipsoidGate(vector<coordinate> _antipodal, vector<string> _params);
	void transforming(trans_local &);
	ellipsoidGate * clone(){return new ellipsoidGate(*this);};
	void convertToPb(pb::gate & gate_pb);
	ellipsoidGate(const pb::gate & gate_pb);
	vector<bool> gating(flowData &);
	unsigned short getType(){return POLYGONGATE;}//expose it to R as polygonGate since the original antipodal points can't be used directly anyway
};

/*
 *.
 * And gate classes are sits in more abstract level than GatingHierarchy in the C++ class tree,
 * thus GatingHierarchy data structure should be invisible to gate.
 */
/**
 * \class boolGate
 * \brief boolean gate
 *
 * It is not the geometric gate but the boolean combination of the other reference gates.
 * So instead of defining the gating function in this class, the actual gating logic for boolGate is defined
 * in GatingHierarchy::gating function because it needs the indices from the reference nodes which are only accessible at GatingHierarchy object.
 */
class boolGate:public gate {
public:
	boolGate();
	vector<BOOL_GATE_OP> boolOpSpec;//the gatePaths with the their logical operators
public:
	vector<BOOL_GATE_OP> getBoolSpec(){return boolOpSpec;};
	unsigned short getType(){return BOOLGATE;}
	boolGate * clone(){return new boolGate(*this);};
	void convertToPb(pb::gate & gate_pb);
	boolGate(const pb::gate & gate_pb);
};
/**
 * \class logicalGate
 * \brief a special boolGate
 *
 * This is mainly used to deal with the situation where the gating algorithm (typically clustering based gating) doesn't generate any type of gate object.
 * In order still be able to record the gating results (i.e. the logical indices), this logicalGate can be used as the dummy gate to be added to the node.
 * Because nodeProperties requires a population node to have a gate to be associated with.
 *
 */
class logicalGate:public boolGate {
private:
	unsigned short getType(){return LOGICALGATE;}
	logicalGate * clone(){return new logicalGate(*this);};
	void convertToPb(pb::gate & gate_pb);
public:
	logicalGate(const pb::gate & gate_pb);
	logicalGate():boolGate(){};
};

enum QUAD{
	Q1,//-+
	Q2,//++
	Q3,//+-
	Q4//--

};
/*
 * Before interpolation, the intersection points are stored as the first element of param in polygonGate
 */
class CurlyGuadGate:public polygonGate{
	bool interpolated;
	QUAD quadrant;
public:
	CurlyGuadGate(paramPoly _inter, QUAD _quad):polygonGate(),quadrant(_quad),interpolated(false){
		param = _inter;
	};
	void transforming(trans_local & trans){
		if(interpolated)
			polygonGate::transforming(trans);
		else
			throw(logic_error("CurlyGuadGate can't not be transformed before interpolation!"));
	};
	void interpolate(trans_local & trans);
	vector<bool> gating(flowData &);
	virtual unsigned short getType(){return CURLYQUADGATE;}
	CurlyGuadGate * clone(){return new CurlyGuadGate(*this);};

};
#endif /* GATE_HPP_ */
