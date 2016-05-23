/*
 * transformation.hpp
 *
 *  Created on: Apr 24, 2012
 *      Author: wjiang2
 */

#ifndef TRANSFORMATION_HPP_
#define TRANSFORMATION_HPP_

#define CALTBL 0
#define LOG 1
#define LIN 2
#define FLIN 3
#define FASINH 4
//#define LOGICLE 1
#define BIEXP 5

#include <map>
#include <string>
#include <vector>
#include <stdexcept>
#include "calibrationTable.hpp"
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/compare.hpp>
#include <boost/regex.hpp>

#include "global.hpp"

using namespace std;

struct coordinate
{
//	friend class boost::serialization::access;

	double x,y;
	coordinate(double _x,double _y){x=_x;y=_y;};
	coordinate(){};
	void convertToPb(pb::coordinate & coor_pb){
		coor_pb.set_x(x);
		coor_pb.set_y(y);
	};
	coordinate(const pb::coordinate & coor_pb):x(coor_pb.x()),y(coor_pb.y()){};
};
bool compare_x(coordinate i, coordinate j);
bool compare_y(coordinate i, coordinate j);
//typedef map<string,double> ARGLIST;


class transformation{
protected:
	calibrationTable calTbl; //no longer have to store calTbl since we now can compute it from biexp even for mac workspace
	bool isGateOnly;
	unsigned short type;//could have been avoided if it is not required by R API getTransformation that needs to extract concrete transformation
	string name;
	string channel;
	bool isComputed;//this flag allow lazy computCalTbl/interpolation
public:
	transformation();
	transformation(bool _isGate,unsigned short _type);
	virtual ~transformation(){};
	virtual void transforming(valarray<double> & input);
	virtual void computCalTbl(){};//dummy routine that does nothing
	virtual Spline_Coefs getSplineCoefs(){return calTbl.getSplineCoefs();};
	virtual void setCalTbl(calibrationTable _tbl);
	virtual calibrationTable getCalTbl(){return calTbl;};
	virtual void interpolate(){calTbl.interpolate();};
	virtual bool isInterpolated(){return calTbl.isInterpolated();}
	virtual bool gateOnly(){return isGateOnly;};
	virtual void setGateOnlyFlag(bool _flag){isGateOnly=_flag;};
	virtual bool computed(){return isComputed;};
	virtual void setComputeFlag(bool _flag){isComputed=_flag;};
	virtual string getName(){return name;};
	virtual void setName(string _name){name=_name;};
	virtual string getChannel(){return channel;};
	virtual void setChannel(string _channel){channel=_channel;};
	virtual unsigned short getType(){return type;};
	virtual void setType(unsigned short _type){type=_type;};
	virtual transformation * clone(){return new transformation(*this);};
	virtual void convertToPb(pb::transformation & trans_pb);
	transformation(const pb::transformation & trans_pb);
	virtual boost::shared_ptr<transformation> getInverseTransformation();
	virtual void setTransformedScale(int scale){throw(domain_error("setTransformedScale function not defined!"));};
	virtual int getTransformedScale(){throw(domain_error("getTransformedScale function not defined!"));};
	virtual int getRawScale(){throw(domain_error("getRawScale function not defined!"));};
};
/* case insensitive compare predicate*/
struct ciLessBoost : std::binary_function<std::string, std::string, bool>
{
    bool operator() (const std::string & s1, const std::string & s2) const {
        return lexicographical_compare(s1, s2, boost::is_iless());
    }
};

typedef map<string,transformation *, ciLessBoost> trans_map;/* we always do case-insensitive searching for transformation lookup
due to some of channel name discrepancies occured in flowJo workspaces*/
typedef std::map<std::string, std::string, ciLessBoost> CHANNEL_MAP;
struct PARAM{
		string param;
		bool log;
		unsigned range;
		unsigned highValue;
		unsigned calibrationIndex;
		//EDIT: can't trust this info from xml
//		double timestep;//only meaningful for time channel which is used to scale time channel (only for data, not for gates since gates are already stored at scaled value)
		PARAM(){};
		void updateChannels(const CHANNEL_MAP & chnl_map){
			CHANNEL_MAP::const_iterator itChnl = chnl_map.find(param);
			if(itChnl!=chnl_map.end())
				param = itChnl->second;
		};
		PARAM(const pb::PARAM & param_pb){
			param = param_pb.param();
			log = param_pb.log();
			range = param_pb.range();
			highValue = param_pb.highvalue();
			calibrationIndex = param_pb.calibrationindex();
//			timestep = param_pb.timestep();
		};
		 void convertToPb(pb::PARAM & param_pb){
			 param_pb.set_param(param);
			 param_pb.set_log(log);
			 param_pb.set_range(range);
			 param_pb.set_highvalue(highValue);
			 param_pb.set_calibrationindex(calibrationIndex);
//			 param_pb.set_timestep(timestep);
		 };
		};
typedef vector<PARAM> PARAM_VEC;

PARAM_VEC::const_iterator findTransFlag(const PARAM_VEC & pVec, const string & name, const string & , const string & );

class trans_local{
private:
	trans_map tp;
public:
	trans_map getTransMap(){return tp;};
	void setTransMap(trans_map _tp){tp=_tp;};
	transformation * getTran(string);
	trans_map cloneTransMap();
	void addTrans(string tName,transformation* trans){tp[tName]=trans;};
	virtual void convertToPb(pb::trans_local & lg_pb, pb::GatingSet & gs_pb);
	trans_local(){};
	trans_local(const pb::trans_local & tg_pb, map<intptr_t, transformation *> & trans_tbl);
	virtual void convertToPb(pb::trans_local & lg_pb);
	void updateChannels(const CHANNEL_MAP & chnl_map);
};

class trans_global:public trans_local{
private:
	string groupName;
	vector<int> sampleIDs;
public:
	void setSampleIDs(vector<int> _sampleIDs){sampleIDs=_sampleIDs;}
	vector<int> getSampleIDs(){return sampleIDs;}
	string getGroupName(){return groupName;}
	void setGroupName(string _groupName){groupName=_groupName;};
	void convertToPb(pb::trans_local & tg_pb, pb::GatingSet & gs_pb);
	trans_global(){};
	trans_global(const pb::trans_local & tg_pb, map<intptr_t, transformation *> & trans_tbl);

};

typedef vector<trans_global> trans_global_vec;

class biexpTrans:public transformation{
public:
	int channelRange;
	double pos, neg, widthBasis, maxValue;
public:
	biexpTrans();
	void computCalTbl();
	biexpTrans * clone(){return new biexpTrans(*this);};
	void convertToPb(pb::transformation & trans_pb);
	biexpTrans(const pb::transformation & trans_pb);
	void setTransformedScale(int scale){
		channelRange = scale;
		//recompute cal table
		computCalTbl();
		interpolate();
	};
	int getTransformedScale(){return channelRange;};
	int getRawScale(){return maxValue;};

};

class fasinhTrans:public transformation{
public:
	double maxRange;//unused at this moment
	double length,T, A, M;
public:
	fasinhTrans();
	fasinhTrans(double , double , double , double , double );
	virtual void transforming(valarray<double> & input);
	fasinhTrans * clone(){return new fasinhTrans(*this);};
	void convertToPb(pb::transformation & trans_pb);
	fasinhTrans(const pb::transformation & trans_pb);
	boost::shared_ptr<transformation> getInverseTransformation();
	void setTransformedScale(int scale){length = scale;};
	int getTransformedScale(){return length;};
	int getRawScale(){return T;};
};
/*
 * inverse transformation of fasinhTrans
 */
class fsinhTrans:public fasinhTrans{

public:
	fsinhTrans();
	fsinhTrans(double , double , double , double , double );
	void transforming(valarray<double> & input);
	boost::shared_ptr<transformation> getInverseTransformation(){throw(domain_error("inverse function not defined!"));};
//	fsinhTrans * clone(){return new fasinhTrans(*this);};
//	void convertToPb(pb::transformation & trans_pb);
//	fasinhTrans(const pb::transformation & trans_pb);
//	boost::shared_ptr<transformation> getInverseTransformation();
//	void setTransformedScale(int scale){length = scale;};
};

/*
 * TODO:right now set two flags to TRUE in the contructor to avoid doing cal table stuff,
 * we should consider redesign the classes so that logTrans does not share this extra feature from parent class
 */
class logTrans:public transformation{
public:
		double offset;
		double decade;
		unsigned scale;
		unsigned T; //top value; derived from keyword $PnR for each channel
public:
	logTrans();//deprecated
	logTrans(double _offset,double _decade, unsigned T, unsigned _scale);
	double flog(double x,double _max,double _min);
	void transforming(valarray<double> & input);
	logTrans * clone(){return new logTrans(*this);};
	void convertToPb(pb::transformation & trans_pb);
	logTrans(const pb::transformation & trans_pb);

	boost::shared_ptr<transformation> getInverseTransformation();
	void setTransformedScale(int _scale){scale = _scale;};
	int getTransformedScale(){return scale;};
	int getRawScale(){return T;};
};

class logInverseTrans:public logTrans{
public:
	logInverseTrans(double _offset,double _decade, unsigned _T, unsigned _scale):logTrans(_offset, _decade, _T, _scale){};
	void transforming(valarray<double> & input);

};


class linTrans:public transformation{
public:
        linTrans();
        void transforming(valarray<double> & input);
        linTrans * clone(){return new linTrans(*this);};
        void convertToPb(pb::transformation & trans_pb);
        linTrans(const pb::transformation & trans_pb);
        boost::shared_ptr<transformation> getInverseTransformation(){throw(domain_error("inverse function not defined!"));};
        void setTransformedScale(int scale){throw(domain_error("setTransformedScale function not defined!"));};

};

/*
 * This class is dedicated to scale the EllipsoidGate
 */
class scaleTrans:public linTrans{
	int t_scale; //transformed scale
	int r_scale; // raw scale

public:
	scaleTrans();
	scaleTrans(int,int);
	void transforming(valarray<double> & input);
	scaleTrans * clone(){return new scaleTrans(*this);};
	boost::shared_ptr<transformation> getInverseTransformation();
	void setTransformedScale(int _scale){t_scale = _scale;};

};



class flinTrans:public transformation{
	double min;
	double max;
public:
	flinTrans();
	flinTrans(double _minRange, double _maxRange);
	double flin(double x);
	void transforming(valarray<double> & input);
	flinTrans * clone(){return new flinTrans(*this);};
	void convertToPb(pb::transformation & trans_pb);
	flinTrans(const pb::transformation & trans_pb);
	boost::shared_ptr<transformation> getInverseTransformation(){throw(domain_error("inverse function not defined!"));};
	void setTransformedScale(int scale){throw(domain_error("setTransformedScale function not defined!"));};

};


#endif /* TRANSFORMATION_HPP_ */
