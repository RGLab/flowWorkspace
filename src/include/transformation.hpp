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
//#define LOGICLE 1
//#define BIEXP 2

#include <map>
#include <string>
#include <vector>
#include <stdexcept>
#include "calibrationTable.hpp"
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/compare.hpp>
#include <boost/regex.hpp>
using namespace std;

struct coordinate
{
//	friend class boost::serialization::access;

	double x,y;
	template<class Archive>
						void serialize(Archive &ar, const unsigned int version)
						{


							ar & BOOST_SERIALIZATION_NVP(x) & BOOST_SERIALIZATION_NVP(y);
						}
	coordinate(double _x,double _y){x=_x;y=_y;};
	coordinate(){};
};
bool compare_x(coordinate i, coordinate j);
bool compare_y(coordinate i, coordinate j);
//typedef map<string,double> ARGLIST;


class transformation{

	friend class boost::serialization::access;

protected:
	calibrationTable calTbl;
	bool isGateOnly;
	unsigned short type;//could have been avoided if it is not required by R API getTransformation that needs to extract concrete transformation
	string name;
	string channel;
	bool isComputed;//this flag allow lazy computCalTbl/interpolation
private:
	template<class Archive>
				void serialize(Archive &ar, const unsigned int version)
				{


					ar & BOOST_SERIALIZATION_NVP(calTbl);
					ar & BOOST_SERIALIZATION_NVP(isGateOnly);
					ar & BOOST_SERIALIZATION_NVP(type);
					ar & BOOST_SERIALIZATION_NVP(name);
					ar & BOOST_SERIALIZATION_NVP(channel);
					ar & BOOST_SERIALIZATION_NVP(isComputed);
				}
public:
	transformation();
	transformation(bool _isGate,unsigned short _type);

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

typedef struct {
		string param;
		bool log;
		unsigned range;
		unsigned highValue;
		unsigned calibrationIndex;
		template<class Archive>
			void serialize(Archive &ar, const unsigned int version)
			{


				ar & BOOST_SERIALIZATION_NVP(param) & BOOST_SERIALIZATION_NVP(log) & BOOST_SERIALIZATION_NVP(range) & BOOST_SERIALIZATION_NVP(highValue) & BOOST_SERIALIZATION_NVP(calibrationIndex);
			}
		} PARAM;
typedef vector<PARAM> PARAM_VEC;

PARAM_VEC::iterator findTransFlag(PARAM_VEC & pVec, string name);

class trans_local{
	friend std::ostream & operator<<(std::ostream &os, const trans_local &gh);
	friend class boost::serialization::access;
private:
	trans_map tp;
	template<class Archive>
				void serialize(Archive &ar, const unsigned int version)
				{
					ar & BOOST_SERIALIZATION_NVP(tp);
				}
public:
	trans_map getTransMap(){return tp;};
	void setTransMap(trans_map _tp){tp=_tp;};
	transformation * getTran(string);
	trans_map cloneTransMap();
	void addTrans(string tName,transformation* trans){tp[tName]=trans;};
};

class trans_global:public trans_local{

	friend class boost::serialization::access;
private:
	string groupName;
	vector<int> sampleIDs;
	template<class Archive>
			    void serialize(Archive &ar, const unsigned int version)
			    {
        			ar & boost::serialization::make_nvp("trans_local",boost::serialization::base_object<trans_local>(*this));
					ar & BOOST_SERIALIZATION_NVP(groupName);
					ar & BOOST_SERIALIZATION_NVP(sampleIDs);

			    }
public:
	void setSampleIDs(vector<int> _sampleIDs){sampleIDs=_sampleIDs;}
	vector<int> getSampleIDs(){return sampleIDs;}
	string getGroupName(){return groupName;}
	void setGroupName(string _groupName){groupName=_groupName;};
};

typedef vector<trans_global> trans_global_vec;

class biexpTrans:public transformation{
	friend class boost::serialization::access;
public:
	int channelRange;
	double pos, neg, widthBasis, maxValue;
private:
	template<class Archive>
				void serialize(Archive &ar, const unsigned int version)
				{
					ar & boost::serialization::make_nvp("transformation",boost::serialization::base_object<transformation>(*this));

					ar & BOOST_SERIALIZATION_NVP(channelRange);
					ar & BOOST_SERIALIZATION_NVP(pos);
					ar & BOOST_SERIALIZATION_NVP(neg);
					ar & BOOST_SERIALIZATION_NVP(widthBasis);
					ar & BOOST_SERIALIZATION_NVP(maxValue);
				}

public:
	biexpTrans();
	void computCalTbl();
	biexpTrans * clone(){return new biexpTrans(*this);};

};


//class logicleTrans:public transformation{
//
//	friend class boost::serialization::access;
//private:
//	int channelRange;
//	double pos, neg, widthBasis, maxValue;
//
//	template<class Archive>
//				void serialize(Archive &ar, const unsigned int version)
//				{
//					ar & boost::serialization::make_nvp("transformation",boost::serialization::base_object<transformation>(*this));
//
//					ar & BOOST_SERIALIZATION_NVP(channelRange);
//					ar & BOOST_SERIALIZATION_NVP(pos);
//					ar & BOOST_SERIALIZATION_NVP(neg);
//					ar & BOOST_SERIALIZATION_NVP(widthBasis);
//					ar & BOOST_SERIALIZATION_NVP(maxValue);
//				}
//
//
//public:
//	logicleTrans * clone(){return new logicleTrans(*this);};
//};

/*
 * TODO:right now set two flags to TRUE in the contructor to avoid doing cal table stuff,
 * we should consider redesign the classes so that logTrans does not share this extra feature from parent class
 */
class logTrans:public transformation{

	friend class boost::serialization::access;
private:
		template<class Archive>
					void serialize(Archive &ar, const unsigned int version)
					{
						ar & boost::serialization::make_nvp("transformation",boost::serialization::base_object<transformation>(*this));
						ar & BOOST_SERIALIZATION_NVP(offset);
						ar & BOOST_SERIALIZATION_NVP(decade);
					}
public:
		double offset;
		double decade;
public:
	logTrans();
	logTrans(double _offset,double _decade);
	double flog(double x,double _max,double _min);
	void transforming(valarray<double> & input);
	logTrans * clone(){return new logTrans(*this);};
};


class linTrans:public transformation{
friend class boost::serialization::access;
        private:
                        template<class Archive>
                                                void serialize(Archive &ar, const unsigned int version)
                                                {
                                                        ar & boost::serialization::make_nvp("transformation",boost::serialization::base_object<transformation>(*this));

                                                }

public:
        linTrans();
        void transforming(valarray<double> & input);
        linTrans * clone(){return new linTrans(*this);};
};

/*
 * This class is dedicated to scale the EllipsoidGate
 */
class scaleTrans:public linTrans{
friend class boost::serialization::access;
	private:
				template<class Archive>
				void serialize(Archive &ar, const unsigned int version)
				{
						ar & boost::serialization::make_nvp("linTrans",boost::serialization::base_object<transformation>(*this));
						ar & BOOST_SERIALIZATION_NVP(scale_factor);
				}

	float scale_factor;

public:
	scaleTrans();
	scaleTrans(float _scale_factor);
	void transforming(valarray<double> & input);
	scaleTrans * clone(){return new scaleTrans(*this);};
};


class flinTrans:public transformation{
	double min;
	double max;
friend class boost::serialization::access;
	private:
			template<class Archive>
						void serialize(Archive &ar, const unsigned int version)
						{
							ar & boost::serialization::make_nvp("transformation",boost::serialization::base_object<transformation>(*this));
							ar & BOOST_SERIALIZATION_NVP(min);
							ar & BOOST_SERIALIZATION_NVP(max);
						}

public:
	flinTrans();
	flinTrans(double _minRange, double _maxRange);
	double flin(double x);
	void transforming(valarray<double> & input);
	flinTrans * clone(){return new flinTrans(*this);};
};


#endif /* TRANSFORMATION_HPP_ */
