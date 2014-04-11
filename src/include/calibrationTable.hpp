/*
 * calibrationTable.hpp
 *
 *  Created on: May 14, 2012
 *      Author: wjiang2
 */

#ifndef CALIBRATIONTABLE_HPP_
#define CALIBRATIONTABLE_HPP_

#include <map>
#include <string>
#include <vector>
#include <stdexcept>
#include "spline.hpp"
using namespace std;
#include <boost/config.hpp>
#include <boost/archive/tmpdir.hpp>
#include <boost/archive/binary_iarchive.hpp>
#include <boost/archive/binary_oarchive.hpp>
#include <boost/serialization/base_object.hpp>
#include <boost/serialization/utility.hpp>
#include <boost/serialization/list.hpp>
#include <boost/serialization/map.hpp>
#include <boost/serialization/vector.hpp>
#include <boost/serialization/valarray.hpp>
#include <boost/graph/adj_list_serialize.hpp>
#include <boost/serialization/assume_abstract.hpp>

struct Spline_Coefs{
	map<string,vector<double> > coefs;
	int method;
	string type;
};

class calibrationTable{
	friend std::ostream & operator<<(std::ostream &os, const calibrationTable &gh);
	friend class boost::serialization::access;
private:
	valarray<double> x,y,b,c,d;
	int spline_method;
	string caltype;//TODO:move this to transformation class
	bool flag;
	template<class Archive>
					void serialize(Archive &ar, const unsigned int version)
					{


						ar & BOOST_SERIALIZATION_NVP(x);
						ar & BOOST_SERIALIZATION_NVP(y);
						ar & BOOST_SERIALIZATION_NVP(b);
						ar & BOOST_SERIALIZATION_NVP(c);
						ar & BOOST_SERIALIZATION_NVP(d);
						ar & BOOST_SERIALIZATION_NVP(spline_method);
						ar & BOOST_SERIALIZATION_NVP(caltype);
						ar & BOOST_SERIALIZATION_NVP(flag);
					}

public:
	calibrationTable();
	calibrationTable & operator=(const calibrationTable& source);
	calibrationTable(string _caltype,int _spline_method);
	void interpolate();
	void init(unsigned);
	valarray<double> transforming(valarray<double> &);
	Spline_Coefs getSplineCoefs();
	valarray<double> getX(){return x;};
	valarray<double> getY(){return y;};
	void setY(valarray<double> _y){
			y.resize(_y.size());
			y=_y;
			};
	void setX(valarray<double> _x){
				x.resize(_x.size());
				x=_x;
				};
	valarray<double> getB(){return b;};
	valarray<double> getC(){return c;};
	valarray<double> getD(){return d;};
	void setCaltype(string _caltype){caltype=_caltype;};
	string getCaltype(){return caltype;};
	void setMethod(int _spline_method){spline_method=_spline_method;};
	int getMethod(){return spline_method;};
	void setInterpolated(bool _flag){flag=_flag;};
	bool isInterpolated(){return flag;}
};


#endif /* CALIBRATIONTABLE_HPP_ */
