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
#include <boost/graph/adj_list_serialize.hpp>
#include <boost/foreach.hpp>
#include "GatingSet.pb.h"

struct Spline_Coefs{
	map<string,vector<double> > coefs;
	int method;
	string type;//to be deprecated
};

class calibrationTable{
	friend std::ostream & operator<<(std::ostream &os, const calibrationTable &gh);
private:
	valarray<double> x,y,b,c,d;
	int spline_method;
	string caltype;//TODO:move this to transformation class
	bool flag;
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
	void convertToPb(pb::calibrationTable & cal_pb);
	calibrationTable(const pb::calibrationTable & cal_pb);
};


#endif /* CALIBRATIONTABLE_HPP_ */
