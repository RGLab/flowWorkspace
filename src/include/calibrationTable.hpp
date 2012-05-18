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

struct Spline_Coefs{
	map<string,vector<double> > coefs;
	int method;
	string type;
};

class calibrationTable{

public:
	int spline_method;
	string caltype;
	/*
	 * TODO:try boost::tokenizer/boost::split and boost::lexical_cast
	 * or stream and getline to parse it into double array
	 */

	valarray<double> x,y,b,c,d;
	bool isInterpolated;
public:
	calibrationTable();
	calibrationTable(string _caltype,int _spline_method);
	void interpolate();
	void init(unsigned);
	valarray<double> transforming(valarray<double> &);
	Spline_Coefs getCalTbl();
};


#endif /* CALIBRATIONTABLE_HPP_ */
