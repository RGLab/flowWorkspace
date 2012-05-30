/*
 * calibrationTable.cpp
 *
 *  Created on: May 14, 2012
 *      Author: wjiang2
 */

#include "include/calibrationTable.hpp"
#include <fstream>
calibrationTable::calibrationTable(){
	isInterpolated=false;
}

calibrationTable::calibrationTable(string _caltype,int _spline_method){
//								type=CALTBL;
		caltype=_caltype;
		spline_method=_spline_method;
		isInterpolated=false;
};

void calibrationTable::init(unsigned nSize){
	x.resize(nSize);
	y.resize(nSize);
	b.resize(nSize);
	c.resize(nSize);
	d.resize(nSize);

}

void calibrationTable::interpolate(){

//	cout<<"entering interpolate"<<endl;


	if(!isInterpolated)
	{
		natural_spline(x, y, b, c, d);
		isInterpolated=true;
	}


}
/*
 * API provided for Rcpp to access calibration table
 */
Spline_Coefs calibrationTable::getCalTbl(){

	map<string,vector<double> > coefs;
	unsigned nX=x.size();

	vector<double> _x(nX),_y(nX),_b(nX),_c(nX),_d(nX);
	for(unsigned i=0;i<nX;i++)
	{
		_x[i]=x[i];
		_y[i]=y[i];
		_b[i]=b[i];
		_c[i]=c[i];
		_d[i]=d[i];
	}

	coefs["x"]=_x;
	coefs["y"]=_y;
	coefs["b"]=_b;
	coefs["c"]=_c;
	coefs["d"]=_d;

	Spline_Coefs res;
	res.coefs=coefs;
	res.method=spline_method;
	res.type=caltype;

	return res;
}
valarray<double> calibrationTable::transforming(valarray<double> & input){


	int imeth=2;
	int nu=input.size();
	valarray<double> output(nu);

	spline_eval(imeth,input,output,x, y, b, c, d);
	return output;
}



