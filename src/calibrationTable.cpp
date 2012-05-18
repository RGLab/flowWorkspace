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
	/*
	 * c
	 */

//	double *x1=new double[x.size()];
//	double *y1=new double[x.size()];
//	for(unsigned i=0;i<x.size();i++)
//	{
//		x1[i]=x[i];
//		y1[i]=y[i];
//	}
//
//	double *b1=new double[x.size()];
//	double *c1=new double[x.size()];
//	double *d1=new double[x.size()];
//
//	switch(spline_method)
//	{
//		case NATURAL:
//		{
//			natural_spline_C(x.size(),x1, y1, b1, c1, d1);
//			break;
//		}
//
//		default:
//			throw(domain_error("other type of spline function not supported!"));
//	}
//
//
//	b=valarray<double>(b1,x.size());
//	c=valarray<double>(c1,x.size());
//	d=valarray<double>(d1,x.size());
//
//	delete x1;
//	delete y1;
//	delete b1;
//	delete c1;
//	delete d1;

	/*
	 * c++
	 */
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
//valarray<double> calibrationTable::transforming(valarray<double> & input){
//
//
//	int imeth=2;
//	int n=x.size();
//	int nu=input.size();
//	valarray<double> output(nu);
//	double *u=new double[nu];
//	double *v=new double[nu];
//
//	for(int i=0;i<nu;i++)
//	{
//		u[i]=input[i];
//	}
//
//	double *_x,* _y,*_b,*_c,*_d;
//	_x=new double[n];
//	_y=new double[n];
//	_b=new double[n];
//	_c=new double[n];
//	_d=new double[n];
//
//	for(unsigned i=0;i<n;i++)
//	{
//		_x[i]=x[i];
//		_y[i]=y[i];
//		_b[i]=b[i];
//		_c[i]=c[i];
//		_d[i]=d[i];
//	}
//
//
//
//	spline_eval_C(&imeth,&nu,u,v,&n,_x, _y, _b, _c, _d);
//
//	delete _x;
//	delete _y;
//	delete _b;
//	delete _c;
//	delete _d;
//
//	for(int i=0;i<nu;i++)
//	{
//		output[i]=v[i];
//
//	}
//	/*
//	 * small test suite
//	 */
////	double x2[2]={1,1000};
////	double y2[2];
////	int m=2,n=2,nx=x.size();
////	spline_eval(&m,&n,x2,y2,&nx,x1, y1, b1, c1, d1);
////	int dd=3;
//	//	double * output=new double[input.size()];
////	spline_eval(2,output,x.size(),x1, y1, b1, c1, d1);
//	return output;
//}


