/*
 * transformation.cpp
 *
 *  Created on: Apr 24, 2012
 *      Author: wjiang2
 */

#include "include/transformation.hpp"
#include <fstream>

void calibrationTable::init(unsigned nSize){
	x.resize(nSize);
	y.resize(nSize);
	b.resize(nSize);
	c.resize(nSize);
	d.resize(nSize);

}

void calibrationTable::interpolate(){

	/*
	 * c
	 */
//
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
//	natural_spline_C(x.size(),x1, y1, b1, c1, d1);
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
	natural_spline(x, y, b, c, d);

}
/*
 * API provided for Rcpp to access calibration table
 */
Spline_Coefs calibrationTable::getCalTbl(){
	Spline_Coefs res;
	unsigned nX=x.size();
	unsigned nB=b.size();
	vector<double> _x(nX),_y(nX),_b(nB),_c(nB),_d(nB);
	for(unsigned i=0;i<nX;i++)
	{
		_x[i]=x[i];
		_y[i]=y[i];
	}
	for(unsigned i=0;i<nB;i++)
	{
		_b[i]=b[i];
		_c[i]=c[i];
		_d[i]=d[i];
	}
	res["x"]=_x;
	res["y"]=_y;
	res["b"]=_b;
	res["c"]=_c;
	res["d"]=_d;
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
//
//
//
//	spline_eval_C(&imeth,&nu,u,v,&n,x1, y1, b1, c1, d1);
//
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
