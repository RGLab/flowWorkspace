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

	x1=new double[x.size()];
	y1=new double[x.size()];
	for(unsigned i=0;i<x.size();i++)
	{
		x1[i]=x[i];
		y1[i]=y[i];
	}

	b1=new double[x.size()];
	c1=new double[x.size()];
	d1=new double[x.size()];

	natural_spline(x.size(),x1, y1, b1, c1, d1);

/*
 * small test suite
 */
//	double x1[6]={-7.390373 ,-7.326289, -7.262243, -7.198235, -7.134263 ,-7.070327};
//	double y1[6]={0 ,1 ,2, 3, 4 ,5};
//	double b1[6],c1[6],d1[6];
//	natural_spline(6,x1, y1, b1, c1, d1);



	b=valarray<double>(b1,x.size());
	c=valarray<double>(c1,x.size());
	d=valarray<double>(d1,x.size());
}

valarray<double> calibrationTable::transforming(valarray<double> & input){


	int imeth=2;
	int n=x.size();
	int nu=input.size();
	valarray<double> output(nu);
	double *u=new double[nu];
	double *v=new double[nu];

	for(int i=0;i<nu;i++)
	{
		u[i]=input[i];
	}




	spline_eval(&imeth,&nu,u,v,&n,x1, y1, b1, c1, d1);

//	ofstream xyOutput("../output/c++/xy.csv");
//	xyOutput<<"x,y,b,c,d"<<endl;
//	for(int i=0;i<n;i++)
//	{
//		xyOutput<<x1[i]<<","<<y1[i]<<","<<b1[i]<<","<<c1[i]<<","<<d1[i]<<endl;;
//
//	}
//
//	ofstream uvOutput("../output/c++/uv.csv");
//	uvOutput<<"u,v"<<endl;
//	for(int i=0;i<nu;i++)
//	{
//		uvOutput<<u[i]<<","<<v[i]<<endl;;
//
//	}


	for(int i=0;i<nu;i++)
	{
		output[i]=v[i];
//		cout<<v[i]<<",";
	}
	/*
	 * small test suite
	 */
//	double x2[2]={1,1000};
//	double y2[2];
//	int m=2,n=2,nx=x.size();
//	spline_eval(&m,&n,x2,y2,&nx,x1, y1, b1, c1, d1);
//	int dd=3;
	//	double * output=new double[input.size()];
//	spline_eval(2,output,x.size(),x1, y1, b1, c1, d1);
	return output;
}
