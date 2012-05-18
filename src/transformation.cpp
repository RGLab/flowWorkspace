/*
 * transformation.cpp
 *
 *  Created on: Apr 24, 2012
 *      Author: wjiang2
 */

#include "include/transformation.hpp"

/*
 * transformation
 */
transformation::~transformation(){
	if(calTbl!=NULL)
		delete calTbl;
}

valarray<double> transformation::transforming(valarray<double> & input){
		if(calTbl==NULL)
			throw(domain_error("calibration table not assigned yet!"));
		return calTbl->transforming(input);
}
void transformation::computCalTbl(){
	throw(domain_error("calibration table can not be computed in abstract tranformation function!"));
}

transformation * trans_local::getTran(string channel){
	if(channel.compare("Time")==0||channel.compare("time")==0)
		return NULL;
	transformation * res;
	res=transformations[channel];
	/*
	 * try generic trans when channel specific trans not found
	 */
//	if(res==NULL)
//		res=transformations["*"];
	return res;
}
/*
 *biexpTrans
 */
biexpTrans::biexpTrans(){
	channelRange=4096;
	maxValue=262144;
	pos=4.5;
	neg=0;
	widthBasis=-10;
}
/*
 * directly translated from java routine from tree star
 */
 double logRoot(double b, double w)
{
	double xLo = 0;
	double xHi = b;
	double d = (xLo + xHi) / 2;
	double dX = abs((long) (xLo - xHi));
	double dXLast = dX;
	double fB = -2 * log(b) + w * b;
	double f = 2. * log(d) + w * b + fB;
	double dF = 2 / d + w;
	if (w == 0) return b;
	for (long i = 0; i < 100; i++)
	{
		if (((d - xHi) * dF - f) * ((d - xLo) * dF - f) >= 0 ||
				abs((long) (2 * f)) > abs((long) (dXLast * dF)))
		{
			dX = (xHi - xLo) / 2;
			d = xLo + dX;
			if (d == xLo)
				return d;
		}
		else
		{
			dX = f / dF;
			double t = d;
			d -= dX;
			if (d == t)
				return d;
		}
		if (abs((long) dX) < 1.0e-12)
			return d;
		dXLast = dX;
		f = 2 * log(d) + w * d + fB;
		dF = 2 / d + w;
		if (f < 0) xLo = d;
		else xHi = d;
	}
	return d;
}
 /*
  * directly translated from java routine from tree star
  */

void logInterpolate(double *f, int i, long n, double x)
{
	double minVal = f[i];
	double maxVal = x;
	double logMinVal = log(minVal);
	double logMaxVal = log(maxVal);
	for (int j = i; j < n; j++)
	{
		float frxn = (float)(j - i) / (float)(n - i);
		double curVal = frxn * (logMaxVal - logMinVal) + logMinVal;
		f[j] = exp(curVal);
	}
}
void biexpTrans::computCalTbl(){
	/*
	 * directly translated from java routine from tree star
	 */

	double ln10 = log(10.0);
	double decades = pos;
	double lowScale = widthBasis;
	double width = log10(-lowScale);

	if (width < 0.5 || width > 3) width = 0.5;
	decades -= width / 2;
	double extra = neg;
	if (extra < 0) extra = 0;
	extra += width / 2;

	int zeroChan = (int)(extra * channelRange / (extra + decades));
	zeroChan = min(zeroChan, channelRange / 2);

	if (zeroChan > 0) decades = extra * channelRange / zeroChan;
	width /= 2 * decades;        // 1.1

	double maximum = maxValue;
	double positiveRange = ln10 * decades;
	double minimum = maximum / exp(positiveRange);
	double negativeRange = logRoot(positiveRange, width);

	double *positive = new double[channelRange + 1];
	double *negative = new double[channelRange + 1];
	positive[0] = negative[0] = 1.;
	logInterpolate(positive, 0, channelRange + 1, exp(positiveRange));
	logInterpolate(negative, 0, channelRange + 1, exp(-negativeRange));

	double s = exp((positiveRange + negativeRange) * (width + extra / decades));
	int j;
	for (j = 0; j < channelRange + 1; j++)
		negative[j] *= s;
	s = positive[zeroChan] - negative[zeroChan];
	for (j = zeroChan; j < channelRange + 1; j++)
		positive[j] = minimum * (positive[j] - negative[j] - s);
	for (j = 0; j < zeroChan; j++)
		positive[j] = -positive[2 * zeroChan - j];

	/*
	 * save the calibration table
	 */
	calTbl=new calibrationTable("flowJo",2);
	calTbl->init(channelRange+1);

	for (int chan = 0; chan <= channelRange; chan++)
	{
		calTbl->y[chan] =chan;
		calTbl->x[chan] = positive[chan];
	}


//	if(dMode>=GATING_SET_LEVEL)
//		cout<<"spline interpolating..."<<name<<endl;

//	calTbl->interpolate();
}
