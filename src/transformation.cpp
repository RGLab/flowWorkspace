/*
 * transformation.cpp
 *
 *  Created on: Apr 24, 2012
 *      Author: wjiang2
 */

#include "include/transformation.hpp"
bool compare_x(coordinate i, coordinate j) { return i.x<j.x; }
bool compare_y(coordinate i, coordinate j) { return i.y<j.y; }

PARAM_VEC::iterator findTransFlag(PARAM_VEC & pVec, string name){
	PARAM_VEC::iterator it;
	for(it=pVec.begin();it!=pVec.end();it++)
	{
		if(it->param.compare(name)==0)
			break;
	}
	return it;
}
trans_map trans_local::cloneTransMap(){

	trans_map res;
	/*
	 * clone trans map
	 */

	for(trans_map::iterator it=tp.begin();it!=tp.end();it++)
	{
		transformation * curTran=it->second;
		if(curTran!=NULL)
		{
			COUT<<"cloning transformatioin:"<<curTran->getChannel()<<endl;
			res[it->first]=curTran->clone();
		}
	}
	return res;
}


/*
 * transformation
 */


/*
	 * if it is pure transformation object,then assume calibration is directly read from ws
	 * so there is no need to compute calibration
	 */

transformation::transformation():isGateOnly(false),type(CALTBL),isComputed(true){}
transformation::transformation(bool _isGate, unsigned short _type):isGateOnly(_isGate),type(_type),isComputed(true){}
logTrans::logTrans():transformation(false,LOG),offset(0),decade(1){
	calTbl.setInterpolated(true);
}
logTrans::logTrans(double _offset,double _decade):transformation(false,LOG),offset(_offset),decade(_decade){
	calTbl.setInterpolated(true);
}

linTrans::linTrans():transformation(true,LIN){
	calTbl.setInterpolated(true);
}
flinTrans::flinTrans():transformation(false,FLIN),min(0),max(0){
	calTbl.setInterpolated(true);
}
flinTrans::flinTrans(double _minRange, double _maxRange):transformation(false,FLIN),min(_minRange),max(_maxRange){
	calTbl.setInterpolated(true);
}
/*
 *
 *now we switch back to zero imputation instead of min value since
 *when convert to R version of transformation function, the data is
 *no available anymore, thus no way to specify this minvalue
 *
 */
double logTrans::flog(double x,double T,double _min) {

	double M=decade;
	return x>0?(log10(x/T)/M+offset):_min;
//	return x>0?(log10((x+offset)/T)/M):_min;

}
/*
 * these transforming functions change the input data
 */

void logTrans::transforming(valarray<double> & input){


		double thisMax=262144;//input.max();
		double thisMin=0;//input.min();

		for(unsigned i=0;i<input.size();i++){
			input[i]=flog(input[i],thisMax,thisMin);
		}


//		input=log10(input);

}
double flinTrans::flin(double x){
	double T=max;
	double A=min;
	return (x+A)/(T+A);
}
void linTrans::transforming(valarray<double> & input){

		input*=64;
}

void flinTrans::transforming(valarray<double> & input){

	for(unsigned i=0;i<input.size();i++){
		input[i]=flin(input[i]);
	}

}
void transformation::transforming(valarray<double> & input){
		if(!calTbl.isInterpolated()){
			 /* calculate calibration table from the function
			 */
			if(!computed())
			{

				COUT<<"computing calibration table..."<<endl;
				computCalTbl();
			}

			if(!isInterpolated())
			{

				COUT<<"spline interpolating..."<<endl;
				interpolate();
			}
		}

		input=calTbl.transforming(input);

}

void transformation::setCalTbl(calibrationTable _tbl){
	calTbl=_tbl;
}
transformation * trans_local::getTran(string channel){
	transformation * res;
	if(channel.compare("Time")==0||channel.compare("time")==0)
		res=NULL;


	trans_map::iterator it=tp.find(channel);
	if(it==tp.end())
		res=NULL;
	else
		res=it->second;

	return res;
}


biexpTrans::biexpTrans(){
	channelRange=4096;
	maxValue=262144;
	pos=4.5;
	neg=0;
	widthBasis=-10;
	isComputed=false;

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
	calTbl.setCaltype("flowJo");
	calTbl.setMethod(2);
	calTbl.init(channelRange+1);
	valarray<double> x(channelRange+1),y(channelRange+1);
	for (int chan = 0; chan <= channelRange; chan++)
	{
		y[chan] =chan;
		x[chan] = positive[chan];
	}
	calTbl.setX(x);
	calTbl.setY(y);

	isComputed=true;

	delete[] positive;
	delete[] negative;

}

