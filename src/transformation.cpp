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
			if(g_loglevel>=POPULATION_LEVEL)
				COUT<<"cloning transformatioin:"<<curTran->getChannel()<<endl;
			res[it->first]=curTran->clone();
		}
	}
	return res;
}

void trans_local::convertToPb(pb::trans_local & lg_pb){

	BOOST_FOREACH(trans_map::value_type & it, tp){
		intptr_t address = (intptr_t)it.second;
		pb::trans_pair * tp = lg_pb.add_tp();
		tp->set_name(it.first);
		tp->set_trans_address(address);
	}
}
void trans_local::convertToPb(pb::trans_local & lg_pb, pb::GatingSet & gs_pb){
	// save  address vs name pair and address(global) is to be referred in gh
	convertToPb(lg_pb);

	//save it to global mapping (address vs trans obj)
	BOOST_FOREACH(trans_map::value_type & it, tp){
			intptr_t address = (intptr_t)it.second;
			pb::TRANS_TBL * tb = gs_pb.add_trans_tbl();
			tb->set_trans_address(address);
			pb::transformation * trans_pb = tb->mutable_trans();
			transformation * trans = it.second;
			trans->convertToPb(*trans_pb);
		}
}
trans_local::trans_local(const pb::trans_local & lg_pb, map<intptr_t, transformation *> & trans_tbl){

	for(int i = 0; i < lg_pb.tp_size(); i ++){
		const pb::trans_pair & tp_pb = lg_pb.tp(i);
		intptr_t old_address = (intptr_t)tp_pb.trans_address();
		//look up from the tbl for the new pointer
		map<intptr_t, transformation *>::iterator it = trans_tbl.find(old_address);
		if(it!=trans_tbl.end()){
			tp[tp_pb.name()] = it->second;
		}
		else
			throw(domain_error("the current archived transformation is not found in the global table!"));

	}
}
void trans_global::convertToPb(pb::trans_local & tg_pb, pb::GatingSet & gs_pb){
		trans_local::convertToPb(tg_pb, gs_pb);//pass gs_pb on to the base method
		tg_pb.set_groupname(groupName);
		BOOST_FOREACH(vector<int>::value_type & it,sampleIDs){
			tg_pb.add_sampleids(it);
		}

}
trans_global::trans_global(const pb::trans_local & tg_pb, map<intptr_t, transformation *> & trans_tbl):trans_local(tg_pb, trans_tbl){
	groupName = tg_pb.groupname();
	for(int i = 0; i < tg_pb.sampleids_size(); i++)
		sampleIDs.push_back(tg_pb.sampleids(i));

}
/*
 * transformation
 */
transformation::transformation(const pb::transformation & trans_pb){
	isComputed = trans_pb.iscomputed();
	isGateOnly = trans_pb.isgateonly();
	type = trans_pb.type();
	name = trans_pb.name();
	channel = trans_pb.channel();
	calTbl = calibrationTable(trans_pb.caltbl());
}
void transformation::convertToPb(pb::transformation & trans_pb){

	trans_pb.set_isgateonly(isGateOnly);
	trans_pb.set_type(type);
	trans_pb.set_name(name);
	trans_pb.set_channel(channel);
	//skip saving calibration table to save disk space, which means it needs to be always recalculated when load it back
	if(type == BIEXP)
		trans_pb.set_iscomputed(false);
	else
	{
		trans_pb.set_iscomputed(isComputed);
		pb::calibrationTable * cal_pb = trans_pb.mutable_caltbl();
		calTbl.convertToPb(*cal_pb);
	}



}
void biexpTrans::convertToPb(pb::transformation & trans_pb){
	transformation::convertToPb(trans_pb);
	trans_pb.set_trans_type(pb::PB_BIEXP);
	pb::biexpTrans * bt_pb = trans_pb.mutable_bt();
	bt_pb->set_channelrange(channelRange);
	bt_pb->set_maxvalue(maxValue);
	bt_pb->set_neg(neg);
	bt_pb->set_pos(pos);
	bt_pb->set_widthbasis(widthBasis);
}
biexpTrans::biexpTrans(const pb::transformation & trans_pb):transformation(trans_pb){
	if(!trans_pb.has_bt())
		throw(domain_error("biexpTrans field not found in pb::transformation!"));
	const pb::biexpTrans & bt_pb = trans_pb.bt();

	channelRange = bt_pb.channelrange();
	maxValue = bt_pb.maxvalue();
	neg = bt_pb.neg();
	pos = bt_pb.pos();
	widthBasis = bt_pb.widthbasis();
}
void logTrans::convertToPb(pb::transformation & trans_pb){
	transformation::convertToPb(trans_pb);
	trans_pb.set_trans_type(pb::PB_LOG);
	pb::logTrans * lt_pb = trans_pb.mutable_lt();
	lt_pb->set_decade(decade);
	lt_pb->set_offset(offset);
}
logTrans::logTrans(const pb::transformation & trans_pb):transformation(trans_pb){
	const pb::logTrans & lt_pb = trans_pb.lt();
	decade = lt_pb.decade();
	offset = lt_pb.offset();
}
void fasinhTrans::convertToPb(pb::transformation & trans_pb){
	transformation::convertToPb(trans_pb);
	trans_pb.set_trans_type(pb::PB_FASIGNH);
	pb::fasinhTrans * ft_pb = trans_pb.mutable_ft();
	ft_pb->set_a(A);
	ft_pb->set_length(length);
	ft_pb->set_m(M);
	ft_pb->set_maxrange(maxRange);
	ft_pb->set_t(T);
}
fasinhTrans::fasinhTrans(const pb::transformation & trans_pb):transformation(trans_pb){
	const pb::fasinhTrans & ft_pb = trans_pb.ft();
	length = ft_pb.length();
	maxRange = ft_pb.maxrange();
	T = ft_pb.t();
	A = ft_pb.a();
	M = ft_pb.m();
}
void linTrans::convertToPb(pb::transformation & trans_pb){
	transformation::convertToPb(trans_pb);
	trans_pb.set_trans_type(pb::PB_LIN);

}
linTrans::linTrans(const pb::transformation & trans_pb):transformation(trans_pb){}

//void scaleTrans::convertToPb(pb::transformation & trans_pb){
//	transformation::convertToPb(trans_pb);
//	trans_pb.set_trans_type(pb::PB_SCALE);
//	pb::scaleTrans * st_pb = trans_pb.mutable_st();
//	st_pb->set_scale_factor(scale_factor);
//}
//scaleTrans::scaleTrans(const pb::transformation & trans_pb):linTrans(trans_pb){
//	const pb::scaleTrans & st_pb = trans_pb.st();
//	scale_factor = st_pb.scale_factor();
//}


void flinTrans::convertToPb(pb::transformation & trans_pb){
	transformation::convertToPb(trans_pb);
	trans_pb.set_trans_type(pb::PB_FLIN);
	pb::flinTrans * ft_pb = trans_pb.mutable_flt();
	ft_pb->set_max(max);
	ft_pb->set_min(min);
}
flinTrans::flinTrans(const pb::transformation & trans_pb):transformation(trans_pb){
	const pb::flinTrans & ft_pb = trans_pb.flt();
	max = ft_pb.max();
	min = ft_pb.min();
}

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

fasinhTrans::fasinhTrans():transformation(false,FASINH),length(256),maxRange(262144), T(262144),A(0),M(4.5){
	calTbl.setInterpolated(true);
}

fasinhTrans::fasinhTrans(double _length, double _maxRange, double _T, double _A, double _M):transformation(false, FASINH),length(_length),maxRange(_maxRange), T(_T),A(_A),M(_M){
	calTbl.setInterpolated(true);
}

linTrans::linTrans():transformation(true,LIN){
        calTbl.setInterpolated(true);
}

scaleTrans::scaleTrans():linTrans(),scale_factor(1024){}
scaleTrans::scaleTrans(float _scale_factor):linTrans(),scale_factor(_scale_factor){}

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


/*
 * implementation copied from flowCore
 */
void fasinhTrans::transforming(valarray<double> & input){


	for(unsigned i=0;i<input.size();i++){
		input[i] = ( asinh(input[i] * sinh(M * log(10)) / T) + A * log(10)) / ((M + A) * log(10));
	}
//		double myB = (M + A) * log(10);
//		double myC = A * log(10);
//		double myA = T / sinh(myB - myC);
//		input = input / myA;
//
//		// This formula for the arcsinh loses significance when x is negative
//		//Therefore we take advantage of the fact that sinh is an odd function
//		input = abs(input);
//
//		input = log(input + sqrt(input * input + 1));
//		result = rep(NA, times=length(asinhx))
//		result[negative] = (myC - asinhx[negative]) / myB
//		result[!negative] = (asinhx[!negative] + myC) / myB
//		result








}

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

void scaleTrans::transforming(valarray<double> & input){

		input*=scale_factor;
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
				if(g_loglevel>=POPULATION_LEVEL)
					COUT<<"computing calibration table..."<<endl;
				computCalTbl();
			}

			if(!isInterpolated())
			{
				if(g_loglevel>=POPULATION_LEVEL)
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

biexpTrans::biexpTrans():transformation(false, BIEXP),channelRange(4096), pos(4.5), neg(0), widthBasis(-10),maxValue(262144){
	setComputeFlag(false);
	calTbl.setInterpolated(false);
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

