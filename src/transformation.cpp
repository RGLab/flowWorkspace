/*
 * transformation.cpp
 *
 *  Created on: Apr 24, 2012
 *      Author: wjiang2
 */

#include "include/transformation.hpp"
bool compare_x(coordinate i, coordinate j) { return i.x<j.x; }
bool compare_y(coordinate i, coordinate j) { return i.y<j.y; }
/**
 *
 * @param pVec
 * @param name  channel name (possibly prefixed by comp.prefix)
 * @param comp
 * @return
 */
PARAM_VEC::const_iterator findTransFlag(const PARAM_VEC & pVec, const string & name, const string & prefix, const string & suffix){
	PARAM_VEC::const_iterator it;
	for(it=pVec.begin();it!=pVec.end();it++)
	{
		//	try both the bare and prefixed chnl ma,e
		string chnl = it->param;
		string chnl_comp = prefix + chnl + suffix;//append prefix
		if(chnl.compare(name)==0||chnl_comp.compare(name)==0)
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

void trans_local::updateChannels(const CHANNEL_MAP & chnl_map){

	//iterate throiugh chnl_map instead of tp since tp iterator will be invalidated when erased
	for(CHANNEL_MAP::const_iterator itChnl = chnl_map.begin(); itChnl != chnl_map.end(); itChnl++)
	{

		string oldN = itChnl->first;
		trans_map::iterator itTp = tp.find(oldN);

		if(itTp!=tp.end())
		{
			string newN = itChnl->second;
			if(g_loglevel>=GATING_SET_LEVEL)
				COUT<<"update transformation: "<< oldN << "-->" << newN <<endl;

			transformation * curTran = itTp->second;
			curTran->setChannel(newN);
			/*
			 *
			 * have to delete the old one before adding newN
			 * because tp[newN] could be just updating existing tp[oldN]
			 * instead of inserting new entry due to the fact tp trans_map is set to case insensitive key searching
			 * otherwise, tp.erase would lead to losing the entry when oldN and newN are equivalent
			 */
			tp.erase(itTp);
			tp[newN] = curTran; //add new entry

		}

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
	/* For PB_BIEXP, caltbl was not saved during archiving and thus could be 0x0
	  and thus trans_pb.caltbl() call is supposed to fall back to return the one from default_instance, which should not be null.
	  However strange enough, in some circumstances (e.g. after save_gs() call), this default_instance_->caltbl_ does become null
	  which leads this pb auto generated accessor function to be unsafe to be invoked.
	  So we have to skip it in case of biexp. (we don't need to do it anyway)
	*/
	if(trans_pb.trans_type() != pb::PB_BIEXP)
		calTbl = calibrationTable(trans_pb.caltbl());
}
void transformation::convertToPb(pb::transformation & trans_pb){

	trans_pb.set_isgateonly(isGateOnly);
	trans_pb.set_type(type);
	trans_pb.set_name(name);
	trans_pb.set_channel(channel);
	/*skip saving calibration table to save disk space, which means it needs to be always recalculated when load it back
	 	 Setting the flag to FALSE can only make sure it is recomputed properly for the APIs where the flag is checked first
		but it is not sufficient to prevent the segfault because the pointer to the caltbl in pb object is unset and somehow the
		default_instance_->caltbl_ can also be null due to some previous operations (i.e.`save_gs`). So ::pb::calibrationTable& transformation::caltbl
		becomes unsafe to call.
	*/
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

	//make sure to always recompute caltbl (regardless of compute flag) since it was not saved for the sake of space
	computCalTbl();
}
void logTrans::convertToPb(pb::transformation & trans_pb){
	transformation::convertToPb(trans_pb);
	trans_pb.set_trans_type(pb::PB_LOG);
	pb::logTrans * lt_pb = trans_pb.mutable_lt();
	lt_pb->set_decade(decade);
	lt_pb->set_offset(offset);
	lt_pb->set_t(T);
}
logTrans::logTrans(const pb::transformation & trans_pb):transformation(trans_pb){
	const pb::logTrans & lt_pb = trans_pb.lt();
	decade = lt_pb.decade();
	offset = lt_pb.offset();
	T = lt_pb.t();
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
logTrans::logTrans():transformation(false,LOG),offset(0),decade(1),T(262144), scale(1){
	calTbl.setInterpolated(true);
}


logTrans::logTrans(double _offset,double _decade, unsigned _T, unsigned _scale):transformation(false,LOG),offset(_offset),decade(_decade),T(_T), scale(_scale){
	calTbl.setInterpolated(true);
}
fasinhTrans::fasinhTrans():transformation(false,FASINH),length(256),maxRange(262144), T(262144),A(0),M(4.5){
	calTbl.setInterpolated(true);
}

fasinhTrans::fasinhTrans(double _length, double _maxRange, double _T, double _A, double _M):transformation(false, FASINH),length(_length),maxRange(_maxRange), T(_T),A(_A),M(_M){
	calTbl.setInterpolated(true);
}
boost::shared_ptr<transformation>  fasinhTrans::getInverseTransformation(){
	return boost::shared_ptr<transformation>(new fsinhTrans(length, maxRange, T, A , M));
}
boost::shared_ptr<transformation>  logTrans::getInverseTransformation(){
	return boost::shared_ptr<transformation>(new logInverseTrans(offset, decade, T,scale));
}

fsinhTrans::fsinhTrans():fasinhTrans(){}

fsinhTrans::fsinhTrans(double _length, double _maxRange, double _T, double _A, double _M):fasinhTrans(_length,_maxRange, _T, _A, _M){}

void fsinhTrans:: transforming(valarray<double> & input){
	for(unsigned i=0;i<input.size();i++)
		input[i] = sinh(((M + A) * log(10)) * input[i]/length - A * log(10)) * T / sinh(M * log(10));

}
linTrans::linTrans():transformation(true,LIN){
        calTbl.setInterpolated(true);
}

scaleTrans::scaleTrans():linTrans(),t_scale(256), r_scale(262144){}
scaleTrans::scaleTrans(int _t_scale, int _r_scale):linTrans(),t_scale(_t_scale), r_scale(_r_scale){}

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
		input[i] = length * (asinh(input[i] * sinh(M * log(10)) / T) + A * log(10)) / ((M + A) * log(10));
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

/*
 *
 */
boost::shared_ptr<transformation>  transformation::getInverseTransformation(){
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

	//clone the existing trans
	boost::shared_ptr<transformation>  inverse = boost::shared_ptr<transformation>(new transformation(*this));
	//make sure to reset type to avoid type-discrepancy because
	//it returns the base transformation type instead of the original one (e.g. biexp)
	inverse->type = CALTBL;

	//swap the x, y vectors in calTbl

	inverse->calTbl.setX(this->calTbl.getY());
	inverse->calTbl.setY(this->calTbl.getX());

	//re-interpolate the inverse calibration tbl
	inverse->calTbl.setInterpolated(false);
	if(g_loglevel>=POPULATION_LEVEL)
			COUT<<"spline interpolating..."<<endl;
	inverse->interpolate();
	return inverse;
}

boost::shared_ptr<transformation> scaleTrans::getInverseTransformation(){
	return boost::shared_ptr<transformation>(new scaleTrans(r_scale, t_scale));//swap the raw and trans scale
}
void logTrans::transforming(valarray<double> & input){


//		double thisMax=input.max();//max val must be globally determined during xml parsing
		double thisMin=0;//input.min();

		for(unsigned i=0;i<input.size();i++){
			input[i]=flog(input[i],T,thisMin) * scale;
		}

}
void logInverseTrans::transforming(valarray<double> & input){


//		double thisMax=input.max();
		double thisMin=0;//input.min();

		for(unsigned i=0;i<input.size();i++){
			input[i]= pow(10, (input[i]/scale - 1) * decade) * T;
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

		input*=(t_scale/(double)r_scale);
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

	double maxChannlVal = channelRange + 1;
	unsigned int nPoints = maxChannlVal;//4097;//fix the number of points so that it won't lost the precision when scale is set to 256 (i.e. channelRange = 256)

	valarray<double> positive(nPoints), negative(nPoints), vals(nPoints);
	double step = (maxChannlVal-1)/(double)(nPoints -1);
	for (int j = 0; j < nPoints; j++)
	{
		vals[j] = j * step;
		positive[j] = exp((float)(j) / (float)(nPoints) * positiveRange);
		negative[j] = exp((float)(j) / (float)(nPoints) * (-negativeRange));
	}



	double s = exp((positiveRange + negativeRange) * (width + extra / decades));
	negative *= s;
	s = positive[zeroChan] - negative[zeroChan];
	for (int j = zeroChan; j < nPoints; j++)
		positive[j] = minimum * (positive[j] - negative[j] - s);
	for (int j = 0; j < zeroChan; j++)
		positive[j] = -positive[2 * zeroChan - j];

	/*
	 * save the calibration table
	 */
	calTbl.setCaltype("flowJo");
	calTbl.setMethod(2);
	calTbl.init(nPoints);

	calTbl.setX(positive);
	calTbl.setY(vals);

	isComputed=true;


}

