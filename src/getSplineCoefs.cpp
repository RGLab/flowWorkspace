#include "include/transformation.hpp"
#include <Rcpp.h>
//[[Rcpp::plugins(temp)]]
Rcpp::List getSplineCoefs(int channelRange=4096, double maxValue=262144, double pos = 4.5, double neg = 0, double widthBasis = -10);

//[[Rcpp::export]]
Rcpp::List getSplineCoefs(int channelRange, double maxValue, double pos, double neg, double widthBasis){

	biexpTrans curTran;
	curTran.channelRange = channelRange;
	curTran.maxValue = maxValue;
	curTran.pos = pos;
	curTran.neg = neg;
	curTran.widthBasis = widthBasis;


	curTran.computCalTbl();
	calibrationTable cal = curTran.getCalTbl();

	cal.interpolate();
	Spline_Coefs obj=cal.getSplineCoefs();

	return Rcpp::List::create(Named("z",obj.coefs)
								, Named("method",obj.method)
								, Named("type",obj.type)
								);


}
