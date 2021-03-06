/*
 *
 *
 * some C++ routines to be invoked by R for the faster R APIs
 *
 *  Created on: Aug 18, 2014
 *      Author: wjiang2
 */
#include "cytolib/GatingSet.hpp"
#include <Rcpp.h>
using namespace Rcpp;
using namespace cytolib;

#define ARRAY_TYPE vector<double>
//[[Rcpp::export]]
string gen_uid()
{
	return generate_uid();
}
//' construct the biexpTrans c++ object on the fly
//'
//' It returns the spline coefficients vectors to R.
//'
//' It is used to extract the spline coefficient vectors from the calibration table
//' which is computed by biexpTrans class and then return to R for constructing flowJo transformation function within R.
//' Mainly used for openCyto autoGating process where no xml workspace is needed to create flowJo transformation.
//' @noRd
//[[Rcpp::export(".getSplineCoefs")]]
Rcpp::List getSplineCoefs(int channelRange=4096, double maxValue=262144, double pos = 4.5, double neg = 0, double widthBasis = -10, bool inverse = false){

	biexpTrans curTran;
	curTran.channelRange = channelRange;
	curTran.maxValue = maxValue;
	curTran.pos = pos;
	curTran.neg = neg;
	curTran.widthBasis = widthBasis;


	curTran.computCalTbl();
	calibrationTable cal = curTran.getCalTbl();

	if(inverse)
	{
		ARRAY_TYPE tmp = cal.getX();
		cal.setX(cal.getY());
		cal.setY(tmp);
	}
	cal.interpolate();
	Spline_Coefs obj=cal.getSplineCoefs();

	return Rcpp::List::create(Named("z",obj.coefs)
								, Named("method",obj.method)
								, Named("type", "biexp")
								, Named("channelRange", channelRange)
								, Named("maxValue", maxValue)
								, Named("neg", neg)
								, Named("pos", pos)
								, Named("widthBasis", widthBasis)
								);


}

//' store the transformation functions created from R into GatingSet
//'
//' @param gsPtr external pointer that points to the C data structure of GatingSet
//' @param transformList a transformList that constains a list of transformation functions.
//'         Each of these functions carries the attributes to be used to convert to c++ transformation
//' @noRd
//[[Rcpp::export(".addTrans")]]
void addTrans(Rcpp::XPtr<GatingSet> gsPtr, Rcpp::S4 transformList){

	trans_map tm;
	/*
	 * parse the transformList
	 */
	Rcpp::List funs = transformList.slot("transforms");
	for(Rcpp::List::iterator it = funs.begin(); it != funs.end(); it++){
		Rcpp::S4 transMp = *it;
		std::string ch = transMp.slot("input");
		Rcpp::Function transFunc = transMp.slot("f");

		Rcpp::RObject type = transFunc.attr("type");
		if(type.isNULL())
			Rcpp::stop("transformation function must have 'type' attribute!");
		else{
			std::string trans_type = Rcpp::as<std::string>(type.get__());
			if(trans_type == "biexp")
			{
				Rcpp::List param = transFunc.attr("parameters");
				/*
				 * create biexpTrans based on the parameters stored as function attribute
				 */
				shared_ptr<biexpTrans> trans(new biexpTrans());
				trans->channelRange = Rcpp::as<int>(param["channelRange"]);
				trans->maxValue = Rcpp::as<int>(param["maxValue"]);
				trans->neg = Rcpp::as<double>(param["neg"]);
				trans->pos = Rcpp::as<double>(param["pos"]);
				trans->widthBasis = Rcpp::as<double>(param["widthBasis"]);
				//compute the calibration table
				trans->computCalTbl();
				trans->interpolate();

				//push into the trans map
				tm[ch] = trans;

			}else
				Rcpp::stop("add the unsupported transformation function!" + trans_type);
		}
	}

	/*
	 * propagate to each sample
	 */
	StringVec sn = gsPtr->get_sample_uids();
	for(StringVec::iterator it = sn.begin(); it != sn.end(); it++){
		GatingHierarchyPtr gh = gsPtr->getGatingHierarchy(*it);
		gh->addTransMap(tm);
	}

}


//' Update the channel information of a GatingSet (c++ part)
//' 
//' It updates the channels stored in gates,compensations and transformations
//' based on given mapping between the old and new channel names.
//' 
//' @param gs a GatingSet
//' @param sampleNames the sample names specifies samples to be operated on
//' @param map \code{data.frame} contains the mapping from old to new channel names
//'                             Note: Make sure to remove the '<' or '>' characters from 'old` name because the API tries 
//'                                   to only look at the raw channel name so that the gates with both prefixed and non-prefixed names could be updated. 
//'                                   
//' @examples 
//' \dontrun{
//'  updateChannels(gs, map = data.frame(old = c("Qdot 655-A")  ##this will update both "Qdot 655-A" and "<Qdot 655-A>"
//'                                          , new = c("<QDot 655-A>")
//'                                          )
//'                        , nodes = "14-")  
//'}
//' @noRd
//[[Rcpp::export(.updateChannels)]]
void updateChannels(Rcpp::S4 gs, Rcpp::DataFrame map){

	Rcpp::XPtr<GatingSet> gsPtr = gs.slot("pointer");
	//convert dataframe to map
	CHANNEL_MAP stdmap;
	std::vector<std::string> oldN =  map["old"];
	std::vector<std::string> newN =  map["new"];
	for(unsigned i = 0; i < oldN.size(); i++){
		stdmap[oldN.at(i)] = newN.at(i);
	}
	gsPtr->set_channels(stdmap);

}
