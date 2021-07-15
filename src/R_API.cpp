/*
 *
 *
 * some C++ routines to be invoked by R for the faster R APIs
 *
 *  Created on: Aug 18, 2014
 *      Author: wjiang2
 */
#include "cytolib/GatingSet.hpp"
#include <cpp11.hpp>

using namespace cytolib;

#define ARRAY_TYPE vector<double>
[[cpp11::register]]
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

[[cpp11::register]]
cpp11::list getSplineCoefs(int channelRange=4096, double maxValue=262144, double pos = 4.5, double neg = 0, double widthBasis = -10, bool inverse = false){

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
    cpp11::writable::list coef;
    for (auto it : obj.coefs)
    {
        coef.push_back(cpp11::named_arg(it.first.c_str()) = it.second);
    }
	return cpp11::list({cpp11::named_arg("z")=coef
								, cpp11::named_arg("method")=obj.method
								, cpp11::named_arg("type")= "biexp"
								, cpp11::named_arg("channelRange")= channelRange
								, cpp11::named_arg("maxValue")= maxValue
								, cpp11::named_arg("neg")= neg
								, cpp11::named_arg("pos")= pos
								, cpp11::named_arg("widthBasis")= widthBasis
    });


}

// //' store the transformation functions created from R into GatingSet
// //'
// //' @param gsPtr external pointer that points to the C data structure of GatingSet
// //' @param transformList a transformList that constains a list of transformation functions.
// //'         Each of these functions carries the attributes to be used to convert to c++ transformation
// //' @noRd
// //[[Rcpp::export(".addTrans")]]
// [[cpp11::register]]
// void addTrans(cpp11::external_pointer<GatingSet> gsPtr, Rcpp::S4 transformList){

// 	trans_map tm;
// 	/*
// 	 * parse the transformList
// 	 */
// 	cpp11::list funs = transformList.slot("transforms");
// 	for(cpp11::list::iterator it = funs.begin(); it != funs.end(); it++){
// 		Rcpp::S4 transMp = *it;
// 		std::string ch = transMp.slot("input");
// 		Rcpp::Function transFunc = transMp.slot("f");

// 		Rcpp::RObject type = transFunc.attr("type");
// 		if(type.isNULL())
// 			Rcpp::stop("transformation function must have 'type' attribute!");
// 		else{
// 			std::string trans_type = Rcpp::as<std::string>(type.get__());
// 			if(trans_type == "biexp")
// 			{
// 				cpp11::list param = transFunc.attr("parameters");
// 				/*
// 				 * create biexpTrans based on the parameters stored as function attribute
// 				 */
// 				shared_ptr<biexpTrans> trans(new biexpTrans());
// 				trans->channelRange = Rcpp::as<int>(param["channelRange"]);
// 				trans->maxValue = Rcpp::as<int>(param["maxValue"]);
// 				trans->neg = Rcpp::as<double>(param["neg"]);
// 				trans->pos = Rcpp::as<double>(param["pos"]);
// 				trans->widthBasis = Rcpp::as<double>(param["widthBasis"]);
// 				//compute the calibration table
// 				trans->computCalTbl();
// 				trans->interpolate();

// 				//push into the trans map
// 				tm[ch] = trans;

// 			}else
// 				Rcpp::stop("add the unsupported transformation function!" + trans_type);
// 		}
// 	}

// 	/*
// 	 * propagate to each sample
// 	 */
// 	StringVec sn = gsPtr->get_sample_uids();
// 	for(StringVec::iterator it = sn.begin(); it != sn.end(); it++){
// 		GatingHierarchyPtr gh = gsPtr->getGatingHierarchy(*it);
// 		gh->addTransMap(tm);
// 	}

// }


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
[[cpp11::register]]
void updateChannels_cpp(cpp11::external_pointer<GatingSet> gsPtr, cpp11::data_frame map){

	
	//convert dataframe to map
	CHANNEL_MAP stdmap;
	cpp11::strings oldN(map["old"]);
	cpp11::strings newN(map["new"]);
	for(int i = 0; i < oldN.size(); i++){
		stdmap[oldN.at(i)] = newN.at(i);
	}
	gsPtr->set_channels(stdmap);

}
